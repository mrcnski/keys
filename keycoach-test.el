;;; keycoach-test.el --- Tests for keycoach.el  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Run with:
;;
;;   emacs -Q --batch -L . -l keycoach-test.el -f ert-run-tests-batch-and-exit
;;
;;; Code:

(require 'ert)
(require 'keycoach)

(defmacro keycoach-test--with-config (config &rest body)
  "Run BODY with keys state bound to defaults, overridden by CONFIG."
  (declare (indent 1))
  `(let ((keycoach-keys '("s-w" "M-F" "C-M-y"))
         (keycoach-keys-current nil)
         (keycoach-display-amount nil)
         (keycoach-indicator-separator " | ")
         (keycoach-indicator-truncated "…")
         (keycoach-indicator-format "%s")
         (keycoach-indicator-string "")
         (keycoach-random nil)
         (keycoach-error t)
         (keycoach--missed-key nil)
         (keycoach-post-change-hook nil)
         ;; `keycoach--changed' only fills the indicator while the mode is on.
         (global-keycoach-mode t)
         ,@config)
     (setq keycoach-keys-current (copy-sequence keycoach-keys))
     ,@body))

(defmacro keycoach-test--saving-formats (&rest body)
  "Run BODY, restoring the globals the indicator installs itself into.

`header-line-format' is always buffer-local, so it can't be let-bound
like the others and is saved and restored by hand."
  (declare (indent 0))
  `(let ((global-mode-string global-mode-string)
         (frame-title-format frame-title-format)
         (keycoach-indicator-target nil)
         (keycoach-indicator-string "")
         (keycoach--installed-target nil)
         (keycoach--saved-format nil)
         (saved-header-line (default-value 'header-line-format)))
     (unwind-protect
         (progn ,@body)
       (setq-default header-line-format saved-header-line))))

(ert-deftest keycoach-test-indicator-default-shows-all ()
  "Indicator must not error when `keycoach-display-amount' is nil (the default)."
  (keycoach-test--with-config ()
    (should (equal (keycoach-indicator) "s-w | M-F | C-M-y"))))

(ert-deftest keycoach-test-indicator-truncates ()
  (keycoach-test--with-config ((keycoach-display-amount 2))
    (should (equal (keycoach-indicator) "s-w | M-F | …"))))

(ert-deftest keycoach-test-indicator-no-ellipsis-when-all-shown ()
  (keycoach-test--with-config ((keycoach-display-amount 5))
    (should (equal (keycoach-indicator) "s-w | M-F | C-M-y"))))

(ert-deftest keycoach-test-indicator-truncation-disabled ()
  (keycoach-test--with-config ((keycoach-display-amount 1)
                           (keycoach-indicator-truncated nil))
    (should (equal (keycoach-indicator) "s-w"))))

(ert-deftest keycoach-test-indicator-empty ()
  (keycoach-test--with-config ((keycoach-keys '()))
    (should (equal (keycoach-indicator) ""))))

(ert-deftest keycoach-test-indicator-missed-key ()
  (keycoach-test--with-config ((keycoach--missed-key "s-w"))
    (should (string-match-p "ERROR: missed" (keycoach-indicator)))))

(ert-deftest keycoach-test-reset-restores-keys ()
  (keycoach-test--with-config ()
    (setq keycoach-keys-current '())
    (keycoach-reset)
    (should (equal keycoach-keys-current keycoach-keys))
    ;; Must be a fresh copy: typing keys must not mutate `keycoach-keys'.
    (should-not (eq keycoach-keys-current keycoach-keys))))

(ert-deftest keycoach-test-disable-clears-missed-key ()
  (keycoach-test--with-config ((keycoach--missed-key "s-w"))
    (keycoach--disable)
    (should-not keycoach--missed-key)))

(defun keycoach-test--dummy-command ()
  "Command bound to a learned key in the miss-detection tests."
  (interactive))

(defmacro keycoach-test--with-binding (key command &rest body)
  "Run BODY with KEY globally bound to COMMAND, then unbind."
  (declare (indent 2))
  `(unwind-protect
       (progn
         (global-set-key (kbd ,key) ,command)
         ,@body)
     (global-set-key (kbd ,key) nil)))

;; In batch mode `this-single-command-keys' is empty, so `keycoach--post-command'
;; always takes the "invoked some other way" branch — exactly the miss path.

(ert-deftest keycoach-test-miss-detected-via-live-lookup ()
  "Invoking a learned command without its key flags that key."
  (keycoach-test--with-config ((keycoach-keys '("C-c C-t k")))
    (keycoach-test--with-binding "C-c C-t k" #'keycoach-test--dummy-command
      (let ((real-this-command #'keycoach-test--dummy-command)
            (ring-bell-function #'ignore))
        (keycoach--post-command))
      (should (equal keycoach--missed-key "C-c C-t k")))))

(ert-deftest keycoach-test-miss-respects-rebinding ()
  "A key rebound after reset must no longer flag its old command."
  (keycoach-test--with-config ((keycoach-keys '("C-c C-t k")))
    (keycoach-test--with-binding "C-c C-t k" #'ignore
      ;; The learned key is now bound to `ignore', not the dummy command;
      ;; invoking the dummy command is not a miss.
      (let ((real-this-command #'keycoach-test--dummy-command)
            (ring-bell-function #'ignore))
        (keycoach--post-command))
      (should-not keycoach--missed-key))))

(ert-deftest keycoach-test-no-miss-for-unrelated-command ()
  (keycoach-test--with-config ()
    (let ((real-this-command #'keycoach-test--dummy-command)
          (ring-bell-function #'ignore))
      (keycoach--post-command))
    (should-not keycoach--missed-key)))

;;; Indicator placement

(ert-deftest keycoach-test-indicator-string-uses-format ()
  (keycoach-test--with-config ((keycoach-indicator-format "  —  %s"))
    (keycoach--changed)
    (should (equal keycoach-indicator-string "  —  s-w | M-F | C-M-y"))))

(ert-deftest keycoach-test-indicator-string-empty-stays-empty ()
  "An empty indicator must not pick up padding from the format string.

Otherwise a separator is left stranded once every key has been used."
  (keycoach-test--with-config ((keycoach-keys '())
                               (keycoach-indicator-format "  —  %s"))
    (keycoach--changed)
    (should (equal keycoach-indicator-string ""))))

(ert-deftest keycoach-test-disable-clears-indicator-string ()
  (keycoach-test--with-config ()
    (keycoach--changed)
    (should-not (equal keycoach-indicator-string ""))
    ;; `define-minor-mode' clears the variable before running the body.
    (setq global-keycoach-mode nil)
    (keycoach--disable)
    (should (equal keycoach-indicator-string ""))))

(ert-deftest keycoach-test-reset-while-off-shows-nothing ()
  "`keycoach-reset' on a timer must not put keys back on screen.

`midnight-hook' fires whether or not the mode is on, and the indicator
may be placed by hand, where nothing else would hide it."
  (keycoach-test--with-config ((global-keycoach-mode nil))
    (keycoach-reset)
    (should (equal keycoach-indicator-string ""))))

(ert-deftest keycoach-test-wrap-format-never-starts-with-nil ()
  "A construct whose car is nil is read as a conditional, not as a list.

Checked structurally because `format-mode-line' returns \"\" for
everything in batch mode.  Rendered in a terminal frame, (nil VAR)
produces \"\" while (\"\" VAR) produces the value of VAR."
  (should (equal (keycoach--wrap-format nil)
                 '("" keycoach-indicator-string)))
  (should (equal (keycoach--wrap-format "Emacs")
                 '("Emacs" keycoach-indicator-string))))

(ert-deftest keycoach-test-mode-line-preserves-other-packages ()
  (keycoach-test--saving-formats
    (setq global-mode-string '("" display-time-string))
    (let ((keycoach-indicator-target 'mode-line))
      (keycoach--install-indicator))
    (should (equal global-mode-string
                   '("" display-time-string keycoach-indicator-string)))
    (keycoach--uninstall-indicator)
    (should (equal global-mode-string '("" display-time-string)))))

(ert-deftest keycoach-test-mode-line-from-empty ()
  "Installing into an empty `global-mode-string' must not build a conditional."
  (keycoach-test--saving-formats
    (setq global-mode-string nil)
    (let ((keycoach-indicator-target 'mode-line))
      (keycoach--install-indicator))
    (should (equal global-mode-string '("" keycoach-indicator-string)))))

(ert-deftest keycoach-test-mode-line-install-is-idempotent ()
  "Re-enabling the mode must not stack up copies of the indicator."
  (keycoach-test--saving-formats
    (setq global-mode-string nil)
    (let ((keycoach-indicator-target 'mode-line))
      (keycoach--install-indicator)
      (keycoach--install-indicator))
    (should (equal global-mode-string '("" keycoach-indicator-string)))))

(ert-deftest keycoach-test-mode-line-tolerates-a-bare-string ()
  "`global-mode-string' is a construct, so it may be a string, not a list."
  (keycoach-test--saving-formats
    (setq global-mode-string "battery")
    (let ((keycoach-indicator-target 'mode-line))
      (keycoach--install-indicator))
    (should (equal global-mode-string '("battery" keycoach-indicator-string)))))

(ert-deftest keycoach-test-frame-title-install-is-idempotent ()
  "Enabling an already-enabled mode must not wrap the format twice.

Nesting compounds per enable, and teardown then restores the wrapped
copy rather than the original."
  (keycoach-test--saving-formats
    (setq frame-title-format "Emacs")
    (let ((keycoach-indicator-target 'frame-title))
      (keycoach--install-indicator)
      (keycoach--install-indicator)
      (keycoach--install-indicator))
    (should (equal frame-title-format '("Emacs" keycoach-indicator-string)))
    (keycoach--uninstall-indicator)
    (should (equal frame-title-format "Emacs"))))

(ert-deftest keycoach-test-header-line-install-is-idempotent ()
  (keycoach-test--saving-formats
    (setq-default header-line-format nil)
    (let ((keycoach-indicator-target 'header-line))
      (keycoach--install-indicator)
      (keycoach--install-indicator))
    (should (equal (default-value 'header-line-format)
                   '("" keycoach-indicator-string)))
    (keycoach--uninstall-indicator)
    (should-not (default-value 'header-line-format))))

(ert-deftest keycoach-test-frame-title-round-trip ()
  (keycoach-test--saving-formats
    (setq frame-title-format "Emacs")
    (let ((keycoach-indicator-target 'frame-title))
      (keycoach--install-indicator))
    (should (equal frame-title-format '("Emacs" keycoach-indicator-string)))
    (keycoach--uninstall-indicator)
    (should (equal frame-title-format "Emacs"))))

(ert-deftest keycoach-test-frame-title-keeps-later-changes ()
  "A format set while the mode was on must survive turning the mode off."
  (keycoach-test--saving-formats
    (setq frame-title-format "Emacs")
    (let ((keycoach-indicator-target 'frame-title))
      (keycoach--install-indicator))
    (setq frame-title-format "set by the user later")
    (keycoach--uninstall-indicator)
    (should (equal frame-title-format "set by the user later"))))

(ert-deftest keycoach-test-header-line-round-trip ()
  (keycoach-test--saving-formats
    (setq-default header-line-format nil)
    (let ((keycoach-indicator-target 'header-line))
      (keycoach--install-indicator))
    (should (equal (default-value 'header-line-format)
                   '("" keycoach-indicator-string)))
    (keycoach--uninstall-indicator)
    (should-not (default-value 'header-line-format))))

(ert-deftest keycoach-test-uninstall-follows-installed-target ()
  "Retargeting must clean up where the indicator actually went."
  (keycoach-test--saving-formats
    (setq global-mode-string nil)
    (let ((keycoach-indicator-target 'mode-line))
      (keycoach--install-indicator))
    (let ((keycoach-indicator-target 'frame-title))
      (keycoach--uninstall-indicator))
    (should (equal global-mode-string '("")))))

(ert-deftest keycoach-test-mode-toggle-leaves-no-trace ()
  "Turning the mode on twice and off must leave the format as it was.

Exercises the real entry points, since it's `global-keycoach-mode'
being called again that installs a second time."
  (keycoach-test--saving-formats
    (let ((keycoach-keys '("s-w"))
          (keycoach-keys-current nil)
          (keycoach-indicator-target 'frame-title))
      (setq frame-title-format "Emacs")
      (unwind-protect
          (progn
            (global-keycoach-mode 1)
            (global-keycoach-mode 1)
            (should (equal frame-title-format
                           '("Emacs" keycoach-indicator-string)))
            (should (equal keycoach-indicator-string "s-w")))
        (global-keycoach-mode -1)))
    (should (equal frame-title-format "Emacs"))
    (should (equal keycoach-indicator-string ""))))

(ert-deftest keycoach-test-retarget-while-on-moves-the-indicator ()
  "Customizing the target with the mode on moves it, leaving nothing behind.

Covers the `:set' function, the only way the target can change without
the mode being toggled."
  (keycoach-test--saving-formats
    (let ((keycoach-keys '("s-w"))
          (keycoach-keys-current nil))
      (setq frame-title-format "Emacs"
            global-mode-string nil)
      (customize-set-variable 'keycoach-indicator-target 'frame-title)
      (unwind-protect
          (progn
            (global-keycoach-mode 1)
            (should (equal frame-title-format
                           '("Emacs" keycoach-indicator-string)))
            (customize-set-variable 'keycoach-indicator-target 'mode-line)
            (should (equal frame-title-format "Emacs"))
            (should (equal global-mode-string
                           '("" keycoach-indicator-string))))
        (global-keycoach-mode -1)))))

(ert-deftest keycoach-test-no-target-installs-nothing ()
  (keycoach-test--saving-formats
    (setq global-mode-string nil
          frame-title-format "Emacs")
    (let ((keycoach-indicator-target nil))
      (keycoach--install-indicator))
    (should-not global-mode-string)
    (should (equal frame-title-format "Emacs"))))

(provide 'keycoach-test)
;;; keycoach-test.el ends here
