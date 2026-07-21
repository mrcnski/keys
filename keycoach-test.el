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
         (keycoach-random nil)
         (keycoach-error t)
         (keycoach--missed-key nil)
         (keycoach-post-change-hook nil)
         ,@config)
     (setq keycoach-keys-current (copy-sequence keycoach-keys))
     ,@body))

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

(provide 'keycoach-test)
;;; keycoach-test.el ends here
