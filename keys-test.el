;;; keys-test.el --- Tests for keys.el  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Run with:
;;
;;   emacs -Q --batch -L . -l keys-test.el -f ert-run-tests-batch-and-exit
;;
;;; Code:

(require 'ert)
(require 'keys)

(defmacro keys-test--with-config (config &rest body)
  "Run BODY with keys state bound to defaults, overridden by CONFIG."
  (declare (indent 1))
  `(let ((keys-keys '("s-w" "M-F" "C-M-y"))
         (keys-keys-current nil)
         (keys-display-amount nil)
         (keys-indicator-separator " | ")
         (keys-indicator-truncated "…")
         (keys-random nil)
         (keys-error t)
         (keys--missed-key nil)
         (keys-post-change-hook nil)
         ,@config)
     (setq keys-keys-current (copy-sequence keys-keys))
     ,@body))

(ert-deftest keys-test-indicator-default-shows-all ()
  "Indicator must not error when `keys-display-amount' is nil (the default)."
  (keys-test--with-config ()
    (should (equal (keys-indicator) "s-w | M-F | C-M-y"))))

(ert-deftest keys-test-indicator-truncates ()
  (keys-test--with-config ((keys-display-amount 2))
    (should (equal (keys-indicator) "s-w | M-F | …"))))

(ert-deftest keys-test-indicator-no-ellipsis-when-all-shown ()
  (keys-test--with-config ((keys-display-amount 5))
    (should (equal (keys-indicator) "s-w | M-F | C-M-y"))))

(ert-deftest keys-test-indicator-truncation-disabled ()
  (keys-test--with-config ((keys-display-amount 1)
                           (keys-indicator-truncated nil))
    (should (equal (keys-indicator) "s-w"))))

(ert-deftest keys-test-indicator-empty ()
  (keys-test--with-config ((keys-keys '()))
    (should (equal (keys-indicator) ""))))

(ert-deftest keys-test-indicator-missed-key ()
  (keys-test--with-config ((keys--missed-key "s-w"))
    (should (string-match-p "ERROR: missed" (keys-indicator)))))

(ert-deftest keys-test-reset-restores-keys ()
  (keys-test--with-config ()
    (setq keys-keys-current '())
    (keys-reset)
    (should (equal keys-keys-current keys-keys))
    ;; Must be a fresh copy: typing keys must not mutate `keys-keys'.
    (should-not (eq keys-keys-current keys-keys))))

(ert-deftest keys-test-disable-clears-missed-key ()
  (keys-test--with-config ((keys--missed-key "s-w"))
    (keys--disable)
    (should-not keys--missed-key)))

(defun keys-test--dummy-command ()
  "Command bound to a learned key in the miss-detection tests."
  (interactive))

(defmacro keys-test--with-binding (key command &rest body)
  "Run BODY with KEY globally bound to COMMAND, then unbind."
  (declare (indent 2))
  `(unwind-protect
       (progn
         (global-set-key (kbd ,key) ,command)
         ,@body)
     (global-set-key (kbd ,key) nil)))

;; In batch mode `this-single-command-keys' is empty, so `keys--post-command'
;; always takes the "invoked some other way" branch — exactly the miss path.

(ert-deftest keys-test-miss-detected-via-live-lookup ()
  "Invoking a learned command without its key flags that key."
  (keys-test--with-config ((keys-keys '("C-c C-t k")))
    (keys-test--with-binding "C-c C-t k" #'keys-test--dummy-command
      (let ((real-this-command #'keys-test--dummy-command)
            (ring-bell-function #'ignore))
        (keys--post-command))
      (should (equal keys--missed-key "C-c C-t k")))))

(ert-deftest keys-test-miss-respects-rebinding ()
  "A key rebound after reset must no longer flag its old command."
  (keys-test--with-config ((keys-keys '("C-c C-t k")))
    (keys-test--with-binding "C-c C-t k" #'ignore
      ;; The learned key is now bound to `ignore', not the dummy command;
      ;; invoking the dummy command is not a miss.
      (let ((real-this-command #'keys-test--dummy-command)
            (ring-bell-function #'ignore))
        (keys--post-command))
      (should-not keys--missed-key))))

(ert-deftest keys-test-no-miss-for-unrelated-command ()
  (keys-test--with-config ()
    (let ((real-this-command #'keys-test--dummy-command)
          (ring-bell-function #'ignore))
      (keys--post-command))
    (should-not keys--missed-key)))

(provide 'keys-test)
;;; keys-test.el ends here
