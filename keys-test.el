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

(provide 'keys-test)
;;; keys-test.el ends here
