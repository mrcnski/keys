;;; keys.el --- Learn keybindings  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Marcin Swieczkowski <marcin@realemail.net>
;;
;; Author: Marcin Swieczkowski <marcin@realemail.net>
;; URL: https://github.com/mrcnski/keys
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: help
;;
;;; Commentary:
;;
;; Provides keys-mode to help you remember and learn new keybindings.
;;
;;; Code:

(require 'seq)

(defcustom keys-keys '()
  "Keys to learn."
  :type '(repeat string)
  :group 'keys)

(defcustom keys-display-amount nil
  "If not NIL, only show this many keybindings in the indicator."
  :type 'integer
  :group 'keys)

(defcustom keys-indicator-separator " | "
  "Separator to use in the `keys-mode' indicator (see `keys-indicator')."
  :type 'string
  :group 'keys)

(defcustom keys-indicator-truncated "…"
  "If not NIL, show this string in the indicator when not all keys are displayed."
  :type 'string
  :group 'keys)

(defcustom keys-random t
  "If not NIL, shuffle the keys everytime the keys are reset.

This includes when `keys-reset' is called and when the mode is enabled."
  :type 'boolean
  :group 'keys)

(defcustom keys-post-change-hook nil
  "Hook run after keys have changed."
  :type 'hook
  :group 'keys)

(defvar keys-keys-current '())

;; TODO:
;;  - don't unset keys-current when disabling mode
;;  - only evaluate this if the mode is set
(defun keys-indicator ()
  "Generate the keys indicator string.

Can be used in the mode-line, frame title, or other \"mode line constructs\"."
  (concat
   (mapconcat 'identity
              (if keys-display-amount
                  (seq-take keys-keys-current keys-display-amount)
                keys-keys-current)
              keys-indicator-separator)
   (when keys-indicator-truncated
     (concat keys-indicator-separator keys-indicator-truncated))))

(defun keys-reset ()
  "Reset the state of `keys-mode', including the mode-line indicator.

You can e.g. integrate this with `midnight-mode'."
  (interactive)
  (setq keys-keys-current keys-keys)
  (when keys-random
    (keys--shuffle-list keys-keys-current)))

;;; Internal

;; From: https://gist.github.com/purcell/34824f1b676e6188540cdf71c7cc9fc4
(defun keys--shuffle-list (list)
  "Shuffles LIST randomly, modying it in-place."
  (dolist (i (reverse (number-sequence 1 (1- (length list)))))
    (let ((j (random (1+ i)))
	  (tmp (elt list i)))
      (setf (elt list i) (elt list j))
      (setf (elt list j) tmp)))
  list)

(defun keys--pre-command ()
  "Check if the command matches one of the keys we are trying to learn."
  (let ((key (key-description (this-single-command-keys))))
    (when (member key keys-keys-current)
      (setq keys-keys-current (delete key keys-keys-current))
      (run-hooks 'keys-post-change-hook))))

(defun keys--enable ()
  "Initialize `keys-mode'."
  (add-hook 'pre-command-hook 'keys--pre-command)
  (keys-reset)
  (run-hooks 'keys-post-change-hook))

(defun keys--disable ()
  "Cleanup `keys-mode'."
  (remove-hook 'pre-command-hook 'keys--pre-command)
  (run-hooks 'keys-post-change-hook))

;;; Autoloads

;;;###autoload
(define-minor-mode keys-mode
  "Toggle `keys-mode'.

Starts listening for the keys defined in `keys-keys', so that
must have been set before calling `keys-mode'."
  :global t
  :init-value nil
  :lighter nil
  :keymap nil
  :group 'keys

  (if keys-mode
      (keys--enable)
    (keys--disable)))

(provide 'keys)
;;; keys.el ends here
