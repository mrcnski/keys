;;; keys.el --- Learn keybindings  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Marcin Swieczkowski <marcin@realemail.net>
;;
;; Author: Marcin Swieczkowski <marcin@realemail.net>
;; URL: https://github.com/mrcnski/keys
;; Version: 0.2.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: help
;;
;;; Commentary:
;;
;; Provides `global-keys-mode' to help you remember and learn new keybindings!
;;
;; Features:
;;
;; keys is a simple, unopinionated package with two main features:
;;
;; - A configurable display to remind you about keybindings you want to learn.
;;   Keys are removed from the display (called the *indicator*) when they're
;;   typed.
;;
;; - Error messages if you fail to use one of your keys to invoke a command
;;   (i.e. `M-x`).
;;
;; For example, you can try to use all your keys every day, and set
;; `midnight-mode` to reset them for the next day. See below!
;;
;; Get started:
;;
;; For a totally basic setup, this turns on `global-keys-mode` and sticks some
;; keys in your frame title:
;;
;; ```el
;; (use-package keys
;;   :load-path "~/.emacs.d/packages/keys" ; Coming to MELPA soon I hope
;;
;;   :config
;;
;;   (setq keys-keys '("s-w" "M-F" "C-M-y"))
;;
;;   ;; Update the indicator every time it should change.
;;   ;; You can also just do `(:eval (when global-keys-mode (keys-indicator)))`,
;;   ;;   but this avoids constantly re-calculating the indicator.
;;   ;; The same idea applies for the mode-line, header, etc.
;;   (defvar frame-title-keys)
;;   (defvar frame-title-separator "  —  ")
;;   (setq frame-title-format '("Emacs" frame-title-keys))
;;   (add-hook
;;    'keys-post-change-hook
;;    #'(lambda ()
;;        (let ((indicator (keys-indicator)))
;;          (setq frame-title-keys
;;                (when (and global-keys-mode (not (string-empty-p indicator)))
;;                  (format "%s%s" frame-title-separator indicator))))))
;;
;;   ;; Ready to turn on keys-mode!
;;   (global-keys-mode)
;;   )
;; ```
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
  "Separator to use in the `global-keys-mode' indicator (see `keys-indicator')."
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

(defcustom keys-error t
  "If not NIL, error if we failed to use a key we are learning.

This means, disallow commands that are associated with any keys
that we are learning from being called manually, or with other
keybindings."
  :type 'boolean
  :group 'keys)

(defcustom keys-post-change-hook nil
  "Hook run after keys have changed."
  :type 'hook
  :group 'keys)

(defvar keys-keys-current '())

(defun keys-indicator ()
  "Generate keys's indicator string.

Can be used in the mode-line, frame title, or other \"mode line constructs\"."
  (if keys--missed-key
      (format "ERROR: missed %s" ; TODO: make this customizable
              (propertize keys--missed-key 'face 'help-key-binding))
    (concat
     (mapconcat 'identity
                (if keys-display-amount
                    (seq-take keys-keys-current keys-display-amount)
                  keys-keys-current)
                keys-indicator-separator)
     (when (and keys-indicator-truncated
                (< keys-display-amount (length keys-keys-current)))
       (concat keys-indicator-separator keys-indicator-truncated)))))

(defun keys-reset ()
  "Reset the state of `global-keys-mode', including the mode-line indicator.

You can e.g. integrate this with `midnight-mode'."
  (interactive)
  (setq keys-keys-current (copy-sequence keys-keys))
  (when keys-random
    (keys--shuffle-list keys-keys-current))
  (setq keys--keys-commands (keys--make-commands keys-keys))
  (setq keys--missed-key nil)
  (run-hooks 'keys-post-change-hook))

;;; Internal

;; A pre-calculated list of commands corresponding to our keys.
(defvar keys--keys-commands '())

;; If not NIL, we have an error to display in the indicator...!
(defvar keys--missed-key nil)

;; From: https://gist.github.com/purcell/34824f1b676e6188540cdf71c7cc9fc4
(defun keys--shuffle-list (list)
  "Shuffles LIST randomly, modying it in-place."
  (dolist (i (reverse (number-sequence 1 (1- (length list)))))
    (let ((j (random (1+ i)))
	      (tmp (elt list i)))
      (setf (elt list i) (elt list j))
      (setf (elt list j) tmp)))
  list)

(defun keys--post-command ()
  "Check if the command matches one of the keys we are trying to learn."
  (when keys--missed-key
    (setq keys--missed-key nil)
    (run-hooks 'keys-post-change-hook))
  (let ((key (key-description (this-single-command-keys))))
    (if
        ;; The last command was invoked by a key we are learning.
        (member key keys-keys)
        (when (member key keys-keys-current)
          (setq keys-keys-current (delete key keys-keys-current))
          (run-hooks 'keys-post-change-hook))
      (when keys-error
        ;; FIXME: should support multiple keys for the same command.
        (let ((idx (keys--nth-elt real-this-command keys--keys-commands)))
          ;; The command has a key set, but was invoked some other way.
          (when idx
            (let ((learning-key (nth idx keys-keys)))
              ;; NOTE: Don't error or Emacs will remove our sneaky command hook.
              (setq keys--missed-key learning-key)
              (run-hooks 'keys-post-change-hook)
              (beep))))))))

;; Um. Why is this not built in.
;; From https://emacs.stackexchange.com/a/10496/15023
(defun keys--nth-elt (element xs)
  "Return zero-indexed position of ELEMENT in list XS, or nil if absent."
  (let ((idx  0))
    (catch 'nth-elt
      (dolist (x  xs)
        (when (equal element x) (throw 'nth-elt idx))
        (setq idx (1+ idx)))
      nil)))

(defun keys--make-commands (keys)
  "Convert list KEYS to a list of commands."
  (mapcar #'(lambda (elt) (key-binding (kbd elt))) keys))

(defun keys--enable ()
  "Initialize `global-keys-mode'."
  (add-hook 'post-command-hook 'keys--post-command)
  (keys-reset))

(defun keys--disable ()
  "Cleanup `global-keys-mode'."
  (remove-hook 'post-command-hook 'keys--post-command)
  (run-hooks 'keys-post-change-hook))

;;; Autoloads

;;;###autoload
(define-minor-mode global-keys-mode
  "Toggle `global-keys-mode'.

Starts listening for the keys defined in `keys-keys'. If you
update that variable after calling `global-keys-mode', just call
`keys-reset'."
  :global t
  :init-value nil
  :lighter nil
  :keymap nil
  :group 'keys

  (if global-keys-mode
      (keys--enable)
    (keys--disable)))

(provide 'keys)
;;; keys.el ends here
