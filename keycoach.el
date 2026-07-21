;;; keycoach.el --- Learn keybindings  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Marcin Swieczkowski <marcin@realemail.net>
;;
;; Author: Marcin Swieczkowski <marcin@realemail.net>
;; Assisted-by: Claude:claude-fable-5
;; URL: https://github.com/mrcnski/keycoach
;; Version: 0.3.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: help
;;
;; This file is NOT part of GNU Emacs.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;; Commentary:
;;
;; Provides `global-keycoach-mode' to help you remember and learn new
;; keybindings!
;;
;; Features:
;;
;; keycoach is a simple, unopinionated package with two main features:
;;
;; - A configurable display to remind you about keybindings you want to learn.
;;   Keys are removed from the display (called the *indicator*) when they're
;;   typed.
;;
;; - Error messages if you fail to use one of your keys to invoke a command
;;   (i.e. `M-x`).
;;
;; For example, you can try to use all your keys every day, and set
;; `midnight-mode` to reset them for the next day.  See below!
;;
;; Get started:
;;
;; For a totally basic setup, this turns on `global-keycoach-mode` and sticks
;; some keys in your frame title:
;;
;; ```el
;; (use-package keycoach
;;   :load-path "~/.emacs.d/packages/keycoach" ; Coming to MELPA soon I hope
;;
;;   :config
;;
;;   (setq keycoach-keys '("s-w" "M-F" "C-M-y")
;;         keycoach-indicator-target 'frame-title)
;;
;;   ;; Ready to turn on keycoach!
;;   (global-keycoach-mode)
;;   )
;; ```
;;
;; `keycoach-indicator-target' can be `frame-title', `mode-line',
;; `header-line', or nil.  Nil is the default: keycoach then displays nothing
;; on its own, and you place `keycoach-indicator-string' wherever you want it.
;;
;; Related packages:
;;
;; - `which-key-mode': a built-in mode that helps you discover keys.  It shows
;; available keys once you start a key sequence.  Answers "what can I press
;; here?", while keycoach enforces "press what you promised to learn."
;; - See the README for more.
;;
;;; Code:

(require 'seq)

(defgroup keycoach nil
  "Learn keybindings by displaying them in the mode-line, frame title, etc."
  :group 'help
  :prefix "keycoach-"
  :link '(url-link :tag "GitHub" "https://github.com/mrcnski/keycoach"))

(defcustom keycoach-keys '()
  "Keys to learn."
  :type '(repeat string)
  :group 'keycoach)

(defcustom keycoach-display-amount nil
  "If not NIL, only show this many keybindings in the indicator."
  :type '(choice (const :tag "Show all" nil) integer)
  :group 'keycoach)

(defcustom keycoach-indicator-separator " | "
  "Separator to use in the `global-keycoach-mode' indicator.

See `keycoach-indicator'."
  :type 'string
  :group 'keycoach)

(defcustom keycoach-indicator-truncated "…"
  "If not NIL, show this string in the indicator when not all keys are displayed."
  :type '(choice (const :tag "No truncation indicator" nil) string)
  :group 'keycoach)

(defcustom keycoach-random t
  "If not NIL, shuffle the keys everytime the keys are reset.

This includes when `keycoach-reset' is called and when the mode is enabled."
  :type 'boolean
  :group 'keycoach)

(defcustom keycoach-error t
  "If not NIL, error if we failed to use a key we are learning.

This means, disallow commands that are associated with any keys
that we are learning from being called manually, or with other
keybindings."
  :type 'boolean
  :group 'keycoach)

(defcustom keycoach-post-change-hook nil
  "Hook run after keys have changed."
  :type 'hook
  :group 'keycoach)

(defcustom keycoach-indicator-target nil
  "Where `global-keycoach-mode' should display the indicator.

When nil, keycoach displays nothing on its own.  Place
`keycoach-indicator-string' wherever you like instead.

Otherwise the indicator is installed when the mode is enabled, and
removed again when it is disabled."
  :type '(choice (const :tag "Nowhere (place it yourself)" nil)
                 (const :tag "Mode line" mode-line)
                 (const :tag "Header line" header-line)
                 (const :tag "Frame title" frame-title))
  :set (lambda (symbol value)
         (set-default symbol value)
         ;; Move the indicator right away if the mode is already on.
         ;; Installing takes it out of wherever it was first.
         (when (bound-and-true-p global-keycoach-mode)
           (keycoach--install-indicator)))
  :group 'keycoach)

(defcustom keycoach-indicator-format "%s"
  "Format string applied to the indicator before it is displayed.

Use it for padding or a separator, e.g. \"  —  %s\".  It is only
applied when the indicator is non-empty, so nothing is left behind
once every key has been used.

See `keycoach-indicator-string'."
  :type 'string
  :group 'keycoach)

(defvar keycoach-indicator-string ""
  "The indicator, formatted and kept up to date by `global-keycoach-mode'.

Refreshed whenever the keys change, so putting this symbol in a mode
line construct is cheaper than calling `keycoach-indicator' on every
redisplay.  It is empty while the mode is off.

Useful when `keycoach-indicator-target' does not put the indicator
where you want it: add the symbol to `mode-line-format',
`frame-title-format', or anywhere else that takes a construct.")

(defvar keycoach-keys-current '())

;; If not NIL, we have an error to display in the indicator...!
(defvar keycoach--missed-key nil)

(defun keycoach-indicator ()
  "Generate keycoach's indicator string.

Can be used in the mode-line, frame title, or other \"mode line constructs\"."
  (if keycoach--missed-key
      (format "ERROR: missed %s" ; TODO: make this customizable
              ;; `help-key-binding' only exists since Emacs 28.
              (if (facep 'help-key-binding)
                  (propertize keycoach--missed-key 'face 'help-key-binding)
                keycoach--missed-key))
    (concat
     (mapconcat #'identity
                (if keycoach-display-amount
                    (seq-take keycoach-keys-current keycoach-display-amount)
                  keycoach-keys-current)
                keycoach-indicator-separator)
     (when (and keycoach-indicator-truncated
                keycoach-display-amount
                (< keycoach-display-amount (length keycoach-keys-current)))
       (concat keycoach-indicator-separator keycoach-indicator-truncated)))))

(defun keycoach-reset ()
  "Reset the state of `global-keycoach-mode', including the mode-line indicator.

You can e.g. integrate this with `midnight-mode'."
  (interactive)
  (setq keycoach-keys-current (copy-sequence keycoach-keys))
  (when keycoach-random
    (keycoach--shuffle-list keycoach-keys-current))
  (setq keycoach--missed-key nil)
  (keycoach--changed))

;;; Internal

(defvar keycoach--installed-target nil
  "The `keycoach-indicator-target' that is currently installed, if any.")

(defvar keycoach--saved-format nil
  "The format variable keycoach replaced, saved so it can be restored.")

(defun keycoach--changed ()
  "Refresh the indicator, then run `keycoach-post-change-hook'.

The indicator is empty while the mode is off, so that `keycoach-reset'
called from a timer doesn't put keys back on screen."
  (let ((indicator (if (bound-and-true-p global-keycoach-mode)
                       (keycoach-indicator)
                     "")))
    (setq keycoach-indicator-string
          (if (string= "" indicator)
              ""
            (format keycoach-indicator-format indicator))))
  ;; The keys can change from a timer (`midnight-mode', say), so we can't
  ;; rely on the redisplay that follows a command.
  (force-mode-line-update t)
  (run-hooks 'keycoach-post-change-hook))

(defun keycoach--wrap-format (format)
  "Return mode line construct FORMAT with the indicator appended to it.

A nil FORMAT becomes an empty string: a construct starting with nil
would be read as a conditional rather than as a list of elements."
  (list (or format "") 'keycoach-indicator-string))

(defun keycoach--install-indicator ()
  "Display the indicator in `keycoach-indicator-target'."
  ;; Enabling an already-enabled mode runs this again.  Without this, the
  ;; format would be wrapped a second time, and the wrapped copy stranded in
  ;; `keycoach--saved-format' for good.
  (keycoach--uninstall-indicator)
  (cond
   ((eq keycoach-indicator-target 'mode-line)
    ;; Leave the rest of `global-mode-string' alone, so that other packages
    ;; displaying there keep working.  It's a mode line construct, so it may
    ;; be nil or a bare string rather than a list.
    (unless (consp global-mode-string)
      (setq global-mode-string (list (or global-mode-string ""))))
    (unless (memq 'keycoach-indicator-string global-mode-string)
      (setq global-mode-string
            (append global-mode-string '(keycoach-indicator-string)))))
   ((eq keycoach-indicator-target 'header-line)
    (setq keycoach--saved-format (default-value 'header-line-format))
    (setq-default header-line-format
                  (keycoach--wrap-format keycoach--saved-format)))
   ((eq keycoach-indicator-target 'frame-title)
    (setq keycoach--saved-format frame-title-format)
    (setq frame-title-format
          (keycoach--wrap-format keycoach--saved-format))))
  (setq keycoach--installed-target keycoach-indicator-target))

(defun keycoach--uninstall-indicator ()
  "Stop displaying the indicator wherever it was installed."
  (cond
   ((eq keycoach--installed-target 'mode-line)
    (setq global-mode-string
          (delq 'keycoach-indicator-string global-mode-string)))
   ;; Only restore what we replaced if nobody has touched it since, so that a
   ;; format set while the mode was on survives turning the mode off.
   ((eq keycoach--installed-target 'header-line)
    (when (equal (default-value 'header-line-format)
                 (keycoach--wrap-format keycoach--saved-format))
      (setq-default header-line-format keycoach--saved-format)))
   ((eq keycoach--installed-target 'frame-title)
    (when (equal frame-title-format
                 (keycoach--wrap-format keycoach--saved-format))
      (setq frame-title-format keycoach--saved-format))))
  (setq keycoach--installed-target nil
        keycoach--saved-format nil))

;; From: https://gist.github.com/purcell/34824f1b676e6188540cdf71c7cc9fc4
(defun keycoach--shuffle-list (list)
  "Shuffles LIST randomly, modying it in-place."
  (dolist (i (reverse (number-sequence 1 (1- (length list)))))
    (let ((j (random (1+ i)))
	      (tmp (elt list i)))
      (setf (elt list i) (elt list j))
      (setf (elt list j) tmp)))
  list)

(defun keycoach--post-command ()
  "Check if the command matches one of the keys we are trying to learn."
  (when keycoach--missed-key
    (setq keycoach--missed-key nil)
    (keycoach--changed))
  (let ((key (key-description (this-single-command-keys))))
    (if
        ;; The last command was invoked by a key we are learning.
        (member key keycoach-keys)
        (when (member key keycoach-keys-current)
          (setq keycoach-keys-current (delete key keycoach-keys-current))
          (keycoach--changed))
      (when (and keycoach-error (commandp real-this-command))
        ;; The command was invoked some other way. Look up its bindings on the
        ;; spot: this respects mode-local maps and later rebinds, and supports
        ;; multiple keys for the same command.
        (let ((learning-key
               (seq-find (lambda (k) (member k keycoach-keys))
                         (mapcar #'key-description
                                 (where-is-internal real-this-command)))))
          (when learning-key
            ;; NOTE: Don't error or Emacs will remove our sneaky command hook.
            (setq keycoach--missed-key learning-key)
            (keycoach--changed)
            (beep)))))))

(defun keycoach--enable ()
  "Initialize `global-keycoach-mode'."
  (add-hook 'post-command-hook #'keycoach--post-command)
  (keycoach--install-indicator)
  (keycoach-reset))

(defun keycoach--disable ()
  "Cleanup `global-keycoach-mode'."
  (remove-hook 'post-command-hook #'keycoach--post-command)
  (keycoach--uninstall-indicator)
  (setq keycoach--missed-key nil)
  ;; The keys survive being turned off, but `keycoach--changed' clears the
  ;; indicator: the mode variable is already nil by the time we run.
  (keycoach--changed))

;;; Autoloads

;;;###autoload
(define-minor-mode global-keycoach-mode
  "Toggle `global-keycoach-mode'.

Starts listening for the keys defined in `keycoach-keys'.  If you
update that variable after calling `global-keycoach-mode', just call
`keycoach-reset'."
  :global t
  :init-value nil
  :lighter nil
  :keymap nil
  :group 'keycoach

  (if global-keycoach-mode
      (keycoach--enable)
    (keycoach--disable)))

(provide 'keycoach)
;;; keycoach.el ends here
