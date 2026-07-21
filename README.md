<div align="center">
  <img src="mascot.svg" alt="keycoach mascot — a keycap in a coach's cap, with a whistle resting against it" width="180"/>

# keycoach

*Put all your crazy Emacs keys in the mode-line, frame title, or wherever!
`keycoach` helps you learn them!*

<img src="keycoach-frame-title.png" alt="keycoach indicator showing keybindings in the Emacs frame title" width="370">
</div>

## Usage

```
M-x global-keycoach-mode
```

keycoach is a simple, unopinionated package with two main features:

- A configurable display to remind you about keybindings you want to learn.
  Keys are removed from the display (called the *indicator*) when they're typed.
- Error messages if you fail to use one of your keys to invoke a command (i.e.
  `M-x`).

For example, you can try to use all your keys every day, and set `midnight-mode`
to reset them for the next day. See below!

## Get started

For a totally basic setup, this turns on `global-keycoach-mode` and sticks some
keys in your frame title:

```el
(use-package keycoach
  :load-path "~/.emacs.d/packages/keycoach" ; Coming to MELPA soon I hope

  :config

  (setq keycoach-keys '("s-w" "M-F" "C-M-y"))

  ;; Update the indicator every time it should change.
  ;; You can also just do
  ;;   `(:eval (when global-keycoach-mode (keycoach-indicator)))`,
  ;;   but this avoids constantly re-calculating the indicator.
  ;; The same idea applies for the mode-line, header, etc.
  (defvar frame-title-keys)
  (defvar frame-title-separator "  —  ")
  (setq frame-title-format '("Emacs" frame-title-keys))
  (add-hook
   'keycoach-post-change-hook
   (lambda ()
       (let ((indicator (keycoach-indicator)))
         (setq frame-title-keys
               (when (and global-keycoach-mode (not (string-empty-p indicator)))
                 (format "%s%s" frame-title-separator indicator))))))

  ;; Ready to turn on keycoach!
  (global-keycoach-mode)
  )
```

And here's an example exposing more configuration knobs:

```el
;; In :config section...

;; Customize some settings
(setq
 keycoach-keys '("s-w" "M-F" "C-M-y" "C-x 2" "C-M-," "C-S-v" "s-D" "M-W")
 keycoach-display-amount 2 ; How many keys to show at once
 keycoach-indicator-separator " | " ; Customize the indicator!
 keycoach-random t ; By default, keys are shown in a random order

              ; Calling associated commands manually is an error!
 keycoach-error t ; So if you bind `git-link` to C-c g, you get an error when
              ; invoking `M-x git-link` with this configuration set.
 )

;; Integrate with midnight-mode.
(require 'midnight)
(midnight-delay-set 'midnight-delay "1:00am")
(add-hook 'midnight-hook #'keycoach-reset)
```

## Related

- [`which-key-mode`]: a built-in mode that helps you discover keys.  It shows
  available keys once you start a key sequence.  Answers "what can I press
  here?", while keycoach enforces "press what you promised to learn."
- [free-keys](https://github.com/Fuco1/free-keys): find unused keybindings to
  assign before adding them to your learning list.
- [keyfreq](https://github.com/dacap/keyfreq): statistics on which commands
  and keys you use the most.

## TODO

This repo is basic right now, but I see potential for something really cool if
anyone wants to run with it.

- [ ] EASY: Allow setting disallowed commands. For example, if you have
      `my-other-window`, disallow `other-window` to help you learn the new one.
- [ ] Support for keymaps.
- [ ] Support detecting prefix arguments, e.g. `C-u C-u C-;`.
- [ ] Functions to automate adding indicator to mode/header/frame-title for the
      user.
- [ ] Some nice statistics for key usage.
- [ ] Persist progress across Emacs restarts (e.g. a `keycoach-save-file`), so a
      mid-day restart doesn't bring back already-practiced keys.

## Disclaimer

If you use this I'm not liable for your microwave exploding.
