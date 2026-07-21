# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

Types of changes:

- `Added`: for new features.
- `Changed`: for changes in existing functionality.
- `Deprecated`: for soon-to-be removed features.
- `Removed`: for now removed features.
- `Fixed`: for any bug fixes.
- `Security`: in case of vulnerabilities.

## [0.3.0] - 2026-07-21

### Added

- `keycoach-indicator-target` puts the indicator in the frame title, mode line,
  or header line for you, replacing all the hook boilerplate.  Defaults to nil,
  which displays nothing, as before.  Whatever it replaces is restored when the
  mode is turned off.
- `keycoach-indicator-string` holds the formatted indicator, refreshed on every
  change. Use it directly in a mode line construct if you want the indicator
  somewhere `keycoach-indicator-target` doesn't reach.
- `keycoach-indicator-format` allows the indicator to be formatted before
  display, so you can add padding or a separator.

### Changed

- **Renamed the package from `keys` to `keycoach`**.  Every symbol moved from
  the `keys-` prefix to `keycoach-`, and `global-keys-mode` is now
  `global-keycoach-mode`.  To upgrade, rename `keys-*` options in your config
  (e.g. `keys-keys` becomes `keycoach-keys`) and call `global-keycoach-mode`.

## [0.2.4] - 2026-07-07

### Added

- CI: byte-compile and tests across Emacs 25.3 through snapshot.

### Changed

- Missed-key detection now looks up command bindings live instead of using a
  snapshot taken at reset time. It now respects mode-local maps and rebinds,
  and supports multiple keys bound to the same command.

### Fixed

- Missed-key indicator no longer references a face that doesn't exist on
  Emacs < 28.

## [0.2.3] - 2026-07-07

### Added

- Owl mascot!
- Customize group for `keys` (options now browsable via `M-x customize-group`).
- ERT test suite (`keys-test.el`).

### Fixed

- `keys-indicator` crashed when `keys-display-amount` was nil (the default).
- Indicator kept showing "ERROR: missed ..." after disabling `global-keys-mode`.
- Customize types for `keys-display-amount` and `keys-indicator-truncated`
  rejected their documented nil values.
- README documented nonexistent `keys-force`; the real option is `keys-error`.

## [0.2.2] - Sun 31, 2023

### Added

- License!

## [0.2.1] - Sun 31, 2023

### Added

- Commentary in the elisp.

## [0.2.0] - Sun 31, 2023

### Added

- `keys-random` customization to randomize keys.
- `keys-reset` command.
- `keys-error` customization to error on missed keys.

## [0.1.0] - Sat 30, 2023

### Added

- Provide `keys-mode` with indicator for mode-line, frame title etc.
