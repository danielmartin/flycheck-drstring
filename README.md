# flycheck-drstring
Lint Swift documentation using DrString.

This library provides a [flycheck][] checker for Swift source code
using [DrString][].

## Screenshot

![Sample screenshot with flycheck-drstring in action](https://raw.githubusercontent.com/danielmartin/flycheck-drstring/master/screenshots/screenshot.png)

## Installation

### MELPA

A MELPA package is not available yet.

### Manual

Ensure `flycheck` is installed, then download this code and add the
directory to your Emacs `load-path`.

Then, in your `init.el`:

```lisp
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-drstring-setup))
```

Or, if you use [use-package][]:

``` emacs-lisp
(use-package flycheck-drstring
  :after flycheck
  :hook
  (flycheck-mode . flycheck-drstring-setup))
```

Make sure that the `drstring` binary is present on Emacs' `exec-path`.

## Usage

When `flycheck` is enabled (e.g. with `global-flycheck-mode`),
`swift-mode` buffers will be automatically checked using this checker.

This checker includes an error explainer. Invoking `flycheck-explain-error-at-point`
will show a Help buffer with information about any particular lint warning.

[flycheck]: https://github.com/flycheck/flycheck
[DrString]: https://github.com/dduan/DrString
[melpa]: http://melpa.org
[use-package]: https://github.com/jwiegley/use-package
