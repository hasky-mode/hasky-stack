# Hasky Stack

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![MELPA](https://melpa.org/packages/hasky-stack-badge.svg)](https://melpa.org/#/hasky-stack)
[![Build Status](https://travis-ci.org/hasky-mode/hasky-stack.svg?branch=master)](https://travis-ci.org/hasky-mode/hasky-stack)

This is an Emacs interface to the [Stack](https://haskellstack.org) Haskell
development tool.

![Root menu](https://raw.githubusercontent.com/hasky-mode/hasky-stack/gh-pages/hasky-stack-root-menu.png)

![Build sub-menu](https://raw.githubusercontent.com/hasky-mode/hasky-stack/gh-pages/hasky-stack-build-menu.png)

![Package action menu](https://raw.githubusercontent.com/hasky-mode/hasky-stack/gh-pages/hasky-stack-package-action-menu.png)

## Installation

Download this package and place it somewhere, so Emacs can see it. Then put
`(require 'hasky-stack)` into your configuration file. Done!

It's available via MELPA, so you can just <kbd>M-x package-install RET
hasky-stack</kbd>.

## Usage

Bind the following useful commands:

```emacs-lisp
(global-set-key (kbd "<next> h e") #'hasky-stack-execute)
(global-set-key (kbd "<next> h h") #'hasky-stack-package-action)
(global-set-key (kbd "<next> h i") #'hasky-stack-new)
```

* `hasky-stack-execute` opens a popup with a collection of stack commands
  you can run. Many commands have their own sub-popups like in Magit.

* `hasky-stack-package-action` allows to perform actions on package that the
  user selects from the list of all available packages.

* `hasky-stack-new` allows to create a new project in current directory
  using a Stack template.

## Switchable variables

There is a number of variables that control various aspects of the package.
They can be set with `setq` or via the customization mechanisms. This way
one can change their default values. However, sometimes it's desirable to
quickly toggle the variables and it's possible to do directly from the popup
menus: just hit the key displayed under the “variables” section.

Switchable variables include:

* `hasky-stack-auto-target`—whether to automatically select the default
  build target (build sub-popup).
* `hasky-stack-auto-open-coverage-reports`—whether to attempt to
  automatically open coverage report in browser (build sub-popup).
* `hasky-stack-auto-open-haddocks`—whether to attempt to automatically open
  Haddocks in browser (build sub-popup).
* `hasky-stack-auto-newest-version`—whether to install newest version of
  package without asking (package action popup).

## Customization

There is a number of customization options that are available via <kbd>M-x
customize-group hasky-stack</kbd>.

## License

Copyright © 2017–2018 Mark Karpov

Distributed under GNU GPL, version 3.
