# Hasky Stack

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![MELPA](https://melpa.org/packages/hasky-stack-badge.svg)](https://melpa.org/#/hasky-stack)
[![Build Status](https://travis-ci.org/hasky-mode/hasky-stack.svg?branch=master)](https://travis-ci.org/hasky-mode/hasky-stack)

This is an interface to the [Stack](https://haskellstack.org) Haskell
development tool.

## Installation

Download this package and place it somewhere, so Emacs can see it. Then put
`(require 'hasky-stack)` into your configuration file. Done!

TODO: put here MELPA instructions once it's in MELPA.

## Usage

Bind just two commands, like this:

```emacs-lisp
(global-set-key (kbd "<next> h e") #'hasky-stack-execute)
(global-set-key (kbd "<next> h i") #'hasky-stack-new)
```

* `hasky-stack-execute` opens a popup with a collection of stack commands
  you can run. Many commands have their own popups like in Magit.

* `hasky-stack-new` allows to create a new project in current directory
  using a Stack template.

## Customization

There are a number of customization options that are available via <kbd>M-x
customize-group hasky-stack</kbd>.

## License

Copyright © 2017 Mark Karpov

Distributed under GNU GPL, version 3.
