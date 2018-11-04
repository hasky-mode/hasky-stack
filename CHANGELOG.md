## Hasky Stack 0.9.0

* Added support for the `stack run` command.

## Hasky Stack 0.8.0

* Make commands like “build → bench” and “build → test” only propose targets
  that make sense. For example, targets for “build → bench” won't include
  test suite components.

* Added `--copy-compiler-tool` to build popup.

## Hasky Stack 0.7.0

* Fix automatic opening of Haddocks with Stack 1.6.1.

* Fix propagation of arguments in the package action popup.

## Hasky Stack 0.6.0

* Renamed `hasky-stack-project-action-popup` to
  `hasky-stack-package-action-popup`.

* Updated `README.md` to reflect current state of the package.

## Hasky Stack 0.5.0

* Added a variable that allows to make Hasky Stack open generated Haddocks
  automatically.

* Enhanced detection of home page by grabbing git location and
  reconstructing URL from that. Given that 99% of projects are on GitHub it
  works fine.

## Hasky Stack 0.4.0

* Added commands to edit Cabal and `stack.yaml` files from the root popup
  (invoked by `hasky-stack-execute`).

## Hasky Stack 0.3.0

* Added the `hasky-stack-package-action` command allowing to install and
  lookup information about packages.

* Renamed the face `hasky-project-version` to `hasky-stack-project-version`.

## Hasky Stack 0.2.0

* Fixed the `stack upload` command.

* Fixed the `stack clean` command.

* Added support for `--file-watch` to the `stack build` popup.

* Made `hasky-stack-auto-target` switchable from the `stack build` popup.

* Added `hasky-stack-auto-open-coverage-reports` customization setting and
  the functionality for opening coverage reports automatically.

## Hasky Stack 0.1.0

* Initial release.
