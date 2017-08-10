;;; hasky-stack.el --- Interface to the Stack Haskell development tool -*- lexical-binding: t; -*-
;;
;; Copyright © 2017 Mark Karpov <markkarpov92@gmail.com>
;;
;; Author: Mark Karpov <markkarpov92@gmail.com>
;; URL: https://github.com/hasky-mode/hasky-stack
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4") (avy-menu "0.2") (f "0.18.0") (magit-popup "20170214.347"))
;; Keywords: programming, haskell
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
;; Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Interface to the Stack development tool.  Proper description pending.

;;; Code:

(require 'avy-menu)
(require 'cl-lib)
(require 'f)
(require 'magit-popup)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Settings & Variables

(defgroup hasky-stack nil
  "Interface to the Stack Haskell development tool."
  :group  'programming
  :tag    "Hasky Stack"
  :prefix "hasky-stack-"
  :link   '(url-link :tag "GitHub"
                     "https://github.com/hasky-mode/hasky-stack"))

(defvar hasky-stack--last-directory nil
  "Path to project's directory last time `hasky-stack--prepare' was called.

This is mainly used to check when we need to reload/re-parse
project-local settings that user might have.")

(defvar hasky-stack--cabal-mod-time nil
  "Time of last modification of \"*.cabal\" file.

This is usually set by `hasky-stack--prepare'.")

(defvar hasky-stack--project-name nil
  "Name of current project extracted from \"*.cabal\" file.

This is usually set by `hasky-stack--prepare'.")

(defvar hasky-stack--project-version nil
  "Version of current project extracted from \"*.cabal\" file.

This is usually set by `hasky-stack--prepare'.")

(defvar hasky-stack--project-targets nil
  "List of build targets (strings) extracted from \"*.cabal\" file.

This is usually set by `hasky-stack--prepare'.")

(defcustom hasky-stack-executable nil
  "Path to Stack executable.

If it's not NIL, this value is used in invocation of Stack
commands instead of the standard \"stack\" string.  Set this
variable if your Stack is in a strange place where OS cannot find
it.

Note that the path is quoted with `shell-quote-argument' before
being used to compose command line."
  :tag "Path to Stack Executable"
  :type '(choice (file :must-match t)
                 (const :tag "Use Default" nil)))

(defcustom hasky-stack-read-function #'completing-read
  "Function to be called when user has to choose from list of alternatives."
  :tag  "Completing Function"
  :type '(radio (function-item completing-read)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Various utilities

(defun hasky-stack--all-matches (regexp)
  "Return list of all stings matching REGEXP in current buffer."
  (let (matches)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (push (match-string-no-properties 1) matches))
    (reverse matches)))

(defun hasky-stack--parse-cabal-file (filename)
  "Parse \"*.cabal\" file with name FILENAME and set some variables.

The following variables are set:

  `hasky-stack--project-name'
  `hasky-stack--project-version'
  `hasky-stack--project-targets'

This is used by `hasky-stack--prepare'."
  (with-temp-buffer
    (insert-file-contents filename)
    ;; project name
    (setq hasky-stack--project-name
          (car (hasky-stack--all-matches
                "^[[:blank:]]*name:[[:blank:]]+\\([[:word:]-]+\\)")))
    ;; project version
    (setq hasky-stack--project-version
          (car (hasky-stack--all-matches
                "^[[:blank:]]*version:[[:blank:]]+\\([[:digit:]\\.]+\\)")))
    ;; project targets
    (setq
     hasky-stack--project-targets
     (append
      ;; library
      (mapcar (lambda (_) (format "lib:%s" hasky-stack--project-name))
              (hasky-stack--all-matches
               "^[[:blank:]]*library[[:blank:]]*"))
      ;; executable
      (mapcar (lambda (x) (format "exe:%s" x))
              (hasky-stack--all-matches
               "^[[:blank:]]*executable[[:blank:]]+\\([[:word:]-]+\\)"))
      ;; test suites
      (hasky-stack--all-matches
       "^[[:blank:]]*test-suite[[:blank:]]+\\([[:word:]-]+\\)")
      ;; benchmarks
      (hasky-stack--all-matches
       "^[[:blank:]]*benchmark[[:blank:]]+\\([[:word:]-]+\\)")))))

(defun hasky-stack--find-dir-of-file (regexp)
  "Find file whose name satisfies REGEXP traversing upwards.

Return absolute path to directory containing that file or NIL on
failure.  Returned path is guaranteed to have trailing slash."
  (let ((dir (f-traverse-upwards
              (lambda (path)
                (directory-files path t regexp t))
              (f-full default-directory))))
    (when dir
      (f-slash dir))))

(defun hasky-stack--mod-time (filename)
  "Return time of last modification of file FILENAME."
  (nth 5 (file-attributes filename 'integer)))

(defun hasky-stack--executable ()
  "Return path to stack executable if it's available and NIL otherwise."
  (let ((default "stack")
        (custom  hasky-stack-executable))
    (cond ((executable-find default)     default)
          ((and custom (f-file? custom)) custom))))

(defun hasky-stack--initialized-p (dir)
  "Return non-NIL value if \"stack.yaml\" file exists in DIR."
  (f-file? (f-expand "stack.yaml" dir)))

(defun hasky-stack--templates ()
  "Return list of available Stack templates."
  (with-temp-buffer
    (shell-command
     (format "%s templates"
             (hasky-stack--executable))
     (current-buffer))
    (remove "Template"
            (hasky-stack--all-matches "^\\(\\([[:alnum:]]\\|-\\)+\\)"))))

(defun hasky-stack--completing-read (prompt &optional collection require-match)
  "Read user's input using `hasky-stack-read-function'.

PROMPT is the prompt to show and COLLECTION represents valid
choices.  If REQUIRE-MATCH is not NIL, don't let user input
something different from items in COLLECTION.

COLLECTION is allowed to be a string, in this case it's
automatically wrapped to make it one-element list.

If COLLECTION contains \"none\", and user selects it, interpret
it as NIL.  If user aborts entering of the input, return NIL.

Finally, if COLLECTION is nil, plain `read-string' is used."
  (let* ((collection
          (if (listp collection)
              collection
            (list collection)))
         (result
          (if collection
              (funcall hasky-stack-read-function
                       prompt
                       collection
                       nil
                       require-match
                       nil
                       nil
                       (car collection))
            (read-string prompt))))
    (unless (and (string= result "none")
                 (member result collection))
      result)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preparation

(defun hasky-stack--prepare ()
  "Locate, read, and parse configuration files and set various variables.

This commands searches for first \"*.cabal\" files traversing
directories upwards beginning with `default-directory'.  When
Cabal file is found, the following variables are set:

  `hasky-stack--project-name'
  `hasky-stack--project-version'
  `hasky-stack--project-targets'

At the end, `hasky-stack--last-directory' and
`hasky-stack--cabal-mod-time' are set.  Note that this function
is smart enough to avoid re-parsing all the stuff every time.  It
can detect when we are in different project or when some files
have been changed since its last invocation.

Returned value is T on success and NIL on failure (when no
\"*.cabal\" files is found)."
  (let* ((project-directory
          (hasky-stack--find-dir-of-file "^.+\.cabal$"))
         (cabal-file
          (car (and project-directory
                    (f-glob "*.cabal" project-directory)))))
    (when cabal-file
      (if (or (not hasky-stack--last-directory)
              (not (f-same? hasky-stack--last-directory
                            project-directory)))
          (progn
            ;; We are in different directory (or it's the first
            ;; invocation). This means we should unconditionally parse
            ;; everything without checking of date of last modification.
            (hasky-stack--parse-cabal-file cabal-file)
            (setq hasky-stack--cabal-mod-time (hasky-stack--mod-time cabal-file))
            ;; Set last directory for future checks.
            (setq hasky-stack--last-directory project-directory)
            t) ;; Return T on success.
        ;; We are in an already visited directory, so we don't need to reset
        ;; `hasky-stack--last-directory' this time. We need to
        ;; reread/re-parse *.cabal file if it has been modified though.
        (when (time-less-p hasky-stack--cabal-mod-time
                           (hasky-stack--mod-time cabal-file))
          (hasky-stack--parse-cabal-file cabal-file)
          (setq hasky-stack--cabal-mod-time (hasky-stack--mod-time cabal-file)))
        t))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Low-level construction of individual commands

(defun hasky-stack--format-command (command &rest args)
  "Generate textual representation of a command.

COMMAND is the name of command and ARGS are arguments (strings).
Result is expected to be used as argument of `compile'."
  (mapconcat
   #'identity
   (append
    (list (shell-quote-argument (hasky-stack--executable))
          command)
    (mapcar #'shell-quote-argument
            (remove nil args)))
   " "))

(defun hasky-stack--exec-command (dir command &rest args)
  "Call target as if from DIR performing COMMAND with arguments ARGS.

Arguments are quoted if necessary and NIL arguments are ignored.
This uses `compile' internally."
  (let ((default-directory dir)
        (compilation-buffer-name-function
         (lambda (_major-mode)
           (format "*%s-%s*"
                   (downcase
                    (replace-regexp-in-string
                     "[[:space:]]"
                     "-"
                     hasky-stack--project-name))
                   "stack"))))
    (compile (apply #'hasky-stack--format-command command args))
    nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Popups

(magit-define-popup hasky-stack-build-popup
  "Show popup for the \"stack build\" command."
  'hasky-stack
  :switches '((?r "Dry run"           "--dry-run")
              (?t "Pedantic"          "--pedantic")
              (?f "Fast"              "--fast")
              (?s "Only snapshot"     "--only-snapshot")
              (?d "Only dependencies" "--only-dependencies")
              (?p "Profile"           "--profile")
              (?c "Coverage"          "--coverage")
              (?b "Copy bins"         "--copy-bins")
              (?l "Library profiling" "--library-profiling")
              (?e "Executable profiling" "--executable-profiling"))
  :options  '((?o "GHC options" "--ghc-options=")
              (?e "Exec"        "--exec=")
              (?b "Benchmark arguments" "--benchmark-arguments=")
              (?t "Test arguments"      "--test-arguments=")
              (?h "Haddock arguments"   "--haddock-arguments="))
  :actions  '((?b "Build"   hasky-stack-build)
              (?e "Bench"   hasky-stack-bench)
              (?t "Test"    hasky-stack-test)
              (?h "Haddock" hasky-stack-haddock))
  :default-action 'hasky-stack-build)

(defun hasky-stack-build (target &optional args)
  "Execute \"stack build\" command for TARGET with ARGS."
  (interactive
   (list (hasky-stack--completing-read
          "Build target: "
          (cons hasky-stack--project-name
                hasky-stack--project-targets))
         (hasky-stack-build-arguments)))
  (apply
   #'hasky-stack--exec-command
   hasky-stack--last-directory
   "build"
   target
   args))

(defun hasky-stack-bench (target &optional args)
  "Execute \"stack bench\" command for TARGET with ARGS."
  (interactive
   (list (hasky-stack--completing-read
          "Bench target: "
          (cons hasky-stack--project-name
                hasky-stack--project-targets))
         (hasky-stack-build-arguments)))
  (apply
   #'hasky-stack--exec-command
   hasky-stack--last-directory
   "bench"
   target
   args))

(defun hasky-stack-test (target &optional args)
  "Execute \"stack test\" command for TARGET with ARGS."
  (interactive
   (list (hasky-stack--completing-read
          "Test target: "
          (cons hasky-stack--project-name
                hasky-stack--project-targets))
         (hasky-stack-build-arguments)))
  (apply
   #'hasky-stack--exec-command
   hasky-stack--last-directory
   "test"
   target
   args))

(defun hasky-stack-haddock (&optional args)
  "Execute \"stack haddock\" command for TARGET with ARGS."
  (interactive
   (list (hasky-stack-build-arguments)))
  (apply
   #'hasky-stack--exec-command
   hasky-stack--last-directory
   "haddock"
   args))

(magit-define-popup hasky-stack-clean-popup
  "Show popup for the \"stack clean\" command."
  'hasky-stack
  :switches '((?f "Full"  "--full"))
  :actions  '((?c "Clean" hasky-stack-clean))
  :default-action 'hasky-stack-clean)

(defun hasky-stack-clean (&optional args)
  "Execute \"stack clean\" command for TARGET with ARGS."
  (interactive
   (list (hasky-stack-build-arguments)))
  (apply
   #'hasky-stack--exec-command
   hasky-stack--last-directory
   "clean"
   hasky-stack--project-name
   args))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; High-level interface

;;;###autoload
;; (defun hasky-stack-execute (&optional command)
;;   "Perform cabal command COMMAND.

;; When called interactively or when COMMAND is NIL, propose to
;; choose command with `ebal-select-command-function'."
;;   (interactive)
;;   (if (ebal--target-executable)
;;       (if (ebal--prepare)
;;           (let* ((command-alist
;;                   (if (ebal--cabal-mode-p)
;;                       ebal--cabal-command-alist
;;                     ebal--stack-command-alist))
;;                  (command
;;                   (or command
;;                       (intern
;;                        (funcall
;;                         ebal-select-command-function
;;                         "Choose command: "
;;                         (mapcar (lambda (x) (symbol-name (car x)))
;;                                 command-alist)
;;                         nil
;;                         t))))
;;                  (fnc (cdr (assq command command-alist))))
;;             (when fnc
;;               (funcall fnc)))
;;         (message "Cannot locate ‘.cabal’ file."))
;;     (message "Cannot local Cabal executable on this system.")))

;;;###autoload
(defun hasky-stack-init (project-name template)
  "Initialize the current directory by using a Stack template.

PROJECT-NAME is the name of project and TEMPLATE is quite
obviously template name."
  (interactive
   (list (hasky-stack--completing-read
          "Project name: "
          (file-name-nondirectory
           (directory-file-name
            default-directory)))
         (hasky-stack--completing-read
          "Use template: "
          (cons "none" (hasky-stack--templates))
          t)))
  (if (hasky-stack--prepare)
      (message "The directory is already initialized, it seems")
    (let ((hasky-stack--project-name project-name))
      (hasky-stack--exec-command
       default-directory
       "new"
       "--bare"
       project-name
       template))))

(provide 'hasky-stack)

;;; hasky-stack.el ends here
