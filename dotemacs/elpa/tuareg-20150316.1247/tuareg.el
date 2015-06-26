;;; tuareg.el --- OCaml mode for Emacs.  -*- coding: utf-8 -*-

;; Copyright (C) 1997-2006 Albert Cohen, all rights reserved.
;; Copyright (C) 2009-2010 Jane Street Holding, LLC.
;; Licensed under the GNU General Public License.

;; Author: Albert Cohen <Albert.Cohen@inria.fr>
;;      Sam Steingold <sds@gnu.org>
;;      Christophe Troestler <Christophe.Troestler@umons.ac.be>
;;      Till Varoquaux <till@pps.jussieu.fr>
;;      Sean McLaughlin <seanmcl@gmail.com>
;;      Stefan Monnier <monnier@iro.umontreal.ca>
;; Created: 8 Jan 1997
;; Version: 2.0.9
;; Package-Requires: ((caml "3.12.0.1"))
;; Keywords: ocaml languages
;; URL: https://github.com/ocaml/tuareg
;; EmacsWiki: TuaregMode

;;; Commentary:
;; Description:
;; Tuareg helps editing OCaml code, to highlight important parts of
;; the code, to run an OCaml toplevel, and to run the OCaml debugger
;; within Emacs.

;; Installation:
;; If you have permissions to the local `site-lisp' directory, you
;; only have to copy `tuareg.el', `tuareg_indent.el', `ocamldebug.el'
;; and `tuareg-site-file.el'.  Otherwise, copy the previous files
;; to a local directory and add the following line to your `.emacs':
;;
;; (add-to-list 'load-path "DIR")


;;; Usage:
;; Tuareg allows you to run batch OCaml compilations from Emacs (using
;; M-x compile) and browse the errors (C-x `). Typing C-x ` sets the
;; point at the beginning of the erroneous program fragment, and the
;; mark at the end.  Under Emacs, the program fragment is temporarily
;; hilighted.
;;
;; M-x tuareg-run-ocaml (or simply `run-ocaml') starts an OCaml
;; toplevel with input and output in an Emacs buffer named
;; `*ocaml-toplevel*. This gives you the full power of Emacs to edit
;; the input to the OCaml toplevel. This mode is based on comint so
;; you get all the usual comint features, including command history. A
;; hook named `tuareg-interactive-mode-hook' may be used for
;; customization.
;;
;; Typing C-c C-e in a buffer in tuareg mode sends the current phrase
;; (containing the point) to the OCaml toplevel, and evaluates it.  If
;; you type one of these commands before M-x tuareg-run-ocaml, the
;; toplevel will be started automatically.
;;
;; M-x ocamldebug FILE starts the OCaml debugger ocamldebug on the
;; executable FILE, with input and output in an Emacs buffer named
;; *ocamldebug-FILE*.  It is similar to April 1996 version, with minor
;; changes to support XEmacs, Tuareg and OCaml. Furthermore, package
;; `thingatpt' is not required any more.

;; This file is *NOT* part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;;; Code:

(eval-when-compile (require 'cl))
(require 'easymenu)

(defconst tuareg-mode-revision
  (eval-when-compile
    (with-temp-buffer
      (if (file-directory-p ".git")
           (progn
             (insert "git: ")
             (call-process "git" nil t nil "log" "--pretty=%h" "-1")))
      (unless (zerop (buffer-size))
        (buffer-substring-no-properties
         (point-min) (1- (point-max))))))
  "Tuareg revision from the control system used.")

(defconst tuareg-mode-version
  (let ((version "Tuareg Version 2.0.9"))
    (if (null tuareg-mode-revision)
        version
      (concat version " (" tuareg-mode-revision ")")
      ))
  "         Copyright (C) 1997-2006 Albert Cohen, all rights reserved.
         Copyright (C) 2009-2010 Jane Street Holding, LLC.
         Copyright (C) 2011- Stefan Monnier & Christophe Troestler
         Copying is covered by the GNU General Public License.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU General Public License for more details.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      Compatibility functions

(defun tuareg-editing-ls3 ()
  "Tell whether we are editing Lucid Synchrone syntax."
  (string-match "\\.ls\\'" (or buffer-file-name (buffer-name))))

(defun tuareg-editing-ocamllex ()
  "Tell whether we are editing OCamlLex syntax."
  (string-match "\\.mll\\'" (or buffer-file-name (buffer-name))))

(defalias 'tuareg-match-string
  (if (fboundp 'match-string-no-properties)
      'match-string-no-properties
    'match-string))

(or (fboundp 'read-shell-command)
    (defun read-shell-command  (prompt &optional initial-input history)
      "Read a string from the minibuffer, using `shell-command-history'."
      (read-from-minibuffer prompt initial-input nil nil
                            (or history 'shell-command-history))))

(unless (fboundp 'derived-mode-p) ;; in derived.el in emacs21
  (require 'derived))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                    Import types and help features

(defvar tuareg-with-caml-mode-p
  (and (require 'caml-types nil t) (require 'caml-help nil t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       User customizable variables

(require 'smie nil 'noerror)
(defvar tuareg-use-smie (featurep 'smie)
  "Whether to use SMIE as the indentation engine.")

;; Use the standard `customize' interface or `tuareg-mode-hook' to
;; Configure these variables

(require 'custom)

(defgroup tuareg nil
  "Support for the OCaml language."
  :group 'languages)

;; Comments

(defcustom tuareg-leading-star-in-doc nil
  "*Enable automatic intentation of documentation comments of the form
        (**
         * ...
         *)"
  :group 'tuareg :type 'boolean)

(defcustom tuareg-support-leading-star-comments t
  "*Enable automatic intentation of comments of the form
        (*
         * ...
         *)
Documentation comments (** *) are not concerned by this variable
unless `tuareg-leading-star-in-doc' is also set.

If you do not set this variable and still expect comments to be
indented like
        (*
          ...
         *)
\(without leading `*'), set `tuareg-comment-end-extra-indent' to 1."
  :group 'tuareg :type 'boolean)

;; Indentation defaults

(defcustom tuareg-default-indent 2
  "*Default indentation.

Global indentation variable (large values may lead to indentation overflows).
When no governing keyword is found, this value is used to indent the line
if it has to."
  :group 'tuareg :type 'integer)

(defcustom tuareg-support-camllight nil
  "*If true, handle Caml Light character syntax (incompatible with labels)."
  :group 'tuareg :type 'boolean
  :set (lambda (var val)
         (set-default var val)
         (when (boundp 'tuareg-mode-syntax-table)
           (modify-syntax-entry ?` (if val "\"" ".")
                                tuareg-mode-syntax-table))))

(defcustom tuareg-support-metaocaml nil
  "*If true, handle MetaOCaml syntax."
  :group 'tuareg :type 'boolean
  :set (lambda (var val)
         (set-default var val)
         (ignore-errors
           (tuareg-make-indentation-regexps)
           (dolist (buf (buffer-list))
             (with-current-buffer buf
               (when (derived-mode-p 'tuareg-mode 'tuareg-interactive-mode)
                 (tuareg-install-font-lock)))))))

(defcustom tuareg-in-indent 0 ; tuareg-default-indent
  "*How many spaces to indent from a `in' keyword.
Upstream <http://caml.inria.fr/resources/doc/guides/guidelines.en.html>
recommends 0, and this is what we default to since 2.0.1
instead of the historical `tuareg-default-indent'."
  :group 'tuareg :type 'integer)

(defcustom tuareg-with-indent 0
  "*How many spaces to indent from a `with' keyword.
The examples at <http://caml.inria.fr/resources/doc/guides/guidelines.en.html>
show the '|' is aligned with 'match', thus 0 is the default value."
  :group 'tuareg :type 'integer)

(defcustom tuareg-match-clause-indent 1
  "*How many spaces to indent a clause after a pattern match `| ... ->'.
To respect <http://caml.inria.fr/resources/doc/guides/guidelines.en.html>
the default is 1.")

(defcustom tuareg-match-when-indent (+ 4 tuareg-match-clause-indent)
  "*How many spaces from `|' to indent `when' in a pattern match
   | patt
        when cond ->
      clause")

;; Automatic indentation
;; Using abbrev-mode and electric keys

(defcustom tuareg-use-abbrev-mode nil
  "*Non-nil means electrically indent lines starting with leading keywords.
Leading keywords are such as `end', `done', `else' etc.
It makes use of `abbrev-mode'.

Many people find electric keywords irritating, so you can disable them by
setting this variable to nil."
  :group 'tuareg :type 'boolean
  :set (lambda (var val)
         (set-default var val)
         (dolist (buf (buffer-list))
           (with-current-buffer buf
             (when (derived-mode-p 'tuareg-mode)
               (abbrev-mode (if val 1 -1)))))))

(defcustom tuareg-electric-indent t
  "*Non-nil means electrically indent lines starting with `|', `)', `]' or `}'.

Many people find electric keys irritating, so you can disable them by
setting this variable to nil."
  :group 'tuareg :type 'boolean)
(when (fboundp 'electric-indent-mode)
  (make-obsolete-variable 'tuareg-electric-indent
                          'electric-indent-mode "Emacs-24.1"))

;; Tuareg-Interactive
;; Configure via `tuareg-mode-hook'

(defcustom tuareg-interactive-scroll-to-bottom-on-output nil
  "*Controls when to scroll to the bottom of the interactive buffer
upon evaluating an expression.

See `comint-scroll-to-bottom-on-output' for details."
  :group 'tuareg :type 'boolean
  :set (lambda (var val)
         (set-default var val)
         (when (boundp 'comint-scroll-to-bottom-on-output)
           (dolist (buf (buffer-list))
             (with-current-buffer buf
               (when (derived-mode-p 'tuareg-interactive-mode)
                 (set (make-local-variable 'comint-scroll-to-bottom-on-output)
                      val)))))))

(defcustom tuareg-skip-after-eval-phrase t
  "*Non-nil means skip to the end of the phrase after evaluation in the
Caml toplevel."
  :group 'tuareg :type 'boolean)

(defcustom tuareg-interactive-read-only-input nil
  "*Non-nil means input sent to the OCaml toplevel is read-only."
  :group 'tuareg :type 'boolean)

(defcustom tuareg-interactive-echo-phrase t
  "*Non-nil means echo phrases in the toplevel buffer when sending
them to the OCaml toplevel."
  :group 'tuareg :type 'boolean)

(defcustom tuareg-interactive-input-font-lock t
  "*Non nil means Font-Lock for toplevel input phrases."
  :group 'tuareg :type 'boolean)

(defcustom tuareg-interactive-output-font-lock t
  "*Non nil means Font-Lock for toplevel output messages."
  :group 'tuareg :type 'boolean)

(defcustom tuareg-interactive-error-font-lock t
  "*Non nil means Font-Lock for toplevel error messages."
  :group 'tuareg :type 'boolean)

(defcustom tuareg-display-buffer-on-eval t
  "*Non nil means pop up the OCaml toplevel when evaluating code."
  :group 'tuareg :type 'boolean)

(defcustom tuareg-manual-url
  "http://caml.inria.fr/pub/docs/manual-ocaml/"
  "*URL to the OCaml reference manual."
  :group 'tuareg :type 'string)

(defcustom tuareg-browser 'browse-url
  "*Name of function that displays the OCaml reference manual.
Valid names are `browse-url', `browse-url-firefox', etc."
  :group 'tuareg)

(defcustom tuareg-library-path "/usr/local/lib/ocaml/"
  "*Path to the OCaml library."
  :group 'tuareg :type 'string)

(defcustom tuareg-definitions-max-items 30
  "*Maximum number of items a definitions menu can contain."
  :group 'tuareg :type 'integer)

(defvar tuareg-options-list
  `(("Automatic indentation of leading keywords" . 'tuareg-use-abbrev-mode)
    ("Automatic indentation of ), ] and }" . 'tuareg-electric-indent)
    ,@(unless tuareg-use-smie
        '(("Automatic matching of [| and {<" . 'tuareg-electric-close-vector)))
    "---"
    ,@(unless tuareg-use-smie
        '(("Indent body of comments" . 'tuareg-indent-comments)
          ("Indent first line of comments" . 'tuareg-indent-leading-comments)
          ("Leading-`*' comment style" . 'tuareg-support-leading-star-comments)
          )))
  "*List of menu-configurable Tuareg options.")

(defvar tuareg-interactive-options-list
  '(("Skip phrase after evaluation" . 'tuareg-skip-after-eval-phrase)
    ("Echo phrase in interactive buffer" . 'tuareg-interactive-echo-phrase)
    "---"
    ("Font-lock interactive input" . 'tuareg-interactive-input-font-lock)
    ("Font-lock interactive output" . 'tuareg-interactive-output-font-lock)
    ("Font-lock interactive error" . 'tuareg-interactive-error-font-lock)
    "---"
    ("Read only input" . 'tuareg-interactive-read-only-input))
  "*List of menu-configurable Tuareg options.")

(defvar tuareg-interactive-program "ocaml"
  "*Default program name for invoking an OCaml toplevel from Emacs.")
;; Could be interesting to have this variable buffer-local
;;   (e.g., ocaml vs. metaocaml buffers)
;; (make-variable-buffer-local 'tuareg-interactive-program)

(eval-and-compile
  (defconst tuareg-use-syntax-ppss (fboundp 'syntax-ppss)
    "*If nil, use our own parsing and caching."))

(defgroup tuareg-faces nil
  "Special faces for the Tuareg mode."
  :group 'tuareg)

(defconst tuareg-faces-inherit-p
  (and (boundp 'face-attribute-name-alist)
       (assq :inherit face-attribute-name-alist)))

(defface tuareg-font-lock-governing-face
  '((((class color) (type tty)) (:bold t))
    (((background light)) (:foreground "black" :bold t))
    (t (:foreground "wheat" :bold t)))
  "Face description for governing/leading keywords."
  :group 'tuareg-faces)
(defvar tuareg-font-lock-governing-face
  'tuareg-font-lock-governing-face)

(defface tuareg-font-lock-multistage-face
  '((((background light))
     (:foreground "darkblue" :background "lightgray" :bold t))
    (t (:foreground "steelblue" :background "darkgray" :bold t)))
  "Face description for MetaOCaml staging operators."
  :group 'tuareg-faces)
(defvar tuareg-font-lock-multistage-face
  'tuareg-font-lock-multistage-face)

(defface tuareg-font-lock-line-number-face
  '((((background light)) (:foreground "dark gray"))
    (t (:foreground "gray60")))
  "Face description for line numbering directives."
  :group 'tuareg-faces)
(defvar tuareg-font-lock-line-number-face
  'tuareg-font-lock-line-number-face)

(defface tuareg-font-lock-operator-face
  '((((background light)) (:foreground "brown"))
    (t (:foreground "khaki")))
  "Face description for all operators."
  :group 'tuareg-faces)
(defvar tuareg-font-lock-operator-face
  'tuareg-font-lock-operator-face)

(defface tuareg-font-lock-module-face
  '((t (:inherit font-lock-type-face))); backward compatibility
  "Face description for modules and module paths."
  :group 'tuareg-faces)
(defvar tuareg-font-lock-module-face
  'tuareg-font-lock-module-face)

(defface tuareg-font-lock-constructor-face
  '((t (:inherit default)))
  "Face description for constructors of (polymorphic) variants and exceptions."
  :group 'tuareg-faces)
(defvar tuareg-font-lock-constructor-face
  'tuareg-font-lock-constructor-face)

(defface tuareg-font-lock-error-face
  '((t (:foreground "yellow" :background "red" :bold t)))
  "Face description for all errors reported to the source."
  :group 'tuareg-faces)
(defvar tuareg-font-lock-error-face
  'tuareg-font-lock-error-face)

(defface tuareg-font-lock-interactive-output-face
  '((((background light))
     (:foreground "blue4"))
    (t (:foreground "grey")))
  "Face description for all toplevel outputs."
  :group 'tuareg-faces)
(defvar tuareg-font-lock-interactive-output-face
  'tuareg-font-lock-interactive-output-face)

(defface tuareg-font-lock-interactive-error-face
  (if tuareg-faces-inherit-p
      '((t :inherit font-lock-warning-face))
    '((((background light)) (:foreground "red3"))
      (t (:foreground "red2"))))
  "Face description for all toplevel errors."
  :group 'tuareg-faces)
(defvar tuareg-font-lock-interactive-error-face
  'tuareg-font-lock-interactive-error-face)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            Support definitions

(defun tuareg-leading-star-p ()
  (and tuareg-support-leading-star-comments
       (save-excursion ; this function does not make sense outside of a comment
         (tuareg-beginning-of-literal-or-comment)
         (and (or tuareg-leading-star-in-doc
                  (not (looking-at "(\\*[Tt][Ee][Xx]\\|(\\*\\*")))
              (progn
                (forward-line 1)
                (back-to-indentation)
                (looking-at "\\*[^)]"))))))

(defun tuareg-auto-fill-insert-leading-star (&optional leading-star)
  (let ((point-leading-comment (looking-at "(\\*")) (return-leading nil))
    (save-excursion
      (back-to-indentation)
      (when tuareg-electric-indent
        (when (and (tuareg-in-comment-p)
                   (or leading-star
                       (tuareg-leading-star-p)))
          (unless (looking-at "(?\\*")
            (insert-before-markers "* "))
          (setq return-leading t))
        (unless point-leading-comment
          ;; Use optional argument to break recursion
          (tuareg-indent-command t))))
    return-leading))

(unless (fboundp 'comment-normalize-vars)
  (defun tuareg-auto-fill-function ()
    (unless (tuareg-in-literal-p)
      (let ((leading-star
             (and (not (char-equal ?\n last-command-event))
                  (tuareg-auto-fill-insert-leading-star))))
        (do-auto-fill)
        (unless (char-equal ?\n last-command-event)
          (tuareg-auto-fill-insert-leading-star leading-star))))))

;; these two functions are different from the standard
;; in that they do NOT signal errors beginning-of-buffer and end-of-buffer
(defun tuareg-forward-char (&optional step)
  (if step (goto-char (+ (point) step))
    (goto-char (1+ (point)))))

(defun tuareg-backward-char (&optional step)
  (if step (goto-char (- (point) step))
    (goto-char (1- (point)))))

(defun tuareg-in-indentation-p ()
  "Return non-nil if all chars between beginning of line and point are blanks."
  (save-excursion
    (skip-chars-backward " \t")
    (bolp)))

(defvar tuareg-cache-stop (point-min))
(make-variable-buffer-local 'tuareg-cache-stop)
(defvar tuareg-cache nil)
(make-variable-buffer-local 'tuareg-cache)
(defvar tuareg-cache-local nil)
(make-variable-buffer-local 'tuareg-cache-local)
(defvar tuareg-cache-last-local nil)
(make-variable-buffer-local 'tuareg-cache-last-local)
(defvar tuareg-last-loc (cons nil nil))

;; PPSS definitions
(defun tuareg-ppss-in-literal-or-comment () (error "tuareg uses PPSS"))
(defun tuareg-ppss-fontify (_beg _end) (error "tuareg uses PPSS"))
(defun tuareg-ppss-in-literal-p ()
  "Return non-nil if point is inside an OCaml literal."
  (nth 3 (syntax-ppss)))
(defun tuareg-ppss-in-comment-p ()
  "Return non-nil if point is inside or right before an OCaml comment."
  (or (nth 4 (syntax-ppss))
      (looking-at "[ \t]*(\\*")))
(defun tuareg-ppss-in-literal-or-comment-p ()
  "Return non-nil if point is inside an OCaml literal or comment."
  (nth 8 (syntax-ppss)))
(defun tuareg-ppss-beginning-of-literal-or-comment ()
  "Skips to the beginning of the current literal or comment (or buffer)."
  (interactive)
  (goto-char (or (nth 8 (syntax-ppss)) (point))))
(defun tuareg-ppss-beginning-of-literal-or-comment-fast ()
  (goto-char (or (nth 8 (syntax-ppss)) (point-min))))

;; non-PPSS definitions
(defun tuareg-!ppss-in-literal-p ()
  "Return non-nil if point is inside an OCaml literal."
  (car (tuareg-in-literal-or-comment)))
(defun tuareg-!ppss-in-comment-p ()
  "Return non-nil if point is inside an OCaml comment."
  (cdr (tuareg-in-literal-or-comment)))
(defun tuareg-!ppss-in-literal-or-comment-p ()
  "Return non-nil if point is inside an OCaml literal or comment."
  (tuareg-in-literal-or-comment)
  (or (car tuareg-last-loc) (cdr tuareg-last-loc)))
(defun tuareg-!ppss-in-literal-or-comment ()
  "Return the pair `((tuareg-in-literal-p) . (tuareg-in-comment-p))'."
  (if (and (<= (point) tuareg-cache-stop) tuareg-cache)
      (progn
        (if (or (not tuareg-cache-local) (not tuareg-cache-last-local)
                (and (>= (point) (caar tuareg-cache-last-local))))
            (setq tuareg-cache-local tuareg-cache))
        (while (and tuareg-cache-local (< (point) (caar tuareg-cache-local)))
          (setq tuareg-cache-last-local tuareg-cache-local
                tuareg-cache-local (cdr tuareg-cache-local)))
        (setq tuareg-last-loc
              (if tuareg-cache-local
                  (cons (eq (cadar tuareg-cache-local) 'b)
                        (> (cddar tuareg-cache-local) 0))
                  (cons nil nil))))
    (let ((flag t) (op (point)) (mp (min (point) (1- (point-max))))
          (balance 0) (end-of-comment nil))
      (while (and tuareg-cache (<= tuareg-cache-stop (caar tuareg-cache)))
        (setq tuareg-cache (cdr tuareg-cache)))
      (if tuareg-cache
          (if (eq (cadar tuareg-cache) 'b)
              (progn
                (setq tuareg-cache-stop (1- (caar tuareg-cache)))
                (goto-char tuareg-cache-stop)
                (setq balance (cddar tuareg-cache))
                (setq tuareg-cache (cdr tuareg-cache)))
            (setq balance (cddar tuareg-cache))
            (setq tuareg-cache-stop (caar tuareg-cache))
            (goto-char tuareg-cache-stop)
            (skip-chars-forward "("))
          (goto-char (point-min)))
      (skip-chars-backward "\\\\*")
      (while flag
        (if end-of-comment (setq balance 0 end-of-comment nil))
        (skip-chars-forward "^\\\\'`\"(\\*")
        (cond
          ((looking-at "\\\\")
           (tuareg-forward-char 2))
          ((looking-at "'\\([^\n\\']\\|\\\\[^ \t\n][^ \t\n]?[^ \t\n]?\\)'")
           (setq tuareg-cache (cons (cons (1+ (point)) (cons 'b balance))
                                    tuareg-cache))
           (goto-char (match-end 0))
           (setq tuareg-cache (cons (cons (point) (cons 'e balance))
                                    tuareg-cache)))
          ((and
            tuareg-support-camllight
            (looking-at "`\\([^\n\\']\\|\\\\[^ \t\n][^ \t\n]?[^ \t\n]?\\)`"))
           (setq tuareg-cache (cons (cons (1+ (point)) (cons 'b balance))
                                    tuareg-cache))
           (goto-char (match-end 0))
           (setq tuareg-cache (cons (cons (point) (cons 'e balance))
                                    tuareg-cache)))
          ((looking-at "\"")
           (tuareg-forward-char)
           (setq tuareg-cache (cons (cons (point) (cons 'b balance))
                                    tuareg-cache))
           (skip-chars-forward "^\\\\\"")
           (while (looking-at "\\\\")
             (tuareg-forward-char 2) (skip-chars-forward "^\\\\\""))
           (tuareg-forward-char)
           (setq tuareg-cache (cons (cons (point) (cons 'e balance))
                                    tuareg-cache)))
          ((looking-at "(\\*")
           (setq balance (1+ balance))
           (setq tuareg-cache (cons (cons (point) (cons nil balance))
                                    tuareg-cache))
           (tuareg-forward-char 2))
          ((looking-at "\\*)")
           (tuareg-forward-char 2)
           (if (> balance 1)
               (progn
                 (setq balance (1- balance))
                 (setq tuareg-cache (cons (cons (point) (cons nil balance))
                                          tuareg-cache)))
               (setq end-of-comment t)
               (setq tuareg-cache (cons (cons (point) (cons nil 0))
                                        tuareg-cache))))
          (t (tuareg-forward-char)))
        (setq flag (<= (point) mp)))
      (setq tuareg-cache-local tuareg-cache
            tuareg-cache-stop (point))
      (goto-char op)
      (if tuareg-cache (tuareg-in-literal-or-comment)
          (setq tuareg-last-loc (cons nil nil))
          tuareg-last-loc))))
(defun tuareg-!ppss-beginning-of-literal-or-comment ()
  "Skips to the beginning of the current literal or comment (or buffer)."
  (interactive)
  (when (tuareg-in-literal-or-comment-p)
    (tuareg-beginning-of-literal-or-comment-fast)))

(defun tuareg-!ppss-beginning-of-literal-or-comment-fast ()
  (while (and tuareg-cache-local
              (or (eq 'b (cadar tuareg-cache-local))
                  (> (cddar tuareg-cache-local) 0)))
    (setq tuareg-cache-last-local tuareg-cache-local
          tuareg-cache-local (cdr tuareg-cache-local)))
  (if tuareg-cache-last-local
      (goto-char (caar tuareg-cache-last-local))
    (goto-char (point-min)))
  (when (eq 'b (cadar tuareg-cache-last-local)) (tuareg-backward-char)))

(defun tuareg-!ppss-backward-up-list ()
  "Safe up-list regarding comments, literals and errors."
  (let ((balance 1) (op (point)) (oc nil))
    (tuareg-in-literal-or-comment)
    (while (and (> (point) (point-min)) (> balance 0))
      (setq oc (if tuareg-cache-local (caar tuareg-cache-local) (point-min)))
      (condition-case nil (up-list -1) (error (goto-char (point-min))))
      (if (>= (point) oc) (setq balance (1- balance))
        (goto-char op)
        (skip-chars-backward "^[]{}()") (tuareg-backward-char)
        (cond ((tuareg-in-literal-or-comment-p)
               (tuareg-beginning-of-literal-or-comment-fast))
              ((looking-at "[[{(]")
               (setq balance (1- balance)))
              ((looking-at "[]})]")
               (setq balance (1+ balance)))))
      (setq op (point)))))

(defalias 'tuareg-in-literal-or-comment
  ;; FIXME: These eval-and-compile have no effect.  Maybe eval-when-compile
  ;; was intended?
  (eval-and-compile (if tuareg-use-syntax-ppss
                        'tuareg-ppss-in-literal-or-comment
                      'tuareg-!ppss-in-literal-or-comment)))
(defalias 'tuareg-fontify
    (eval-and-compile (if tuareg-use-syntax-ppss
                          'tuareg-ppss-fontify
                          'tuareg-!ppss-fontify)))
(defalias 'tuareg-in-literal-p
    (eval-and-compile (if tuareg-use-syntax-ppss
                          'tuareg-ppss-in-literal-p
                          'tuareg-!ppss-in-literal-p)))
(defalias 'tuareg-in-comment-p
    (eval-and-compile (if tuareg-use-syntax-ppss
                          'tuareg-ppss-in-comment-p
                          'tuareg-!ppss-in-comment-p)))
(defalias 'tuareg-in-literal-or-comment-p
    (eval-and-compile (if tuareg-use-syntax-ppss
                          'tuareg-ppss-in-literal-or-comment-p
                          'tuareg-!ppss-in-literal-or-comment-p)))
(defalias 'tuareg-beginning-of-literal-or-comment
    (eval-and-compile (if tuareg-use-syntax-ppss
                          'tuareg-ppss-beginning-of-literal-or-comment
                          'tuareg-!ppss-beginning-of-literal-or-comment)))
(defalias 'tuareg-beginning-of-literal-or-comment-fast
    (eval-and-compile (if tuareg-use-syntax-ppss
                          'tuareg-ppss-beginning-of-literal-or-comment-fast
                          'tuareg-!ppss-beginning-of-literal-or-comment-fast)))
(defalias 'tuareg-backward-up-list
    ;; FIXME: not clear if moving out of a string/comment counts as 1 or no.
    (eval-and-compile (if tuareg-use-syntax-ppss
			  (lambda ()
			    (condition-case nil
				(backward-up-list)
			      (scan-error (goto-char (point-min)))))
                          'tuareg-!ppss-backward-up-list)))

(defun tuareg-false-=-p ()
  "Is the underlying `=' the first/second letter of an operator?"
  (or (memq (preceding-char) '(?: ?> ?< ?=))
      (char-equal ?= (char-after (1+ (point))))))

(defun tuareg-at-phrase-break-p ()
  "Is the underlying `;' a phrase break?"
  (and (char-equal ?\; (following-char))
       (or (and (not (eobp))
                (char-equal ?\; (char-after (1+ (point)))))
           (char-equal ?\; (preceding-char)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           Font-lock in Emacs

;; Originally by Stefan Monnier

(defcustom tuareg-font-lock-symbols nil
  "*Display fun and -> and such using symbols in fonts.
This may sound like a neat trick, but note that it can change the
alignment and can thus lead to surprises."
  :group 'tuareg :type 'boolean)
(when (fboundp 'prettify-symbols-mode)
  (make-obsolete-variable 'tuareg-font-lock-symbols
                          'prettify-symbols-mode "Emacs-24.4"))

(defvar tuareg-font-lock-symbols-alist
  (cond ((fboundp 'decode-char) ;; or a unicode font.
         `(("fun" . ,(decode-char 'ucs 955))
           ("sqrt" . ,(decode-char 'ucs 8730))
           ("not" . ,(decode-char 'ucs 172))
           ("&&" . ,(decode-char 'ucs 8743)); 'LOGICAL AND' (U+2227)
           ("or" . ,(decode-char 'ucs 8744)); 'LOGICAL OR' (U+2228)
           ("||" . ,(decode-char 'ucs 8744))
           ("[|" . ,(decode-char 'ucs 12314)) ;; 〚
           ("|]" . ,(decode-char 'ucs 12315)) ;; 〛
           ("*." . ,(decode-char 'ucs 215))
           ("/." . ,(decode-char 'ucs 247))
           ("->" . ,(decode-char 'ucs 8594))
           ("<-" . ,(decode-char 'ucs 8592))
           ("<=" . ,(decode-char 'ucs 8804))
           (">=" . ,(decode-char 'ucs 8805))
           ("<>" . ,(decode-char 'ucs 8800))
           ("==" . ,(decode-char 'ucs 8801))
           ("!=" . ,(decode-char 'ucs 8802))
           ("<=>" . ,(decode-char 'ucs 8660))
           (":=" . ,(decode-char 'ucs 8656))
           ("infinity" . ,(decode-char 'ucs 8734))
           ;; Some greek letters for type parameters.
           ("'a" . ,(decode-char 'ucs 945))
           ("'b" . ,(decode-char 'ucs 946))
           ("'c" . ,(decode-char 'ucs 947))
           ("'d" . ,(decode-char 'ucs 948))
           ("'e" . ,(decode-char 'ucs 949))
           ("'f" . ,(decode-char 'ucs 966))
           ("'i" . ,(decode-char 'ucs 953))
           ("'k" . ,(decode-char 'ucs 954))
           ("'m" . ,(decode-char 'ucs 956))
           ("'n" . ,(decode-char 'ucs 957))
           ("'o" . ,(decode-char 'ucs 969))
           ("'p" . ,(decode-char 'ucs 960))
           ("'r" . ,(decode-char 'ucs 961))
           ("'s" . ,(decode-char 'ucs 963))
           ("'t" . ,(decode-char 'ucs 964))
           ("'x" . ,(decode-char 'ucs 958))))
        ((and (fboundp 'make-char) (fboundp 'charsetp) (charsetp 'symbol))
         `(("fun" . ,(make-char 'symbol 108))
           ("sqrt" . ,(make-char 'symbol 214))
           ("not" . ,(make-char 'symbol 216))
           ("&&" . ,(make-char 'symbol 217))
           ("or" . ,(make-char 'symbol 218))
           ("||" . ,(make-char 'symbol 218))
           ("*." . ,(make-char 'symbol 183))
           ("/." . ,(make-char 'symbol 184))
           ("<=" . ,(make-char 'symbol 163))
           ("<-" . ,(make-char 'symbol 172))
           ("->" . ,(make-char 'symbol 174))
           (">=" . ,(make-char 'symbol 179))
           ("<>" . ,(make-char 'symbol 185))
           ("==" . ,(make-char 'symbol 186))
           ("<=>" . ,(make-char 'symbol 219))
           (":=" . ,(make-char 'symbol 220))
           ("=>" . ,(make-char 'symbol 222))
           ("infinity" . ,(make-char 'symbol 165))
           ;; Some greek letters for type parameters.
           ("'a" . ,(make-char 'symbol 97))
           ("'b" . ,(make-char 'symbol 98))
           ("'c" . ,(make-char 'symbol 103)) ; sic! 99 is chi, 103 is gamma
           ("'d" . ,(make-char 'symbol 100))
           ("'e" . ,(make-char 'symbol 101))
           ("'f" . ,(make-char 'symbol 102))
           ("'i" . ,(make-char 'symbol 105))
           ("'k" . ,(make-char 'symbol 107))
           ("'m" . ,(make-char 'symbol 109))
           ("'n" . ,(make-char 'symbol 110))
           ("'o" . ,(make-char 'symbol 111))
           ("'p" . ,(make-char 'symbol 112))
           ("'r" . ,(make-char 'symbol 114))
           ("'s" . ,(make-char 'symbol 115))
           ("'t" . ,(make-char 'symbol 116))
           ("'x" . ,(make-char 'symbol 120))))))

(defun tuareg-font-lock-compose-symbol (alist)
  "Compose a sequence of ascii chars into a symbol.
Regexp match data 0 points to the chars."
  ;; Check that the chars should really be composed into a symbol.
  (let* ((mbegin (match-beginning 0))
         (mend (match-end 0))
         (syntax (char-syntax (char-after mbegin))))
    (if (or (eq (char-syntax (or (char-before mbegin) ?\ )) syntax)
            (eq (char-syntax (or (char-after mend) ?\ )) syntax)
            (memq (get-text-property mbegin 'face)
                  '(tuareg-doc-face font-lock-string-face
                    font-lock-comment-face)))
        ;; No composition for you. Let's actually remove any composition
        ;;   we may have added earlier and which is now incorrect.
        (remove-text-properties mbegin mend '(composition))
      ;; That's a symbol alright, so add the composition.
      (compose-region mbegin mend (cdr (assoc (match-string 0) alist)))))
  ;; Return nil because we're not adding any face property.
  nil)

(defun tuareg-font-lock-symbols-keywords ()
  (when (fboundp 'compose-region)
    (let ((alist nil))
      (dolist (x tuareg-font-lock-symbols-alist)
        (when (and (if (fboundp 'char-displayable-p)
                       (char-displayable-p (cdr x))
                     t)
                   (not (assoc (car x) alist))) ; not yet in alist.
          (push x alist)))
      (when alist
        `((,(regexp-opt (mapcar 'car alist) t)
           (0 (tuareg-font-lock-compose-symbol ',alist))))))))

(defvar tuareg-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "_" st)
    (modify-syntax-entry ?. "'" st)     ;Make qualified names a single symbol.
    (modify-syntax-entry ?# "_" st)     ;Make name#method a single symbol
    (modify-syntax-entry ?? ". p" st)
    (modify-syntax-entry ?~ ". p" st)
    ;; See http://caml.inria.fr/pub/docs/manual-ocaml/lex.html.
    (dolist (c '(?! ?$ ?% ?& ?+ ?- ?/ ?: ?< ?= ?> ?@ ?^ ?|))
      (modify-syntax-entry c "." st))
    (modify-syntax-entry ?' "_" st) ; ' is part of symbols (for primes).
    (modify-syntax-entry
     ;; ` is punctuation or character delimiter (Caml Light compatibility).
     ?` (if tuareg-support-camllight "\"" ".") st)
    (modify-syntax-entry ?\" "\"" st) ; " is a string delimiter
    (modify-syntax-entry ?\\ "\\" st)
    (modify-syntax-entry ?*  ". 23" st)
    (condition-case nil
        (progn
          (modify-syntax-entry ?\( "()1n" st)
          (modify-syntax-entry ?\) ")(4n" st))
      (error               ;XEmacs signals an error instead of ignoring `n'.
       (modify-syntax-entry ?\( "()1" st)
       (modify-syntax-entry ?\) ")(4" st)))
    st)
  "Syntax table in use in Tuareg mode buffers.")

(defmacro tuareg-with-internal-syntax (&rest body)
  `(progn
     ;; Switch to a modified internal syntax.
     (modify-syntax-entry ?. "w" tuareg-mode-syntax-table)
     (modify-syntax-entry ?' "w" tuareg-mode-syntax-table)
     (modify-syntax-entry ?_ "w" tuareg-mode-syntax-table)
     (unwind-protect (progn ,@body)
       ;; Switch back to the interactive syntax.
       (modify-syntax-entry ?. "'" tuareg-mode-syntax-table)
       (modify-syntax-entry ?' "_" tuareg-mode-syntax-table)
       (modify-syntax-entry ?_ "_" tuareg-mode-syntax-table))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  Font-Lock

(defvar tuareg-doc-face 'font-lock-doc-face)

(unless tuareg-use-syntax-ppss

  (defun tuareg-fontify-buffer ()
    (font-lock-default-fontify-buffer)
    (tuareg-fontify (point-min) (point-max)))

  (defun tuareg-fontify-region (begin end &optional verbose)
    (font-lock-default-fontify-region begin end verbose)
    (tuareg-fontify begin end))

  (defun tuareg-fontify (begin end)
    (when (eq major-mode 'tuareg-mode)
      (save-excursion
       (tuareg-with-internal-syntax

        (let ((case-fold-search nil)
              (modified (buffer-modified-p))) ; Emacs hack (see below)
          (goto-char begin)
          (setq begin (line-beginning-position))
          (goto-char (1- end))
          (end-of-line)
          ;; Dirty hack to trick `font-lock-default-unfontify-region'
          (forward-line 2)
          (setq end (point))

          (while (> end begin)
            (goto-char (1- end))
            (tuareg-in-literal-or-comment)
            (cond
              ((cdr tuareg-last-loc)
               (tuareg-beginning-of-literal-or-comment)
               (put-text-property (max begin (point)) end 'face
                                  (if (looking-at
                                       "(\\*[Tt][Ee][Xx]\\|(\\*\\*[^*]")
                                      tuareg-doc-face
                                      'font-lock-comment-face))
               (setq end (1- (point))))
              ((car tuareg-last-loc)
               (tuareg-beginning-of-literal-or-comment)
               (put-text-property (max begin (point)) end 'face
                                  'font-lock-string-face)
               (setq end (point)))
              (t (while (and tuareg-cache-local
                             (or (> (caar tuareg-cache-local) end)
                                 (eq 'b (cadar tuareg-cache-local))))
                   (setq tuareg-cache-local (cdr tuareg-cache-local)))
                 (setq end (if tuareg-cache-local
                               (caar tuareg-cache-local) begin)))))
          (unless modified (set-buffer-modified-p nil)))
        ))))
  ) ;; end tuareg-use-syntax-ppss

(defconst tuareg-font-lock-syntactic-keywords
  ;; Char constants start with ' but ' can also appear in identifiers.
  ;; Beware not to match things like '*)hel' or '"hel' since the first '
  ;; might be inside a string or comment.
  ;; Note: for compatibility with Emacs<23, we use "\\<" rather than "\\_<",
  ;; which depends on tuareg-font-lock-syntax turning all "_" into "w".
  '(("\\<\\('\\)\\([^'\\\n]\\|\\\\.[^\\'\n \")]*\\)\\('\\)"
     (1 '(7)) (3 '(7)))))

(defvar syntax-propertize-function)
(when (eval-when-compile (fboundp 'syntax-propertize-rules))
  (defun tuareg-syntax-propertize (start end)
    (goto-char start)
    (tuareg--syntax-quotation end)
    (funcall
     (syntax-propertize-rules
      ;; When we see a '"', knowing whether it's a literal char (as opposed to
      ;; the end of a string followed by the beginning of a literal char)
      ;; requires checking syntax-ppss as in:
      ;; ("\\_<\\('\"'\\)"
      ;;  (1 (unless (nth 3 (save-excursion (syntax-ppss (match-beginning 0))))
      ;;       (string-to-syntax "\""))))
      ;; Not sure if it's worth the trouble since adding a space between the
      ;; string and the literal char is easy enough and is the usual
      ;; style anyway.
      ;; For all other cases we don't need to check syntax-ppss because, if the
      ;; first quote is within a string (or comment), the whole match is within
      ;; the string (or comment), so the syntax-properties don't hurt.
      ;;
      ;; Note: we can't just use "\\<" here because syntax-propertize is also
      ;; used outside of font-lock.
      ("\\_<\\('\\)\\(?:[^'\\\n]\\|\\\\.[^\\'\n \")]*\\)\\('\\)"
       (1 "\"") (2 "\""))
      ("\\(<\\)\\(?:<\\S.\\|:[[:alpha:]]+<\\)"
       (1 (prog1 "|" (tuareg--syntax-quotation end))))
      ("\\({\\)[a-z_]*|"
       (1 (prog1 "|" (tuareg--syntax-quotation end))))
      )
     start end)))

(defun tuareg--syntax-quotation (end)
  (let ((ppss (syntax-ppss)))
    (when (eq t (nth 3 ppss))
      (ecase (char-after (nth 8 ppss))
        (?<
         ;; We're indeed inside a quotation.
         (when (re-search-forward ">>" end t)
           (put-text-property (1- (point)) (point)
                              'syntax-table (string-to-syntax "|"))))
        (?\{
         ;; We're inside a quoted string
         ;; http://caml.inria.fr/pub/docs/manual-ocaml/extn.html#sec244
         (let ((id (save-excursion
                     (goto-char (1+ (nth 8 ppss)))
                     (buffer-substring (point)
                                       (progn (skip-chars-forward "a-z_")
                                              (point))))))
           (when (search-forward (concat "|" id "}") end t)
             (put-text-property (1- (point)) (point)
                                'syntax-table (string-to-syntax "|")))))))))

(defun tuareg-font-lock-syntactic-face-function (state)
  (if (nth 3 state)
      (if (and (eq t (nth 3 state)) (eq ?< (char-after (nth 8 state))))
          font-lock-preprocessor-face font-lock-string-face)
    (let ((start (nth 8 state)))
      (if (and (> (point-max) (+ start 2))
               (eq (char-after (+ start 2)) ?*)
               (not (eq (char-after (+ start 3)) ?*)))
          ;; This is a documentation comment
          tuareg-doc-face
        font-lock-comment-face))))

;; Initially empty, set in `tuareg-install-font-lock'
(defvar tuareg-font-lock-keywords ()
  "Font-Lock patterns for Tuareg mode.")

(defconst tuareg-font-lock-syntax
  ;; Note: as a general rule, changing syntax-table during font-lock
  ;; is a potential problem for syntax-ppss.
  `((?_ . "w") (?' . "w")
    ,@(unless tuareg-use-syntax-ppss
        '((?` . ".") (?\" . ".") (?\( . ".") (?\) . ".") (?* . "."))))
  "Syntax changes for Font-Lock.")

(defconst tuareg--whitespace-re
  ;; FIXME: Why's not just "[ \t\n]*"?
  ;; It used to be " *[\t\n]? *" but this is inefficient since it can match
  ;; N spaces in N+1 different ways :-(
  " *\\(?:[\t\n] *\\)?")

(defun tuareg-install-font-lock ()
  (let* ((id "\\<[A-Za-z_][A-Za-z0-9_']*\\>")
         (lid "\\<[a-z_][A-Za-z0-9_']*\\>")
         (uid "\\<[A-Z][A-Za-z0-9_']*\\>")
         ;; Matches corresponding braces for 3 levels.
         (balanced-braces ; needs a closing brace
          (let ((b "\\(?:[^()]\\|(")
                (e ")\\)*"))
            (concat b b b "[^()]*" e e e)))
         (balanced-braces-no-string
          (let ((b "\\(?:[^()\"]\\|(")
                (e ")\\)*"))
            (concat b b b "[^()\"]*" e e e)))
         (unbraced-tuple (concat lid " *\\(?:, *" lid " *\\)*"))
         (tuple (concat "(" balanced-braces ")")); much more than tuple!
         (module-path (concat uid "\\(?:\\." uid "\\)*"))
         (typeconstr (concat "\\(?:" module-path "\\.\\)?" lid))
         (constructor (concat "\\(?:\\(?:" module-path "\\.\\)?" uid
                              "\\|`" id "\\)"))
         (extended-module-name
          (concat uid "\\(?: *([ A-Z]" balanced-braces ")\\)*"))
         (extended-module-path
          (concat extended-module-name
                  "\\(?: *\\. *" extended-module-name "\\)*"))
         (modtype-path (concat "\\(?:" extended-module-path "\\.\\)*" id))
         (typevar "'[A-Za-z_][A-Za-z0-9_']*\\>")
         (typeparam (concat "[+-]?" typevar))
         (typeparams (concat "\\(?:" typeparam "\\|( *"
                             typeparam " *\\(?:, *" typeparam " *\\)*)\\)"))
         (typedef (concat "\\(?:" typeparams " *\\)?" lid))
         ;; Define 2 groups: possible path, variables
         (let-ls3 (regexp-opt '("clock" "node" "static"
                                "present" "automaton" "where" "match"
                                "with" "do" "done" "unless" "until"
                                "reset" "every")))
         (let-binding (concat "\\<\\(?:let\\(?: +"
                              (if (tuareg-editing-ls3) let-ls3 "rec")
                              "\\)?\\|and\\)\\>"))
         ;; group of variables
         (gvars (concat "\\(\\(?:" tuareg--whitespace-re
                        "\\(?:" lid "\\|()\\|" tuple ; = any balanced (...)
                        "\\|[~?]\\(?:" lid
                        "\\(?::\\(?:" lid "\\|(" balanced-braces ")\\)\\)?"
                        "\\|(" balanced-braces ")\\)"
                        "\\)\\)+\\)"))
         ;; group for possible class param
         (class-gparams
          (concat "\\<class\\>\\(?: +type\\>\\)?\\(?: +virtual\\>\\)?"
                  "\\( *\\[ *" typevar " *\\(?:, *" typevar " *\\)*\\]\\)?")))
  (setq
   tuareg-font-lock-keywords
   `(("^#[0-9]+ *\\(?:\"[^\"]+\"\\)?" 0 tuareg-font-lock-line-number-face t)
     ("\\<\\(false\\|true\\)\\>" . font-lock-constant-face)
     ;; "type" to introduce a local abstract type considered a keyword
     (,(concat "( *\\(type\\) +\\(" lid "\\) *)")
      (1 font-lock-keyword-face)
      (2 font-lock-type-face))
     (,(regexp-opt '("module" "include" "sig" "struct" "functor"
                     "type" "constraint" "class" "in" "inherit"
                     "method" "external" "val" "open"
                     "initializer" "let" "rec" "object" "and" "begin" "end")
                   'words)
      . tuareg-font-lock-governing-face)
     ,@(if (tuareg-editing-ls3)
           `((,(concat "\\<\\(let[ \t]+" let-ls3 "\\)\\>")
              . tuareg-font-lock-governing-face)))
     (,(let ((kwd '("as" "do" "done" "downto" "else" "for" "if"
                    "then" "to" "try" "when" "while" "match" "new"
                    "lazy" "assert" "fun" "function" "exception")))
         (if (tuareg-editing-ls3)
             (progn (push "reset" kwd)  (push "merge" kwd)
                    (push "emit" kwd)  (push "period" kwd)))
         (regexp-opt kwd 'words))
      . font-lock-keyword-face)
     ;; with type: "with" treated as a governing keyword
     (,(concat "\\<\\(\\(?:with\\|and\\) +type\\>\\) *\\(" typeconstr "\\)?")
      (1 tuareg-font-lock-governing-face keep)
      (2 font-lock-type-face keep t))
     (,(concat "\\<\\(\\(?:with\\|and\\) +module\\>\\) *\\(?:\\(" module-path
               "\\) *\\)?\\(?:= *\\(" extended-module-path "\\)\\)?")
      (1 tuareg-font-lock-governing-face keep)
      (2 tuareg-font-lock-module-face keep t)
      (3 tuareg-font-lock-module-face keep t))
     ;; "module type of" module-expr (here "of" is a governing keyword)
     ("\\<module +type +of\\>"
      0 tuareg-font-lock-governing-face keep)
     (,(concat "\\<module +type +of +\\(" module-path "\\)?")
      1 tuareg-font-lock-module-face keep t)
     ;; "!", "mutable", "virtual" treated as governing keywords
     (,(concat "\\<\\(\\(?:val" (if (tuareg-editing-ls3) "\\|reset\\|do")
               "\\)!? +\\(?:mutable\\(?: +virtual\\)?\\>"
               "\\|virtual\\(?: +mutable\\)?\\>\\)\\|val!\\)\\( *" lid "\\)?")
      (1 tuareg-font-lock-governing-face keep)
      (2 font-lock-variable-name-face nil t))
     ("\\<class\\>\\(?: +type\\>\\)?\\( +virtual\\>\\)?"
      1 tuareg-font-lock-governing-face nil t)
     ;; "private" treated as governing keyword
     (,(concat "\\<method!?\\(?: +\\(private\\(?: +virtual\\)?"
               "\\|virtual\\(?: +private\\)?\\)\\>\\)?")
      1 tuareg-font-lock-governing-face keep t)
     ;; Other uses of "with", "mutable", "private", "virtual"
     (,(regexp-opt '("of" "with" "mutable" "private" "virtual") 'words)
      . font-lock-keyword-face)
     ;;; labels
     (,(concat "\\([?~]" lid "\\)" tuareg--whitespace-re ":[^:>=]")
      1 font-lock-constant-face keep)
     ;;; label in a type signature
     (,(concat "\\(?:->\\|:[^:>=]\\)" tuareg--whitespace-re
               "\\(" lid "\\)[ \t]*:[^:>=]")
      1 font-lock-constant-face)
     (,(concat "\\<open\\(! +\\|\\> *\\)\\(" module-path "\\)?")
      (1 tuareg-font-lock-governing-face)
      (2 tuareg-font-lock-module-face keep t))
     (,(regexp-opt '("failwith" "failwithf" "exit" "at_exit" "invalid_arg"
                     "parser" "raise" "ref" "ignore") 'words)
      . font-lock-builtin-face)
     ;; module paths A.B.
     (,(concat module-path "\\.") . tuareg-font-lock-module-face)
     (,(concat
         "[][;,()|{}]\\|[-@^!:*=<>&/%+~?#]\\.?\\|\\.\\.\\.*\\|"
         (regexp-opt
          (if (tuareg-editing-ls3)
              '("asr" "asl" "lsr" "lsl" "or" "lor" "and" "land" "lxor"
                "not" "lnot" "mod" "fby" "pre" "last" "at")
            '("asr" "asl" "lsr" "lsl" "or" "lor" "land"
              "lxor" "not" "lnot" "mod"))
          'words))
      . tuareg-font-lock-operator-face)
     ;;; (lid: t) and (lid :> t)
     (,(concat "( *" lid " *:>?\\([ \n'_A-Za-z]"
               balanced-braces-no-string "\\))")
      1 font-lock-type-face keep)
     (,(concat "\\<external +\\(" lid "\\)")  1 font-lock-function-name-face)
     (,(concat "\\<exception +\\(" uid "\\)") 1 font-lock-variable-name-face)
     (,(concat "\\<module\\(?: +type\\)?\\(?: +rec\\)?\\> *\\(" uid "\\)")
      1 tuareg-font-lock-module-face)
     ;; (M: S) -- only color S here (may be "A.T with type t = s")
     (,(concat "( *" uid " *: *\\("
               modtype-path "\\(?: *\\<with\\>" balanced-braces "\\)?\\) *)")
      1 tuareg-font-lock-module-face keep)
     (,(concat "\\<include +\\(" extended-module-path "\\|( *"
               extended-module-path " *: *" balanced-braces " *)\\)")
      1 tuareg-font-lock-module-face keep)
     ;; module type A = B
     (,(concat "\\<module +type +" id " *= *\\(" modtype-path "\\)")
      1 tuareg-font-lock-module-face keep)
     ;; module A(B: _)(C: _) : D = E, including "module A : E"
     (,(concat "\\<module +" uid tuareg--whitespace-re
               "\\(\\(?:( *" uid " *: *"
               modtype-path "\\(?: *\\<with\\>" balanced-braces "\\)?"
               " *)" tuareg--whitespace-re "\\)*\\)\\(?::"
               tuareg--whitespace-re "\\(" modtype-path
               "\\) *\\)?\\(?:=" tuareg--whitespace-re
               "\\(" extended-module-path "\\)\\)?")
      (1 font-lock-variable-name-face keep); functor (module) variable
      (2 tuareg-font-lock-module-face keep t)
      (3 tuareg-font-lock-module-face keep t))
     (,(concat "\\<functor\\> *( *\\(" uid "\\) *: *\\(" modtype-path "\\) *)")
      (1 font-lock-variable-name-face keep); functor (module) variable
      (2 tuareg-font-lock-module-face keep))
     ;;; "type lid" anywhere (e.g. "let f (type t) x =") introduces a new type
     (,(concat "\\<type\\>" tuareg--whitespace-re "\\(" typedef "\\)")
      1 font-lock-type-face keep)
     ;; Constructors
     (,(concat "`" id) . tuareg-font-lock-constructor-face)
     (,(concat "\\(" uid "\\)[^.]")  1 tuareg-font-lock-constructor-face)
     ;;; let-bindings
     (,(concat let-binding " *\\(" lid "\\) *\\(?:: *\\([^=]+\\)\\)?= *"
               "fun\\(?:ction\\)?\\>")
      (1 font-lock-function-name-face nil t)
      (2 font-lock-type-face keep t))
     (,(let* ((maybe-constr (concat "\\(?:" constructor " *\\)?"))
              (var (concat maybe-constr "\\(?:" lid "\\|" tuple "\\)"))
              (simple-patt (concat var "\\(?: *, *" var "\\)*")))
         (concat let-binding " *\\(" simple-patt
                 "\\) *\\(?:: *\\([^=]+\\)\\)?="))
      ;; module paths, types, constructors already colored by the above
      (1 font-lock-variable-name-face keep)
      (2 font-lock-type-face keep t))
     (,(concat let-binding " *\\(" lid "\\)" gvars "?\\(?: +:"
               tuareg--whitespace-re "\\([a-z_]\\|[^ =][^=]*[^ =]\\) *=\\)?")
      (1 font-lock-function-name-face nil t)
      (2 font-lock-variable-name-face keep t)
      (3 font-lock-type-face keep t))
     (,(concat "\\<function\\>" tuareg--whitespace-re "\\(" lid "\\)")
      1 font-lock-variable-name-face)
     (,(concat "\\<fun +" gvars " *->")
      1 font-lock-variable-name-face keep nil)
     (,(concat class-gparams " *\\(" lid "\\)")
      (1 font-lock-type-face keep t)
      (2 font-lock-function-name-face))
     (,(concat class-gparams " *" lid gvars "? *=")
      2 font-lock-variable-name-face keep t)
     ;; "method": long match first to capture the method name
     (,(concat "\\<method!? +\\(?:private +\\(?:virtual +\\)?"
               "\\|virtual +\\(?:private +\\)?\\)\\(" lid "\\)")
      1 font-lock-function-name-face keep t); method name
     (,(concat "\\<method!? +\\(" lid "\\)" gvars "?")
      (1 font-lock-function-name-face keep t); method name
      (2 font-lock-variable-name-face keep t))
     (,(concat "\\<object *(\\(" lid "\\) *\\(?:: *\\("
               balanced-braces "\\)\\)?)")
      (1 font-lock-variable-name-face)
      (2 font-lock-type-face keep t))
     (,(concat "\\<object *( *\\(" typevar "\\|_\\) *)")
      1 font-lock-type-face)
     ;; "val" without "!", "mutable" or "virtual"
     (,(concat "\\<val +\\(" lid "\\)") 1 font-lock-function-name-face)
     (,(concat "\\<\\("
               (regexp-opt '("DEFINE" "IFDEF" "IFNDEF" "THEN" "ELSE" "ENDIF"
                             "INCLUDE" "__FILE__" "__LOCATION__"))
               "\\)\\>")
      . font-lock-preprocessor-face)
     ,@(and tuareg-support-metaocaml
            '(("\\.<\\|>\\.\\|\\.~\\|\\.!"
               0 tuareg-font-lock-multistage-face nil nil)))
     ,@(and tuareg-font-lock-symbols
            (tuareg-font-lock-symbols-keywords)))))
  (setq font-lock-defaults
        `(tuareg-font-lock-keywords
          ,(not tuareg-use-syntax-ppss) nil
          ,tuareg-font-lock-syntax nil
          ,@(unless (fboundp 'tuareg-syntax-propertize)
              '((font-lock-syntactic-keywords
                 . tuareg-font-lock-syntactic-keywords)
                (parse-sexp-lookup-properties . t)))
          (font-lock-syntactic-face-function
           . tuareg-font-lock-syntactic-face-function)
          ,@(unless tuareg-use-syntax-ppss
              '((font-lock-fontify-region-function
                 . tuareg-fontify-region)))))
  ;; (if tuareg-use-smie
  ;;     (push 'smie-backward-sexp-command font-lock-extend-region-functions))
  (when (and (boundp 'font-lock-fontify-region-function)
             (not tuareg-use-syntax-ppss))
    (set (make-local-variable 'font-lock-fontify-region-function)
         'tuareg-fontify-region)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                    Keymap

(defvar tuareg-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "|" 'tuareg-electric-pipe)
    (define-key map ")" 'tuareg-electric-rp)
    (define-key map "}" 'tuareg-electric-rc)
    (define-key map "]" 'tuareg-electric-rb)
    (define-key map "\M-q" 'tuareg-indent-phrase)
    (define-key map "\C-c\C-q" 'tuareg-indent-phrase)
    ;; Don't bother: it's the global default anyway.
    ;;(define-key map "\M-\C-\\" 'indent-region)
    (define-key map "\C-c\C-a" 'tuareg-find-alternate-file)
    (define-key map "\C-c\C-c" 'compile)
    (define-key map "\C-xnd" 'tuareg-narrow-to-phrase)
    (define-key map "\M-\C-x" 'tuareg-eval-phrase)
    (define-key map [remap newline-and-indent] 'tuareg-newline-and-indent)
    (define-key map "\C-x\C-e" 'tuareg-eval-phrase)
    (define-key map "\C-c\C-e" 'tuareg-eval-phrase)
    (define-key map "\C-c\C-r" 'tuareg-eval-region)
    (define-key map "\C-c\C-b" 'tuareg-eval-buffer)
    (define-key map "\C-c\C-s" 'tuareg-run-ocaml)
    (define-key map "\C-c\C-i" 'tuareg-interrupt-ocaml)
    (define-key map "\C-c\C-k" 'tuareg-kill-ocaml)
    (define-key map "\C-c\C-n" 'tuareg-next-phrase)
    (define-key map "\C-c\C-p" 'tuareg-previous-phrase)
    (define-key map [(backspace)] 'backward-delete-char-untabify)
    (define-key map [(control c) (home)]
      'tuareg-move-inside-module-or-class-opening)
    (unless tuareg-use-smie
      (define-key map [(control c) (control down)] 'tuareg-next-phrase)
      (define-key map [(control c) (control up)] 'tuareg-previous-phrase)
      (define-key map [(meta control down)]  'tuareg-next-phrase)
      (define-key map [(meta control up)] 'tuareg-previous-phrase)
      (define-key map [(meta control n)]  'tuareg-next-phrase)
      (define-key map [(meta control p)] 'tuareg-previous-phrase)
      )
    (define-key map [(meta control h)] 'tuareg-mark-phrase)
    (define-key map "\C-c`" 'tuareg-interactive-next-error-source)
    (define-key map "\C-c?" 'tuareg-interactive-next-error-source)
    (define-key map "\C-c.c" 'tuareg-insert-class-form)
    (define-key map "\C-c.b" 'tuareg-insert-begin-form)
    (define-key map "\C-c.f" 'tuareg-insert-for-form)
    (define-key map "\C-c.w" 'tuareg-insert-while-form)
    (define-key map "\C-c.i" 'tuareg-insert-if-form)
    (define-key map "\C-c.l" 'tuareg-insert-let-form)
    (define-key map "\C-c.m" 'tuareg-insert-match-form)
    (define-key map "\C-c.t" 'tuareg-insert-try-form)
    (when tuareg-with-caml-mode-p
      ;; Trigger caml-types
      (define-key map [?\C-c ?\C-t] 'caml-types-show-type)  ; "type"
      (define-key map [?\C-c ?\C-f] 'caml-types-show-call)  ; "function"
      (define-key map [?\C-c ?\C-l] 'caml-types-show-ident) ; "let"
      ;; To prevent misbehavior in case of error during exploration.
      (define-key map [?\C-c mouse-1] 'caml-types-mouse-ignore)
      (define-key map [?\C-c down-mouse-1] 'caml-types-explore)
      ;; Trigger caml-help
      (define-key map [?\C-c ?i] 'ocaml-add-path)
      (define-key map [?\C-c ?\[] 'ocaml-open-module)
      (define-key map [?\C-c ?\]] 'ocaml-close-module)
      (define-key map [?\C-c ?h] 'caml-help)
      (define-key map [?\C-c ?\t] 'tuareg-complete))
    map)
  "Keymap used in Tuareg mode.")

(defvar tuareg-electric-indent-keywords
  '("module" "class" "functor" "object" "type" "val" "inherit"
    "include" "virtual" "constraint" "exception" "external" "open"
    "method" "and" "initializer" "to" "downto" "do" "done" "else"
    "begin" "end" "let" "in" "then" "with"))

(defvar tuareg-mode-abbrev-table ()
  "Abbrev table used for Tuareg mode buffers.")
(if tuareg-mode-abbrev-table ()
  (define-abbrev-table 'tuareg-mode-abbrev-table
    (mapcar (lambda (keyword)
              `(,keyword ,keyword tuareg-abbrev-hook nil t))
            tuareg-electric-indent-keywords)))

(defun tuareg--electric-indent-predicate (char)
  "Check whether we should auto-indent.
For use on `electric-indent-functions'."
  (save-excursion
    (forward-char -1) ;; Go before the inserted char.
    (let ((syntax (char-syntax char)))
      (if (tuareg-in-indentation-p)
          (or (eq char ?|) (eq syntax ?\)))
        (or (case char
              (?\) (char-equal ?* (preceding-char)))
              (?\} (and (char-equal ?> (preceding-char))
                        (progn (tuareg-backward-char)
                               (tuareg-in-indentation-p))))
              (?\] (and (char-equal ?| (preceding-char))
                        (progn (tuareg-backward-char)
                               (tuareg-in-indentation-p)))))
            (and tuareg-use-abbrev-mode  ;; Misnomer, eh?
                 (not (eq syntax ?w))
                 (let ((end (point)))
                   (skip-syntax-backward "w_")
                   (member (buffer-substring (point) end)
                           tuareg-electric-indent-keywords))
                                           (tuareg-in-indentation-p)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;				 SMIE

;; TODO:
;; - Obey tuareg-*-indent customization variables.
;; - Fix use of tuareg-indent-command in tuareg-auto-fill-insert-leading-star.
;; - Use it by default (when possible).
;; - Move the old indentation code to a separate file.

(defconst tuareg-smie-grammar
  ;; Problems:
  ;; - "let D in E" expression vs "let D" declaration.  This is solved
  ;;   by making the lexer return "d-let" for the second case.
  ;; - FIXME: SMIE assumes that concatenation binds tighter than
  ;;   everything else, whereas OCaml gives tighter precedence to ".".
  ;; - "x : t1; (y : (t2 -> t3)); z : t4" but
  ;;   "when (x1; x2) -> (z1; z2)".  We solve this by distinguishing
  ;;   the two kinds of arrows, using "t->" for the type arrow.
  ;; - The "with" in modules's "with type" has different precedence.
  ;; - Big problem with "if...then": because of SMIE's transitivity of the
  ;;   precedence relation, we can't properly parse both "if A then B; C" and
  ;;   "if A then let x = E in B; C else D" (IOW I think a non-transitive OPG
  ;;   could do it).  We could try and fix the problem in the lexer, but it's
  ;;   far from obvious how (we'd probably end up having to pre-parse the text
  ;;   in the lexer to decide which kind of "if" and "then" we're looking
  ;;   at).  A good solution could be found maybe if SMIE let us disambiguate
  ;;   lexemes late, i.e. at a time where we have access to the relevant parse
  ;;   stack.  Or maybe by allowing smie-grammar to use a non-transitive
  ;;   precedence relation.  But until that happens, we will live with an
  ;;   incorrect parse, and instead we try to patch up the result with ad-hoc
  ;;   hacks in tuareg-smie-rules.
  ;; - The "<module-type> with <mod-constraints>" syntax introduces many
  ;;   conflicts:
  ;;      "... with module M = A with module B = C"
  ;;   vs      "... module M = A with module B = C"
  ;;   In the first, the second "with" should either have the first "with" as
  ;;   sibling, or have some earlier construct as parent, whereas in the second
  ;;   the "with" should have the first "=" (or maybe the first "module", tho
  ;;   that would not correspond to the actual language syntax and would
  ;;   probably break other cases) as parent.  Other problems in this
  ;;   mod-constraints syntax: we need a precedence along the lines of
  ;;   "with" < "and" < "module/type", whereas the rest of the syntax wants
  ;;   "module/type" < "and" < "with", so basically all the keywords involved
  ;;   in mod-constraints need to be handled specially in the lexer :-(
  ;; - and then some...
  (when (fboundp 'smie-prec2->grammar)
    (let ((bnfprec2
           (smie-bnf->prec2
            '((decls (decls "type" decls) (decls "d-let" decls)
                     (decls "and" decls) (decls ";;" decls)
                     (decls "exception" decls)
                     (decls "module" decls)
                     (decls "class" decls)
                     (decls "val" decls) (decls "external" decls)
                     (decls "open" decls) (decls "include" decls)
                     (decls "DEFINE" decls)
                     (exception)
                     (def)
                     ;; Hack: at the top-level, a "let D in E" can appear in
                     ;; decls as well, but the lexer classifies it as "d-let",
                     ;; so we need to make sure that "d-let D in E" doesn't
                     ;; end up matching the "in" with some far away thingy.
                     (def-in-exp))
              (def-in-exp (defs "in" exp))
              (def (var "d=" exp) (id "d=" datatype) (id "d=" module))
              (idtype (id ":" type))
              (var (id) ("m-type" var) ("rec" var) ("private" var) (idtype)
                   ("l-module" var) ("l-class" var))
              (exception (id "of" type))
              (datatype ("{" typefields "}") (typebranches)
                        (typebranches "with" id))
              (typebranches (typebranches "|" typebranches) (id "of" type))
              (typefields (typefields ";" typefields) (idtype))
              (type (type "*…" type) (type "t->" type)
                    ;; ("<" ... ">") ;; FIXME!
                    (type "as" id))
              (id)
              (module ("struct" decls "end")
                      ("sig" decls "end")
                      ("functor" id "->" module)
                      (module "m-with" mod-constraints))
              (simpledef (id "c=" type))
              (mod-constraints (mod-constraints "m-and" mod-constraints)
                               ("w-type" simpledef)
                               ("w-module" simpledef))
              ;; http://caml.inria.fr/pub/docs/manual-ocaml/expr.html
              ;; exp1 is "all exps except for `if exp then'".
              (exp1 ("begin" exp "end")
                    ("(" exp:type ")")
                    ("[|" exp "|]")
                    ("{" fields "}")
                    ("if" exp "then" exp1 "else" exp1)
                    ;; ("if" exp "then" exp)
                    ("while" exp "do" exp "done")
                    ("for" forbounds "do" exp "done")
                    (exp1 ";" exp1)
                    ("match" exp "with" branches)
                    ("function" branches)
                    ("fun" patterns "->" exp1)
                    ("try" exp "with" branches)
                    ("let" defs "in" exp1)
                    ("object" class-body "end")
                    ("(" exp:>type ")")
                    ("{<" fields ">}"))
              ;; Like `exp' but additionally allow if-then without else.
              (exp (exp1) ("if" exp "then" exp))
              (forbounds (iddef "to" exp) (iddef "downto" exp))
              (defs (def) (defs "and" defs) ("l-open" id))
              (exp:>type (exp:type ":>" type))
              (exp:type (exp)) ;; (exp ":" type)
              (fields (fields1) (exp "with" fields1))
              (fields1 (fields1 ";" fields1) (iddef))
              (iddef (id "f=" exp1))
              (branches (branches "|" branches) (branch))
              (branch (patterns "->" exp1))
              (patterns (pattern) (pattern "when" exp1))
              (pattern (id) (pattern "as" id) (pattern "," pattern))
              (class-body (class-body "inherit" class-body)
                          (class-body "method" class-body)
                          (class-body "initializer" class-body)
                          (class-body "val" class-body)
                          (class-body "constraint" class-body)
                          (class-field))
              (class-field (exp) ("mutable" idtype)
                           ("virtual" idtype) ("private" idtype))
              ;; We get cyclic dependencies between ; and | because things like
              ;; "branches | branches" implies that "; > |" whereas "exp ; exp"
              ;; implies "| > ;" and while those two do not directly conflict
              ;; because they're constraints on precedences of different sides,
              ;; they do introduce a cycle later on because those operators are
              ;; declared associative, which adds a constraint that both sides
              ;; must be of equal precedence.  So we declare here a dummy rule
              ;; to force a direct conflict, that we can later resolve with
              ;; explicit precedence rules.
              (foo1 (foo1 ";" foo1) (foo1 "|" foo1))
              ;; "mutable x : int ; y : int".
              (foo2 ("mutable" id) (foo2 ";" foo2))
              )
            ;; Type precedence rules.
            ;; http://caml.inria.fr/pub/docs/manual-ocaml/types.html
            '((nonassoc "as") (assoc "t->") (assoc "*…"))
            ;; Pattern precedence rules.
            ;; http://caml.inria.fr/pub/docs/manual-ocaml/patterns.html
            ;; Note that we don't include "|" because its precedence collides
            ;; with the one of the | used between branches and resolving the
            ;; conflict in the lexer is not worth the trouble.
            '((nonassoc "as") (assoc ",") (assoc "::"))
            ;; Resolve "{a=(1;b=2)}" vs "{(a=1);(b=2)}".
            '((nonassoc ";") (nonassoc "f="))
            ;; Resolve "(function a -> b) | c -> d".
            '((nonassoc "function") (nonassoc "|"))
            ;; Resolve "when (function a -> b) -> c".
            '((nonassoc "function") (nonassoc "->"))
            ;; Resolve ambiguity "(let d in e2); e3" vs "let d in (e2; e3)".
            '((nonassoc "in" "match" "->" "with") (nonassoc ";"))
            ;; Resolve "(if a then b else c);d" vs "if a then b else (c; d)".
            '((nonassoc ";") (nonassoc "else")) ;; ("else" > ";")
            ;; Resolve "match e1 with a → (match e2 with b → e3 | c → e4)"
            ;;      vs "match e1 with a → (match e2 with b → e3) | c → e4"
            '((nonassoc "with") (nonassoc "|"))
            ;; Resolve "functor A -> (M with MC)".
            '((nonassoc "->") (nonassoc "m-with"))
            ;; Resolve the conflicts caused by "when" and by SMIE's assumption
            ;; that all non-terminals can match the empty string.
            '((nonassoc "with") (nonassoc "->")) ; "when (match a with) -> e"
            '((nonassoc "|") (nonassoc "->")) ; "when (match a with a|b) -> e"
            ;; Fix up conflict between (decls "and" decls) and (defs "in" exp).
            '((nonassoc "in") (nonassoc "and"))
            ;; Resolve the "artificial" conflict introduced by the `foo1' rule.
            '((assoc "|") (assoc ";"))
            ;; Fix up associative declaration keywords.
            '((assoc "type" "d-let" "exception" "module" "val" "open"
                     "external" "include" "class" "DEFINE" ";;")
              (assoc "and"))
            '((assoc "val" "method" "inherit" "constraint" "initializer"))
            ;; Declare associativity of remaining sequence separators.
            '((assoc ";")) '((assoc "|")) '((assoc "m-and")))))
      ;; (dolist (pair '()) ;; ("then" . "|") ("|" . "then")
      ;;   (display-warning 'prec2 (format "%s %s %s"
      ;;                                   (car pair)
      ;;                                   (gethash pair bnfprec2)
      ;;                                   (cdr pair))))
      ;; SMIE takes for granted that all non-terminals can match the empty
      ;; string, which can lead to the addition of unnecessary constraints.
      ;; Let's remove the ones that cause cycles without causing conflicts.
      (progn
        ;; This comes from "exp ; exp" and "function branches", where
        ;; SMIE doesn't realize that `branches' has to have a -> before ;.
        (assert (eq '> (gethash (cons "function" ";") bnfprec2)))
        (remhash (cons "function" ";") bnfprec2))
      (smie-prec2->grammar
       (smie-merge-prec2s
        bnfprec2
        (smie-precs->prec2
         ;; Precedence of operators.
         ;; http://caml.inria.fr/pub/docs/manual-ocaml/expr.html
         (reverse
          '((nonassoc ".")
            ;; function application, constructor application, assert, lazy
            ;; - -. (prefix)    –
            (right "**…" "lsl" "lsr" "asr")
            (nonassoc "*…" "/…" "%…" "mod" "land" "lor" "lxor")
            (left "+…" "-…")
            (assoc "::")
            (right "@…" "^…")
            (left "=…" "<…" ">…" "|…" "&…" "$…")
            (right "&" "&&")
            (right "or" "||")
            (assoc ",")
            (right "<-" ":=")
            (assoc ";")))))))))

(defun tuareg-smie--search-backward (tokens)
  (let (tok)
    (while (progn
             (setq tok (tuareg-smie--backward-token))
             (if (not (zerop (length tok)))
                 (not (member tok tokens))
	       (unless (bobp)
		 (condition-case err
		     (progn (backward-sexp) t)
		   (scan-error
		    (setq tok (buffer-substring (nth 3 err) (1+ (nth 3 err))))
		    nil))))))
    tok))

(defconst tuareg-smie--type-label-leader
  '("->" ":" "=" ""))
(defconst tuareg-smie--exp-operator-leader
  (delq nil (mapcar (lambda (x) (if (numberp (nth 2 x)) (car x)))
                    tuareg-smie-grammar)))
(defconst tuareg-smie--float-re "[0-9]+\\(?:\\.[0-9]*\\)?\\(?:e[-+]?[0-9]+\\)")

(defun tuareg-smie--forward-token ()
  (forward-comment (point-max))
  (buffer-substring-no-properties
   (point)
   (progn (if (zerop (skip-syntax-forward "."))
              (let ((start (point)))
                (skip-syntax-forward "w_'")
                ;; Watch out for floats!
                (and (memq (char-after) '(?- ?+))
                     (eq (char-before) ?e)
                     (save-excursion
                       (goto-char start)
                       (looking-at tuareg-smie--float-re))
                     (goto-char (match-end 0))))
            ;; The "." char is given symbol property so that "M.x" is
            ;; considered as a single symbol, but in reality, it's part of the
            ;; operator chars, since "+." and friends are operators.
            (while (not (and (zerop (skip-chars-forward "."))
                             (zerop (skip-syntax-forward "."))))))
          (point))))

(defun tuareg-smie--backward-token ()
  (forward-comment (- (point)))
  (buffer-substring-no-properties
   (point)
   (progn (if (and (zerop (skip-chars-backward "."))
                   (zerop (skip-syntax-backward ".")))
              (progn
                (skip-syntax-backward "w_'")
                ;; Watch out for floats!
                (and (memq (char-before) '(?- ?+))
                     (memq (char-after) '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0))
                     (save-excursion
                       (forward-char -1) (skip-syntax-backward "w_")
                       (looking-at tuareg-smie--float-re))
                     (>= (match-end 0) (point))
                     (goto-char (match-beginning 0))))
            (cond
             ((memq (char-after) '(?\; ?,)) nil) ; ".;" is not a token.
             ((and (eq (char-after) ?\.)
                   (memq (char-before) '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0)))
              (skip-chars-backward "0-9")) ; A float number!
             (t ;; The "." char is given symbol property so that "M.x" is
              ;; considered as a single symbol, but in reality, it's part of
              ;; the operator chars, since "+." and friends are operators.
              (while (not (and (zerop (skip-chars-backward "."))
                               (zerop (skip-syntax-backward "."))))))))
          (point))))

(defun tuareg-smie-forward-token ()
  "Move the point at the end of the next token and return the SMIE name
of the token."
  (let ((tok (tuareg-smie--forward-token)))
    (cond
     ((zerop (length tok))
      (if (not (looking-at "{<\\|\\[|"))
          tok
        (goto-char (match-end 0))
        (match-string 0)))
     ((or (member tok '("let" "=" "->"
                        "module" "class" "open" "type" "with" "and"))
          ;; http://caml.inria.fr/pub/docs/manual-ocaml/expr.html lists
          ;; the tokens whose precedence is based on their prefix.
          (memq (aref tok 0) '(?* ?/ ?% ?+ ?- ?@ ?^ ?= ?< ?> ?| ?& ?$)))
      ;; When indenting, the movement is mainly backward, so it's OK to make
      ;; the forward tokenizer a bit slower.
      (save-excursion (tuareg-smie-backward-token)))
     ((and (member tok '("~" "?"))
           (looking-at "[[:alpha:]_][[:alnum:]'_]*:"))
      (goto-char (match-end 0))
      "label:")
     ((and (looking-at ":\\(?:[^:]\\|\\'\\)")
           (string-match "\\`[[:alpha:]_]" tok)
           (save-excursion
             (tuareg-smie--backward-token) ;Go back.
             (member (tuareg-smie--backward-token)
                     tuareg-smie--type-label-leader)))
      (forward-char 1)
      "label:")
     ((and (equal tok "|") (looking-at "\\]")) (forward-char 1) "|]")
     ((and (equal tok ">") (looking-at "}")) (forward-char 1) ">}")
     ((string-match "\\`[[:alpha:]_].*\\.\\'"  tok)
      (forward-char -1) (substring tok 0 -1))
     (t tok))))

(defconst tuareg-smie--exp-leaders
  ;; (let ((leaders ()))
  ;;   (dolist (cat tuareg-smie-bnf)
  ;;     (dolist (rule (cdr cat))
  ;;       (setq rule (reverse rule))
  ;;       (while (setq rule (cdr (memq 'exp rule)))
  ;;         (push (car rule) leaders))))
  ;;   leaders)
  '("if" "then" "try" "match" "do" "while" "begin" "in" "when"
    "downto" "to" "else"))

(defun tuareg-smie--label-colon-p ()
  (and (not (zerop (skip-chars-backward "[[:alnum:]]_")))
       (or (not (zerop (skip-chars-backward "?~")))
           (save-excursion
             (member (tuareg-smie--backward-token)
                     tuareg-smie--type-label-leader)))))

(defun tuareg-smie--=-disambiguate ()
  "Return which kind of \"=\" we've just found.
Point is not moved and should be right in front of the equality.
Return values can be
  \"f=\" for field definition,
  \"d=\" for a normal definition,
  \"c=\" for a type equality constraint, and
  \"=…\" for an equality test."
  (save-excursion
    (let* ((pos (point))
           (telltale '("type" "let" "module" "class" "and" "external"
                       "val" "method" "DEFINE" "="
                       "if" "then" "else" "->" ";" ))
           (nearest (tuareg-smie--search-backward telltale)))
      (cond
       ((and (member nearest '("{" ";"))
             (let ((field t))
               (while
                   (let ((x (tuareg-smie--forward-token)))
                     (and (< (point) pos)
                          (cond
                           ((zerop (length x)) (setq field nil))
                           ((memq (char-syntax (aref x 0)) '(?w ?_)))
                           ((member x '("." ";")))
                           (t (setq field nil))))))
               field))
        "f=")
       ((progn
          (while (and (equal nearest "->")
                      (save-excursion
                        (forward-char 2)
                        (equal (tuareg-smie-backward-token) "t->")))
            (setq nearest (tuareg-smie--search-backward telltale)))
          nil))
       ((not (member nearest '("type" "let" "module" "class" "and"
                               "external" "val" "method" "DEFINE")))
        "=…")
       ((and (member nearest '("type" "module"))
             (member (tuareg-smie--backward-token) '("with" "and"))) "c=")
       (t "d=")))))

(defun tuareg-smie-backward-token ()
  (let ((tok (tuareg-smie--backward-token)))
    (cond
     ;; Distinguish a let expression from a let declaration.
     ((equal tok "let")
      (save-excursion
        (let ((prev (tuareg-smie--backward-token)))
          (if (or (member prev tuareg-smie--exp-leaders)
                  (if (zerop (length prev))
                      (and (not (bobp))
                           (eq 4 (mod (car (syntax-after (1- (point)))) 256)))
                    (and (eq ?. (char-syntax (aref prev 0)))
                         (not (equal prev ";;")))))
              tok
            "d-let"))))
     ;; Handle "let module" and friends.
     ((member tok '("module" "class" "open"))
      (let ((prev (save-excursion (tuareg-smie--backward-token))))
        (cond
         ((equal prev "let") (concat "l-" tok))
         ((and (member prev '("with" "and")) (equal tok "module")) "w-module")
         (t tok))))
     ;; Distinguish a "type ->" from a "case ->".
     ((equal tok "->")
      (save-excursion
        (let (nearest)
          (while (progn
                   (setq nearest (tuareg-smie--search-backward
                                  '("with" "|" "fun" "functor"
				    "type" ":" "of")))
                   (and (equal nearest ":")
                        (tuareg-smie--label-colon-p))))
          (if (member nearest '("with" "|" "fun" "functor"))
              tok "t->"))))
     ;; Handle "module type" and mod-constraint's "with/and type".
     ((equal tok "type")
      (save-excursion
        (let ((prev (tuareg-smie--backward-token)))
          (cond ((equal prev "module") "m-type")
                ((member prev '("and" "with")) "w-type")
                (t tok)))))
     ;; Disambiguate mod-constraint's "and" and "with".
     ((member tok '("with" "and"))
      (save-excursion
        (tuareg-smie--forward-token)
        (if (member (tuareg-smie--forward-token) '("type" "module"))
            (concat "m-" tok) tok)))
     ;; Distinguish a defining = from a comparison-=.
     ((equal tok "=")
      (tuareg-smie--=-disambiguate))
     ((zerop (length tok))
      (if (not (and (memq (char-before) '(?\} ?\]))
                    (save-excursion (forward-char -2)
                                    (looking-at ">}\\||\\]"))))
          tok
        (goto-char (match-beginning 0))
        (match-string 0)))
     ((and (equal tok "|") (eq (char-before) ?\[)) (forward-char -1) "[|")
     ((and (equal tok "<") (eq (char-before) ?\{)) (forward-char -1) "{<")
     ;; Some infix operators get a precedence based on their prefix, so we
     ;; collapse them into a canonical representative.
     ;; See http://caml.inria.fr/pub/docs/manual-ocaml/expr.html.
     ((memq (aref tok 0) '(?* ?/ ?% ?+ ?- ?@ ?^ ?= ?< ?> ?| ?& ?$))
      (cond
       ((member tok '("|" "||" "&" "&&" "<-" "->")) tok)
       ((and (eq (aref tok 0) ?*) (> (length tok) 1) (eq (aref tok 1) ?*))
        "**…")
       (t (string (aref tok 0) ?…))))
     ((equal tok ":")
      (let ((pos (point)))
        (if (tuareg-smie--label-colon-p)
            "label:"
          (goto-char pos)
          tok)))
     ((string-match "\\`[[:alpha:]_].*\\.\\'"  tok)
      (forward-char (1- (length tok))) ".")
     (t tok))))

(defun tuareg-smie-rules (kind token)
  ;; FIXME: Handling of "= |", "with |", "function |", and "[ |" is
  ;; problematic.
  (cond
   ;; Special indentation for module fields.
   ((and (eq kind :after) (member token '("." ";"))
         (smie-rule-parent-p "with")
         (tuareg-smie--with-module-fields-rule)))
   ;; Special indentation for monadic >>>, >>|, and >>= operators.
   ((and (eq kind :before) (tuareg-smie--monadic-rule token)))
   ((member token '(";" "|" "," "and" "m-and"))
    (cond
     ((and (eq kind :before) (member token '("|" ";"))
           (smie-rule-parent-p "then")
           ;; We have misparsed the code: TOKEN is not a child of `then' but
           ;; should have closed the "if E1 then E2" instead!
           (tuareg-smie--if-then-hack token)))
     ;; FIXME: smie-rule-separator doesn't behave correctly when the separator
     ;; is right after the parent (on another line).
     ((smie-rule-prev-p "d=" "with" "[" "function")
      (if (and (eq kind :before) (smie-rule-bolp)
               (smie-rule-prev-p "[" "d=" "function"))
          0 tuareg-with-indent))
     (t (smie-rule-separator kind))))
   (t
    (case kind
      (:elem (if (eq token 'basic) tuareg-default-indent))
      (:list-intro (member token '("fun")))
      (:before
       (cond
        ((equal token "d=") (smie-rule-parent 2))
        ((member token '("fun" "match"))
         (if (and (not (smie-rule-bolp)) (smie-rule-prev-p "d="))
             (smie-rule-parent tuareg-default-indent)))
        ((equal token "then") (smie-rule-parent))
        ((equal token "if") (if (and (not (smie-rule-bolp))
                                     (smie-rule-prev-p "else"))
                                (smie-rule-parent)))
        ((and (equal token "with") (smie-rule-parent-p "{"))
         (smie-rule-parent))
        ;; Align the "with" of "module type A = B \n with ..." w.r.t "module".
        ((and (equal token "m-with") (smie-rule-parent-p "d="))
         (save-excursion
           (smie-backward-sexp token)
           (goto-char (nth 1 (smie-backward-sexp 'halfsexp)))
           (cons 'column (+ 2 (current-column)))))
        ;; Treat purely syntactic block-constructs as being part of their
        ;; parent, when the opening statement is hanging.
        ((member token '("let" "(" "[" "{" "sig" "struct" "begin"))
         (when (and (smie-rule-hanging-p)
                    (apply #'smie-rule-prev-p
                           tuareg-smie--exp-operator-leader))
           (if (let ((openers '("{" "(" "{<" "[" "[|")))
                 (or (apply #'smie-rule-prev-p openers)
                     (not (apply #'smie-rule-parent-p openers))))
               (let ((offset (if (equal token "(") 0 tuareg-default-indent)))
                 (smie-rule-parent offset))
             ;; In "{ a = (", "{" and "a =" are not part of the same
             ;; syntax rule, so "(" is part of "a =" but not of the
             ;; surrounding "{".
             (save-excursion
               (smie-backward-sexp 'halfsexp)
               (cons 'column (smie-indent-virtual))))))
        ;; If we're looking at the first class-field-spec
        ;; in a "object(type)...end", don't rely on the default behavior which
        ;; will treat (type) as a previous element with which to align.
        ((tuareg-smie--object-hanging-rule token))
        ;; Apparently, people like their `| pattern when test -> body' to have
        ;;  the `when' indented deeper than the body.
        ((equal token "when") (smie-rule-parent tuareg-match-when-indent))))
      (:after
       (cond
        ((equal token "d=")
         (and (smie-rule-parent-p "type")
              (not (smie-rule-next-p "["))
              0))
        ((equal token "->")
         (cond
          ((and (smie-rule-parent-p "with")
                ;; Align with "with" but only if it's the only branch (often
                ;; the case in try..with), since otherwise subsequent
                ;; branches can't be both indented well and aligned.
                (save-excursion
                  (and (not (equal "|" (nth 2 (smie-forward-sexp "|"))))
                       ;; Since we may misparse "if..then.." we need to
                       ;; double check that smie-forward-sexp indeed got us
                       ;; to the right place.
                       (equal (nth 2 (smie-backward-sexp "|")) "with"))))
           (smie-rule-parent 2))
          ((smie-rule-parent-p "|") tuareg-match-clause-indent)
          (t 0)))
        ((equal token ":")
         (cond
          ((smie-rule-parent-p "val" "external") (smie-rule-parent 2))
          ((smie-rule-parent-p "module") (smie-rule-parent))
          (t 2)))
        ((equal token "in") tuareg-in-indent) ;;(if (smie-rule-hanging-p)
        ((equal token "with")
         (cond
          ;; ((smie-rule-next-p "|") 2)
          ((smie-rule-parent-p "{") nil)
          (t (+ 2 tuareg-with-indent))))
        ((or (member token '("." "t->" "]"))
             (consp (nth 2 (assoc token tuareg-smie-grammar)))) ;; Closer.
         nil)
        (t tuareg-default-indent)))))))

(defun tuareg-smie--with-module-fields-rule ()
  ;; Indentation of fields after "{ E with Module." where the "Module."
  ;; syntactically only applies to the first field, but has
  ;; semantically a higher position since it applies to all fields.
  (save-excursion
    (forward-char 1)
    (smie-backward-sexp 'halfsexp)
    (when (looking-at "\\(?:\\sw\\|\\s_\\)+\\.[ \t]*$")
      (smie-backward-sexp 'halfsexp)
      (cons 'column (current-column)))))

(defun tuareg-smie--monadic-rule (token)
  ;; When trying to indent a >>=, try to look back to find any earlier
  ;; >>= in a sequence of "monadic steps".
  (or (and (equal token ">…") (looking-at ">>[>=|]")
           (save-excursion
             (tuareg-smie--forward-token)
             (let ((indent nil))
               (while
                   (let ((parent-data (smie-backward-sexp 'halfsexp)))
                     (cond
                      ((car parent-data) (member (nth 2 parent-data) '("->")))
                      ((member (nth 2 parent-data) '(";" "d=")) nil)
                      ((member (nth 2 parent-data) '("fun" '"function"))
                       (if (member (tuareg-smie--backward-token)
                                   '(">>|" ">>=" ">>>"))
                           (progn
                             (setq indent (cons 'column
                                                (smie-indent-virtual)))
                             nil)
                         t)))))
               indent)))
      ;; In "foo >>= fun x => bar" indent `bar' relative to `foo'.
      (and (member token '("fun" "function")) (not (smie-rule-bolp))
           (save-excursion
             (let ((prev (tuareg-smie-backward-token)))
               ;; FIXME: Should we use the same loop as above?
               (and (equal prev ">…") (looking-at ">>[>=|]")
                    (progn (smie-backward-sexp prev)
                           (cons 'column (current-column)))))))))

(defun tuareg-smie--object-hanging-rule (token)
  ;; If we're looking at the first class-field-spec
  ;; in a "object(type)...end", don't rely on the default behavior which
  ;; will treat (type) as a previous element with which to align.
  (cond
   ;; An important role of this first condition is to call smie-indent-virtual
   ;; so that we get called back to compute the (virtual) indentation of
   ;; "object", thus making sure we get called back to apply the second rule.
   ((and (member token '("inherit" "val" "method" "constraint"))
         (smie-rule-parent-p "object"))
    (save-excursion
      (forward-word 1)
      (goto-char (nth 1 (smie-backward-sexp 'halfsexp)))
      (let ((col (smie-indent-virtual)))
        `(column . ,(+ tuareg-default-indent col)))))
   ;; For "class foo = object(type)...end", align object...end with class.
   ((and (equal token "object") (smie-rule-parent-p "class"))
    (smie-rule-parent))))

(defun tuareg-smie--if-then-hack (token)
  ;; Getting SMIE's parser to properly parse "if E1 then E2" is difficult, so
  ;; instead we live with a confused parser and try to work around the mess
  ;; here, although it clearly won't help other uses of the parser
  ;; (e.g. navigation).
  (save-excursion
    (let (pd)
      (while (equal (nth 2 (setq pd (smie-backward-sexp token))) "then")
        (let ((pdi (smie-backward-sexp 'halfsexp)))
          (assert (equal (nth 2 pdi) "if"))))
      (cond
       ((equal (nth 2 pd) token)
        (goto-char (nth 1 pd))
        (cons 'column (smie-indent-virtual)))
       (t (cons 'column (current-column)))))))

(defun tuareg-smie--inside-string ()
  (when (nth 3 (syntax-ppss))
    (goto-char (1+ (nth 8 (syntax-ppss))))
    (current-column)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              The major mode

(defun tuareg--common-mode-setup ()
  (setq local-abbrev-table tuareg-mode-abbrev-table)
  (set (make-local-variable 'syntax-propertize-function)
       #'tuareg-syntax-propertize)
  (set (make-local-variable 'parse-sexp-ignore-comments)
       ;; Tuareg used to set this to nil (for an unknown reason) but SMIE needs
       ;; it to be set to t.
       tuareg-use-smie)
  (if (and tuareg-smie-grammar tuareg-use-smie)
      (progn
        (smie-setup tuareg-smie-grammar #'tuareg-smie-rules
                    :forward-token #'tuareg-smie-forward-token
                    :backward-token #'tuareg-smie-backward-token)
        (add-hook 'smie-indent-functions #'tuareg-smie--inside-string nil t)
        (set (make-local-variable 'add-log-current-defun-function)
             'tuareg-current-fun-name))
    (set (make-local-variable 'indent-line-function) #'tuareg-indent-command))
  (set (make-local-variable 'prettify-symbols-alist)
       tuareg-font-lock-symbols-alist)
  (tuareg-install-font-lock)
  (set (make-local-variable 'open-paren-in-column-0-is-defun-start) nil)

  (add-hook 'completion-at-point-functions #'tuareg-completion-at-point nil t)

  (when (fboundp 'electric-indent-mode)
    (add-hook 'electric-indent-functions
              #'tuareg--electric-indent-predicate nil t))
  (when (boundp 'post-self-insert-hook)
    (add-hook 'post-self-insert-hook #'tuareg--electric-close-vector nil t)))

;;;###autoload(add-to-list 'auto-mode-alist '("\\.ml[ip]?\\'" . tuareg-mode))
;;;###autoload(dolist (ext '(".cmo" ".cmx" ".cma" ".cmxa" ".cmi"
;;;###autoload                ".annot" ".cmt" ".cmti"))
;;;###autoload  (add-to-list 'completion-ignored-extensions ext))

(defalias 'tuareg--prog-mode
  (if (fboundp 'prog-mode) #'prog-mode #'fundamental-mode))
(defvar compilation-first-column)
(defvar compilation-error-screen-columns)

;;;###autoload
(define-derived-mode tuareg-mode tuareg--prog-mode "Tuareg"
  "Major mode for editing OCaml code.

Dedicated to Emacs and XEmacs, version 21 and higher.  Provides
automatic indentation and compilation interface.  Performs font/color
highlighting using Font-Lock.  It is designed for OCaml but handles
Caml Light as well.

The Font-Lock minor-mode is used according to your customization
options.

You have better byte-compile tuareg.el.

For customization purposes, you should use `tuareg-mode-hook'
\(run for every file) or `tuareg-load-hook' (run once) and not patch
the mode itself.  You should add to your configuration file something like:
  (add-hook 'tuareg-mode-hook
            (lambda ()
               ... ; your customization code
            ))
For example you can change the indentation of some keywords, the
`electric' flags, Font-Lock colors... Every customizable variable is
documented, use `C-h-v' or look at the mode's source code.

`dot-emacs.el' is a sample customization file for standard changes.
You can append it to your `.emacs' or use it as a tutorial.

`M-x ocamldebug' FILE starts the OCaml debugger ocamldebug on the executable
FILE, with input and output in an Emacs buffer named *ocamldebug-FILE*.

A Tuareg Interactive Mode to evaluate expressions in a toplevel is
included.  Type `M-x tuareg-run-ocaml' or simply `M-x run-ocaml' or see
special-keys below.

Short cuts for the Tuareg mode:
\\{tuareg-mode-map}

Short cuts for interactions with the toplevel:
\\{tuareg-interactive-mode-map}"

  ;; Initialize the Tuareg menu
  (tuareg-build-menu)

  (unless tuareg-use-smie
    ;; Initialize indentation regexps
    (tuareg-make-indentation-regexps))

  (set (make-local-variable 'paragraph-start)
       (concat "^[ \t]*$\\|\\*)$\\|" page-delimiter))
  (set (make-local-variable 'paragraph-separate) paragraph-start)
  (set (make-local-variable 'require-final-newline) t)
  (set (make-local-variable 'comment-start) "(* ")
  (set (make-local-variable 'comment-end) " *)")
  (set (make-local-variable 'comment-start-skip) "(\\*+[ \t]*")
  ;(set (make-local-variable 'comment-column) 40)              ;FIXME: Why?
  ;(set (make-local-variable 'comment-multi-line) t)           ;FIXME: Why?
  ;; `ocamlc' counts columns from 0, contrary to other tools which start at 1.
  (set (make-local-variable 'compilation-first-column) 0)
  (set (make-local-variable 'compilation-error-screen-columns) nil)
  (tuareg--common-mode-setup)
  (unless tuareg-use-syntax-ppss
    (add-hook 'before-change-functions 'tuareg-before-change-function nil t))
  (when (fboundp 'tuareg-auto-fill-function)
    ;; Emacs-21's newcomment.el provides this functionality by default.
    (set (make-local-variable 'normal-auto-fill-function)
         #'tuareg-auto-fill-function))

  (set (make-local-variable 'imenu-create-index-function)
       #'tuareg-imenu-create-index)

  (when (and tuareg-use-abbrev-mode
             (not (and (boundp 'electric-indent-mode) electric-indent-mode)))
    (abbrev-mode 1))
  (run-mode-hooks 'tuareg-load-hook))

(defconst tuareg-starters-syms
  '("module" "type" "let" "d-let" "and"))

(defun tuareg-find-matching-starter (starters)
  (let (tok)
    (while
        (let ((td (smie-backward-sexp 'halfsexp)))
          (cond
           ((and (car td)
                 (member (nth 2 td) starters))
            (goto-char (nth 1 td)) (setq tok (nth 2 td)) nil)
           ((and (car td) (not (numberp (car td))))
            (unless (bobp) (goto-char (nth 1 td)) t))
           (t t))))
    tok))

(defun tuareg-skip-siblings ()
  (while (and (not (bobp))
              (null (car (smie-backward-sexp))))
    (tuareg-find-matching-starter tuareg-starters-syms))
  (when (looking-at "in")
    ;; Skip over `local...in' and continue.
    (forward-word 1)
    (smie-backward-sexp 'halfsexp)
    (tuareg-skip-siblings)))

(defun tuareg-beginning-of-defun ()
  (when (tuareg-find-matching-starter tuareg-starters-syms)
	(save-excursion (tuareg-smie-forward-token)
                        (forward-comment (point-max))
                        (let ((name (tuareg-smie-forward-token)))
                          (if (not (member name '("rec" "type")))
                              name
                            (forward-comment (point-max))
                        (tuareg-smie-forward-token))))))

(defcustom tuareg-max-name-components 3
  "Maximum number of components to use for the current function name."
  :type 'integer)

(defun tuareg-current-fun-name ()
  (save-excursion
    (let ((count tuareg-max-name-components)
          fullname name)
      (end-of-line)
      (while (and (> count 0)
                  (setq name (tuareg-beginning-of-defun)))
        (decf count)
        (setq fullname (if fullname (concat name "." fullname) name))
        ;; Skip all other declarations that we find at the same level.
        (tuareg-skip-siblings))
      fullname)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Error processing

(require 'compile)

;; In some versions of Emacs, the regexps in
;; compilation-error-regexp-alist do not match the error messages when
;; the language is not English.  Hence we add a regexp.
;; FIXME: We should report those cases to bug-gnu-emacs@gnu.org.

(defconst tuareg-error-regexp
  ;; Errors can take forms like:
  ;;   "File "main.ml", line 1154, characters 30-48:\nError: ..."
  ;;   "Raised at file "pervasives.ml", line 22, characters 22-33"
  ;;   "File "main.ml", line 1018, characters 2-2632:\nWarning 8: ..."
  ;; as well as localized variants depending on locale.
  (concat "^\\(Called from \\)?[[:alpha:]][ [:alpha:]]*[[:alpha:]] "
          "\"\\([^\"\n]+\\)\", "                              ;File name.
          "[[:alpha:]]+ \\([0-9]+\\)\\(?:-\\([0-9]+\\)\\)?, " ;Lines.
          "[[:alpha:]]+ \\([0-9]+\\)-\\([0-9]+\\)"            ;Columns.
          "\\(?::\\(\nWarning\\)?\\|[-,:]\\|$\\)")            ;Warning/error.
  "Regular expression matching the error messages produced by ocamlc.")

(when (boundp 'compilation-error-regexp-alist)
  (or (assoc tuareg-error-regexp
             compilation-error-regexp-alist)
      (setq compilation-error-regexp-alist
            (cons (if (fboundp 'compilation-fake-loc)
                      (list tuareg-error-regexp
                            2 '(3 . 4) '(5 . 6) '(7 . 1))
                    (list tuareg-error-regexp 2 3))
                  ;; Other error format used for unhandled match case.
                  (cons '("^Fatal error: exception [^ \n]*(\"\\([^\"]*\\)\", \\([0-9]+\\), \\([0-9]+\\))"
                          1 2 3)
                        compilation-error-regexp-alist)))))

;; A regexp to extract the range info.

;; (defconst tuareg-error-chars-regexp
;;   ".*, .*, [^\0-@]+ \\([0-9]+\\)-\\([0-9]+\\):"
;;   "Regexp matching the char numbers in an error message produced by ocamlc.")

;; Wrapper around next-error.

;; itz 04-21-96 instead of defining a new function, use defadvice
;; that way we get our effect even when we do \C-x` in compilation buffer

;; smclaughlin 07-19-11 defadvice is to be avoided.  It makes debugging
;; much more difficult.  If you really want this behavior, write your
;; own next-error-function.  In particular, it breaks when omake is
;; used.

;; (defadvice next-error (after tuareg-next-error activate)
;;  "Read the extra positional information provided by the OCaml compiler.

;; Puts the point and the mark exactly around the erroneous program
;; fragment. The erroneous fragment is also temporarily highlighted if
;; possible."
;;  (when (eq major-mode 'tuareg-mode)
;;    (let ((beg nil) (end nil))
;;      (with-current-buffer compilation-last-buffer
;;        (save-excursion
;;          (goto-char (window-point (get-buffer-window (current-buffer) t)))
;;          (when (looking-at tuareg-error-chars-regexp)
;;            (setq beg (string-to-number (tuareg-match-string 1))
;;                  end (string-to-number (tuareg-match-string 2))))))
;;      (beginning-of-line)
;;      (when beg
;;        (setq beg (+ (point) beg) end (+ (point) end))
;;        (goto-char beg) (push-mark end t t)))))

(autoload 'ocaml-module-alist "caml-help")
(autoload 'ocaml-visible-modules "caml-help")
(autoload 'ocaml-module-symbols "caml-help")

(defun tuareg-completion-at-point ()
  (let ((beg (save-excursion (skip-syntax-backward "w_") (point)))
        (end (save-excursion (skip-syntax-forward "w_") (point)))
        (table
         (lambda (string pred action)
           (let ((dot (string-match "\\.[^.]*\\'" string))
                 ;; ocaml-module-symbols contains an unexplained call to
                 ;; pop-to-buffer within save-window-excursion.  Let's try and
                 ;; avoid it pops up a stupid frame.
                 (special-display-buffer-names
                  (cons '("*caml-help*" (same-frame . t))
                        special-display-buffer-names)))
             (if (eq (car-safe action) 'boundaries)
                 `(boundaries ,(if dot (1+ dot) 0)
                              ,@(string-match "\\." (cdr action)))
               (if (null dot)
                   (complete-with-action
                    action (apply #'append
                                  (mapcar (lambda (mod) (concat (car mod) "."))
                                          (ocaml-module-alist))
                                  (mapcar #'ocaml-module-symbols
                                          (ocaml-visible-modules)))
                    string pred)
                 (completion-table-with-context
                  (substring string 0 (1+ dot))
                  (ocaml-module-symbols
                   (assoc (substring string 0 dot) (ocaml-module-alist)))
                  (substring string (1+ dot)) pred action)))))))
    (unless (or (eq beg end)
                (not tuareg-with-caml-mode-p))
      (list beg end table))))


(autoload 'caml-complete "caml-help")

(defun tuareg-complete (arg)
  "Completes qualified ocaml identifiers."
  (interactive "p")
  (modify-syntax-entry ?_ "w" tuareg-mode-syntax-table)
  (unwind-protect
      (caml-complete arg)
    (modify-syntax-entry ?_ "_" tuareg-mode-syntax-table)))

(defun tuareg--try-find-alternate-file (mod-name extension &optional no-create)
  "Switch to the file given by MOD-NAME and EXTENSION.
If NO-CREATE is non-nil and the file doesn't exist, don't switch and return nil,
otherwise return non-nil."
  (let* ((filename (concat mod-name extension))
         (buffer (get-file-buffer filename))
         (what (cond
                ((string= extension ".ml") "implementation")
                ((string= extension ".mli") "interface"))))
    (cond
     (buffer (switch-to-buffer buffer))
     ((file-exists-p filename) (find-file filename))
     ((and (not no-create)
           (y-or-n-p
            (format "Create %s file %s " what
                    (file-name-nondirectory filename))))
      (find-file filename)))))

(defun tuareg-find-alternate-file ()
  "Switch Implementation/Interface."
  (interactive)
  (let ((name buffer-file-name))
    (when (string-match "\\`\\(.*\\)\\.ml\\([il]\\)?\\'" name)
      (let ((mod-name (tuareg-match-string 1 name))
            (e (tuareg-match-string 2 name)))
        (cond
         ((string= e "i")
            (tuareg--try-find-alternate-file mod-name ".ml"))
         (t
          (tuareg--try-find-alternate-file mod-name ".mli")))))))

(define-skeleton tuareg-insert-class-form
  "Insert a nicely formatted class-end form, leaving a mark after end."
  nil
  \n "class " @ " = object (self)" > \n
  "inherit " > _ " as super" \n "end;;" > \n)

(define-skeleton tuareg-insert-begin-form
  "Insert a nicely formatted begin-end form, leaving a mark after end."
  nil
  \n "begin" > \n _ \n "end" > \n)

(define-skeleton tuareg-insert-for-form
  "Insert a nicely formatted for-to-done form, leaving a mark after done."
  nil
  \n "for " - " do" > \n _ \n "done" > \n)

(define-skeleton tuareg-insert-while-form
  "Insert a nicely formatted for-to-done form, leaving a mark after done."
  nil
  \n "while " - " do" > \n _ \n "done" > \n)

(define-skeleton tuareg-insert-if-form
  "Insert a nicely formatted if-then-else form, leaving a mark after else."
  nil
  \n "if" > \n _ \n "then" > \n @ \n "else" \n @)

(define-skeleton tuareg-insert-match-form
  "Insert a nicely formatted math-with form, leaving a mark after with."
  nil
  \n "match" > \n _ \n "with" > \n)

(define-skeleton tuareg-insert-let-form
  "Insert a nicely formatted let-in form, leaving a mark after in."
  nil
  \n "let " > _ " in" > \n)

(define-skeleton tuareg-insert-try-form
  "Insert a nicely formatted try-with form, leaving a mark after with."
  nil
  \n "try" > \n _ \n "with" > \n)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               OPAM

(defconst tuareg-opam-compilers
  (when (file-directory-p "~/.opam")
    (cons "~/.opam/system"
          (directory-files "~/.opam" t "[0-9]+\\.[0-9]+\\.[0-9]+")))
  "The list of OPAM directories for the installed compilers.")

(defvar tuareg-opam
  (let ((opam (executable-find "opam")))
    (if opam opam
      (let ((opam (locate-file "bin/opam" tuareg-opam-compilers)))
        (if (and opam (file-executable-p opam)) opam)))) ; or nil
  "The full path of the opam executable.")

(when tuareg-opam
  (setq tuareg-interactive-program
        (concat tuareg-opam " config exec -- ocaml"))

  (defun tuareg-opam-config-env()
    (let* ((get-env (concat tuareg-opam " config env"))
           (opam-env (shell-command-to-string get-env)))
      (replace-regexp-in-string "; *export.*$" "" opam-env)))

  ;; OPAM compilation — one must update to the current compiler
  ;; before launching the compilation.
  (defadvice compile (before tuareg-compile-opam activate)
      "Run opam to update environment variables."
      (let* ((env (tuareg-opam-config-env)))
	(set (make-local-variable 'compilation-environment)
	     ;; Quotes MUST be removed.
	     (split-string (replace-regexp-in-string "\"" "" env)))))

  (eval-after-load "merlin"
    (defun merlin-command ()
      "Return path of ocamlmerlin binary using the opam executable
detected by Tuareg"
      (if (equal merlin-command 'opam)
          (let* ((opam (concat tuareg-opam " config var bin"))
                 (bin (replace-regexp-in-string
                       "\n$" "" (shell-command-to-string opam)))
                 (merlin (concat bin "/ocamlmerlin")))
            (if (file-executable-p merlin) merlin "ocamlmerlin"))
        merlin-command)))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            Tuareg interactive mode

;; Augment Tuareg mode with an OCaml toplevel.

(require 'comint)

(defvar tuareg-interactive-mode-map
  (let ((map (copy-keymap comint-mode-map)))
    (define-key map "|" 'tuareg-electric-pipe)
    (define-key map ")" 'tuareg-electric-rp)
    (define-key map "}" 'tuareg-electric-rc)
    (define-key map "]" 'tuareg-electric-rb)
    (define-key map "\C-c\C-i" 'tuareg-interrupt-ocaml)
    (define-key map "\C-c\C-k" 'tuareg-kill-ocaml)
    (define-key map "\C-c`" 'tuareg-interactive-next-error-toplevel)
    (define-key map "\C-c?" 'tuareg-interactive-next-error-toplevel)
    (define-key map "\C-m" 'tuareg-interactive-send-input)
    (define-key map "\C-j" 'tuareg-interactive-send-input-or-indent)
    (define-key map "\M-\C-m" 'tuareg-interactive-send-input-end-of-phrase)
    (define-key map [kp-enter] 'tuareg-interactive-send-input-end-of-phrase)
    map))

(defconst tuareg-interactive-buffer-name "*ocaml-toplevel*")

(defconst tuareg-interactive-error-range-regexp
  "[ \t]*Characters \\([0-9]+\\)-\\([1-9][0-9]*\\):\n"
  "Regexp matching the char numbers in OCaml toplevel's error messages.")

(defconst tuareg-interactive-error-regexp
  "\n\\(Error: [^#]*\\)")
(defconst tuareg-interactive-exception-regexp
  "\\(Exception: [^#]*\\)")

(defvar tuareg-interactive-last-phrase-pos-in-source 0)
(defvar tuareg-interactive-last-phrase-pos-in-toplevel 0)

(defun tuareg-interactive-filter (_text)
  (when (eq major-mode 'tuareg-interactive-mode)
    (save-excursion
      (when (>= comint-last-input-end comint-last-input-start)
        (when tuareg-interactive-read-only-input
          (add-text-properties
           comint-last-input-start comint-last-input-end
           (list 'read-only t)))
        (when (and font-lock-mode tuareg-interactive-input-font-lock)
          (font-lock-fontify-region comint-last-input-start
                                    comint-last-input-end))
        (when tuareg-interactive-output-font-lock
          (save-excursion
            (goto-char (point-max))
            (re-search-backward comint-prompt-regexp
                                comint-last-input-end t)
            (add-text-properties
             comint-last-input-end (point)
             '(font-lock-face tuareg-font-lock-interactive-output-face))))
        (when tuareg-interactive-error-font-lock
          (save-excursion
            (goto-char comint-last-input-end)
            (cond
             ((looking-at tuareg-interactive-error-range-regexp)
              (let ((beg (string-to-number (tuareg-match-string 1)))
                    (end (string-to-number (tuareg-match-string 2))))
                (put-text-property
                 (+ comint-last-input-start beg)
                 (+ comint-last-input-start end)
                 'font-lock-face 'tuareg-font-lock-error-face))
              (goto-char comint-last-input-end)
              (when (re-search-forward tuareg-interactive-error-regexp nil t)
                (let ((errbeg (match-beginning 1))
                      (errend (match-end 1)))
                (put-text-property
                 errbeg errend
                 'font-lock-face 'tuareg-font-lock-interactive-error-face))))
             ((looking-at tuareg-interactive-exception-regexp)
              (let ((errbeg (match-beginning 1))
                    (errend (match-end 1)))
                (put-text-property
                 errbeg errend
                 'font-lock-face 'tuareg-font-lock-interactive-error-face)))
             )))))))

(easy-menu-define
  tuareg-interactive-mode-menu tuareg-interactive-mode-map
  "Tuareg Interactive Mode Menu."
  '("Tuareg"
    ("Interactive Mode"
     ["Run OCaml Toplevel" tuareg-run-ocaml t]
     ["Interrupt OCaml Toplevel" tuareg-interrupt-ocaml
      :active (comint-check-proc tuareg-interactive-buffer-name)]
     ["Kill OCaml Toplevel" tuareg-kill-ocaml
      :active (comint-check-proc tuareg-interactive-buffer-name)]
     ["Evaluate Region" tuareg-eval-region :active (region-active-p)]
     ["Evaluate Phrase" tuareg-eval-phrase t]
     ["Evaluate Buffer" tuareg-eval-buffer t])
    "---"
    ["Customize Tuareg Mode..." (customize-group 'tuareg) t]
    ("Tuareg Options" ["Dummy" nil t])
    ("Tuareg Interactive Options" ["Dummy" nil t])
    "---"
    ["About" tuareg-about t]
    ["Help" tuareg-interactive-help t]))

(define-derived-mode tuareg-interactive-mode comint-mode "Tuareg-Interactive"
  "Major mode for interacting with an OCaml process.
Runs an OCaml toplevel as a subprocess of Emacs, with I/O through an
Emacs buffer. A history of input phrases is maintained. Phrases can
be sent from another buffer in tuareg mode.

Short cuts for interactions with the toplevel:
\\{tuareg-interactive-mode-map}"
  (add-hook 'comint-output-filter-functions 'tuareg-interactive-filter)
  (setq comint-prompt-regexp "^#  *")
  (setq comint-process-echoes nil)
  (setq comint-get-old-input 'tuareg-interactive-get-old-input)
  (setq comint-scroll-to-bottom-on-output
        tuareg-interactive-scroll-to-bottom-on-output)
  (set-syntax-table tuareg-mode-syntax-table)
  (set (make-local-variable 'comment-start) "(* ")
  (set (make-local-variable 'comment-end) " *)")
  (set (make-local-variable 'comment-start-skip) "(\\*+[ \t]*")
  (set (make-local-variable 'comint-prompt-read-only) t)

  (tuareg--common-mode-setup)
  (when (or tuareg-interactive-input-font-lock
            tuareg-interactive-output-font-lock
            tuareg-interactive-error-font-lock)
    (font-lock-mode 1))
  (when (boundp 'after-change-functions) ;FIXME: Why?
    (remove-hook 'after-change-functions 'font-lock-after-change-function t))
  (when (boundp 'pre-idle-hook)
    (remove-hook 'pre-idle-hook 'font-lock-pre-idle-hook t))

  (easy-menu-add tuareg-interactive-mode-menu)
  (tuareg-update-options-menu))

;;;###autoload
(defun tuareg-run-ocaml ()
  "Run an OCaml toplevel process.  I/O via buffer `*ocaml-toplevel*'."
  (interactive)
  (tuareg-run-process-if-needed)
  (display-buffer tuareg-interactive-buffer-name))

;;;###autoload
(defalias 'run-ocaml 'tuareg-run-ocaml)

;;;###autoload
(add-to-list 'interpreter-mode-alist '("ocamlrun" . tuareg-mode))
;;;###autoload
(add-to-list 'interpreter-mode-alist '("ocaml" . tuareg-mode))

(defun tuareg-run-process-if-needed (&optional cmd)
  "Run an OCaml toplevel process if needed, with an optional command name.
I/O via buffer `*ocaml-toplevel*'."
  (if cmd
      (setq tuareg-interactive-program cmd)
    (unless (comint-check-proc tuareg-interactive-buffer-name)
      (setq tuareg-interactive-program
            (read-shell-command "OCaml toplevel to run: "
                                tuareg-interactive-program))))
  (unless (comint-check-proc tuareg-interactive-buffer-name)
    (let ((cmdlist (tuareg-args-to-list tuareg-interactive-program))
          (process-connection-type nil))
      (set-buffer (apply (function make-comint) "ocaml-toplevel"
                         (car cmdlist) nil (cdr cmdlist)))
      (tuareg-interactive-mode)
      (sleep-for 1))))

(defun tuareg-args-to-list (string)
  (let ((where (string-match "[ \t]" string)))
    (cond ((null where) (list string))
          ((/= where 0)
           (cons (substring string 0 where)
                 (tuareg-args-to-list (substring string (+ 1 where)
                                                 (length string)))))
          (t (let ((pos (string-match "[^ \t]" string)))
               (when pos
                 (tuareg-args-to-list (substring string pos
                                                 (length string)))))))))

(defun tuareg-interactive-get-old-input ()
  (save-excursion
    (let ((end (point)))
      (re-search-backward comint-prompt-regexp (point-min) t)
      (when (looking-at comint-prompt-regexp)
        (re-search-forward comint-prompt-regexp))
      (buffer-substring-no-properties (point) end))))

(defun tuareg-interactive-end-of-phrase ()
  (save-excursion
    (end-of-line)
    (tuareg-find-meaningful-word)
    (tuareg-find-meaningful-word)
    (looking-at ";;")))

(defun tuareg-interactive-send-input-end-of-phrase ()
  (interactive)
  (goto-char (point-max))
  (unless (tuareg-interactive-end-of-phrase)
    (insert ";;"))
  (comint-send-input))

(defconst tuareg-interactive-send-warning
  "Note: toplevel processing requires a terminating `;;'")

(defun tuareg-interactive-send-input ()
  "Process if the current line ends with `;;' then send the
current phrase else insert a newline."
  (interactive)
  (if (tuareg-interactive-end-of-phrase)
      (progn
        (comint-send-input)
        (goto-char (point-max)))
    (insert "\n")
    (message tuareg-interactive-send-warning)))

(defun tuareg-interactive-send-input-or-indent ()
  "Process if the current line ends with `;;' then send the
current phrase else insert a newline and indent."
  (interactive)
  (if (tuareg-interactive-end-of-phrase)
      (progn
        (goto-char (point-max))
        (comint-send-input))
    (insert "\n")
    (indent-according-to-mode)
    (message tuareg-interactive-send-warning)))

(defun tuareg-eval-region (start end)
  "Eval the current region in the OCaml toplevel."
  (interactive "r")
  (save-excursion (tuareg-run-process-if-needed))
  (comint-preinput-scroll-to-bottom)
  (setq tuareg-interactive-last-phrase-pos-in-source start)
  (save-excursion
    (goto-char start)
    (tuareg-skip-blank-and-comments)
    (setq start (point))
    (goto-char end)
    (tuareg-skip-to-end-of-phrase)
    (setq end (point))
    (let ((text (buffer-substring-no-properties start end)))
      (goto-char end)
      (if (string= text "")
          (message "Cannot send empty commands to OCaml toplevel!")
        (set-buffer tuareg-interactive-buffer-name)
        (goto-char (point-max))
        (setq tuareg-interactive-last-phrase-pos-in-toplevel (point))
        (comint-send-string tuareg-interactive-buffer-name
                            (concat text ";;"))
        (let ((pos (point)))
          (comint-send-input)
          (when tuareg-interactive-echo-phrase
            (save-excursion
              (goto-char pos)
              (insert (concat text ";;")))))))
    (when tuareg-display-buffer-on-eval
      (display-buffer tuareg-interactive-buffer-name))))

(when tuareg-use-smie
  (defconst tuareg-beginning-of-phrase-syms
    '("module" "open" "include" "type" "d-let"))

  (defconst tuareg-beginning-of-phrase-syms-re
    (concat (regexp-opt '("open" "include") 'words) " *")
    "A regular expression matching tokens at beginning of a phrase for
which `smie-backward-sexp' returns `nil'.")

  (defun tuareg--beginning-of-phrase ()
    (while
        (let ((td (smie-backward-sexp 'halfsexp)))
          (cond
           ((member (nth 2 td) tuareg-beginning-of-phrase-syms)
            (goto-char (nth 1 td))
            nil)
           ;; When we are after, say, "open X", `td' is `nil'
           ((and (null td)
                 (looking-back tuareg-beginning-of-phrase-syms-re))
            (tuareg-smie-backward-token)
            nil)
           ((and (car td) (not(numberp (car td))))
            (unless (bobp) (goto-char (nth 1 td)) t))
           (t t)))))

    (defun tuareg-discover-phrase (&optional quiet stop-at-and)
      "Return a triplet '(begin end end-with-comments)."
      (save-excursion
        (end-of-line)
        (tuareg--beginning-of-phrase)
        (let ((begin (point)))
          (smie-forward-sexp 'halfsexp)
          (let ((end (point)))
            (forward-comment 5)
            (list begin end (point)))))))

(defun tuareg-narrow-to-phrase ()
  "Narrow the editting window to the surrounding OCaml phrase (or block)."
  (interactive)
  (save-excursion
    (let ((pair (tuareg-discover-phrase)))
      (narrow-to-region (nth 0 pair) (nth 1 pair)))))

(defun tuareg-eval-phrase ()
  "Eval the surrounding OCaml phrase (or block) in the Caml toplevel."
  (interactive)
  (let ((end))
    (save-excursion
      (let ((pair (tuareg-discover-phrase)))
        (setq end (nth 2 pair))
        (tuareg-eval-region (nth 0 pair) (nth 1 pair))))
    (when tuareg-skip-after-eval-phrase
      (goto-char end))))

(defun tuareg-eval-buffer ()
  "Send the buffer to the Tuareg Interactive process."
  (interactive)
  (tuareg-eval-region (point-min) (point-max)))

(defvar tuareg-interactive-next-error-olv (make-overlay 1 1))
(overlay-put tuareg-interactive-next-error-olv
             'face 'tuareg-font-lock-error-face)
(delete-overlay tuareg-interactive-next-error-olv)

(defun tuareg-interactive-next-error-source ()
  (interactive)
  (let ((error-pos) (beg 0) (end 0))
    (with-current-buffer tuareg-interactive-buffer-name
      (goto-char tuareg-interactive-last-phrase-pos-in-toplevel)
      (setq error-pos
            (re-search-forward tuareg-interactive-error-range-regexp
                               (point-max) t))
      (when error-pos
        (setq beg (string-to-number (tuareg-match-string 1))
              end (string-to-number (tuareg-match-string 2)))))
    (if (not error-pos)
        (message "No syntax or typing error in last phrase.")
      (setq beg (+ tuareg-interactive-last-phrase-pos-in-source beg)
            end (+ tuareg-interactive-last-phrase-pos-in-source end))
      (goto-char beg)
      (move-overlay tuareg-interactive-next-error-olv beg end)
      (unwind-protect
          (sit-for 60 t)
        (delete-overlay tuareg-interactive-next-error-olv))
      )))

(defun tuareg-interactive-next-error-toplevel ()
  (interactive)
  (let ((error-pos) (beg 0) (end 0))
    (save-excursion
      (goto-char tuareg-interactive-last-phrase-pos-in-toplevel)
      (setq error-pos
            (re-search-forward tuareg-interactive-error-range-regexp
                               (point-max) t))
      (when error-pos
        (setq beg (string-to-number (tuareg-match-string 1))
              end (string-to-number (tuareg-match-string 2)))))
    (if (not error-pos)
        (message "No syntax or typing error in last phrase.")
      (setq beg (+ tuareg-interactive-last-phrase-pos-in-toplevel beg)
            end (+ tuareg-interactive-last-phrase-pos-in-toplevel end))
      (move-overlay tuareg-interactive-next-error-olv beg end)
      (unwind-protect
          (sit-for 60 t)
        (delete-overlay tuareg-interactive-next-error-olv))
      (goto-char beg))))

(defun tuareg-interrupt-ocaml ()
  (interactive)
  (when (comint-check-proc tuareg-interactive-buffer-name)
    (with-current-buffer tuareg-interactive-buffer-name
      (comint-interrupt-subjob))))

(defun tuareg-kill-ocaml ()
  (interactive)
  (when (comint-check-proc tuareg-interactive-buffer-name)
    (with-current-buffer tuareg-interactive-buffer-name
      (comint-kill-subjob))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Menu support

(defun tuareg-about ()
  (interactive)
  (describe-variable 'tuareg-mode-version))

(defun tuareg-short-cuts ()
  "Short cuts for the Tuareg mode:
\\{tuareg-mode-map}

Short cuts for interaction within the toplevel:
\\{tuareg-interactive-mode-map}"
  (interactive)
  (describe-function 'tuareg-short-cuts))

(defun tuareg-help ()
  (interactive)
  (describe-function 'tuareg-mode))

(defun tuareg-interactive-help ()
  (interactive)
  (describe-function 'tuareg-interactive-mode))

(defvar tuareg-definitions-menu (list ["Scan..." tuareg-list-definitions t])
  "Initial content of the definitions menu.")
(make-variable-buffer-local 'tuareg-definitions-menu)

(defvar tuareg-definitions-menu-last-buffer nil)
(defvar tuareg-definitions-keymaps nil)

(defun tuareg-build-menu ()
  (easy-menu-define
   tuareg-mode-menu (list tuareg-mode-map)
   "Tuareg Mode Menu."
   '("Tuareg"
     ("Interactive Mode"
      ["Run OCaml Toplevel" tuareg-run-ocaml t]
      ["Interrupt OCaml Toplevel" tuareg-interrupt-ocaml
       :active (comint-check-proc tuareg-interactive-buffer-name)]
      ["Kill OCaml Toplevel" tuareg-kill-ocaml
       :active (comint-check-proc tuareg-interactive-buffer-name)]
      ["Evaluate Region" tuareg-eval-region
       ;; Region-active-p for XEmacs and mark-active for Emacs
       :active mark-active]
      ["Evaluate Phrase" tuareg-eval-phrase t]
      ["Evaluate Buffer" tuareg-eval-buffer t])
     ("OCaml Forms"
      ["try .. with .." tuareg-insert-try-form t]
      ["match .. with .." tuareg-insert-match-form t]
      ["let .. in .." tuareg-insert-let-form t]
      ["if .. then .. else .." tuareg-insert-if-form t]
      ["while .. do .. done" tuareg-insert-while-form t]
      ["for .. do .. done" tuareg-insert-for-form t]
      ["begin .. end" tuareg-insert-begin-form t])
     ["Switch .ml/.mli" tuareg-find-alternate-file t]
     "---"
     ["Compile..." compile t]
     ["Reference Manual..." tuareg-browse-manual t]
     ["OCaml Library..." tuareg-browse-library t]
     ("Definitions"
      ["Scan..." tuareg-list-definitions t])
     "---"
     [ "Show type at point" caml-types-show-type
       tuareg-with-caml-mode-p]
     [ "Show fully qualified ident at point" caml-types-show-ident
       tuareg-with-caml-mode-p]
     [ "Show the kind of call at point" caml-types-show-call
       tuareg-with-caml-mode-p]
     "---"
     [ "Complete identifier" caml-complete
       tuareg-with-caml-mode-p]
     [ "Help for identifier" caml-help
       tuareg-with-caml-mode-p]
     [ "Add path for documentation" ocaml-add-path
       tuareg-with-caml-mode-p]
     [ "Open module for documentation" ocaml-open-module
       tuareg-with-caml-mode-p]
     [ "Close module for documentation" ocaml-close-module
       tuareg-with-caml-mode-p]
     "---"
     ["Customize Tuareg Mode..." (customize-group 'tuareg) t]
     ("Tuareg Options" ["Dummy" nil t])
     ("Tuareg Interactive Options" ["Dummy" nil t])
     "---"
     ["About" tuareg-about t]
     ["Short Cuts" tuareg-short-cuts]
     ["Help" tuareg-help t]))
  (easy-menu-add tuareg-mode-menu)
  (tuareg-update-options-menu))

(defun tuareg-update-definitions-menu ()
  (when (eq major-mode 'tuareg-mode)
    (easy-menu-change
     '("Tuareg") "Definitions"
     tuareg-definitions-menu)))

(defun tuareg-toggle-option (symbol)
  (interactive)
  (set symbol (not (symbol-value symbol)))
  (when (eq 'tuareg-use-abbrev-mode symbol)
    (abbrev-mode tuareg-use-abbrev-mode)) ; toggle abbrev minor mode
  (tuareg-update-options-menu))

(defun tuareg-update-options-menu ()
  (easy-menu-change
   '("Tuareg") "Tuareg Options"
   (mapcar (lambda (pair)
             (if (consp pair)
                 (vector (car pair)
                         (list 'tuareg-toggle-option (cdr pair))
                         ':style 'toggle
                         ':selected (nth 1 (cdr pair))
                         ':active t)
               pair)) tuareg-options-list))
  (easy-menu-change
   '("Tuareg") "Tuareg Interactive Options"
   (mapcar (lambda (pair)
             (if (consp pair)
                 (vector (car pair)
                         (list 'tuareg-toggle-option (cdr pair))
                         ':style 'toggle
                         ':selected (nth 1 (cdr pair))
                         ':active t)
               pair)) tuareg-interactive-options-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Browse Manual

;; From M. Quercia

(defun tuareg-browse-manual ()
  "*Browse OCaml reference manual."
  (interactive)
  (setq tuareg-manual-url (read-from-minibuffer "URL: " tuareg-manual-url))
  (funcall tuareg-browser tuareg-manual-url))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Browse Library

;; From M. Quercia

(defvar tuareg-library-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map [return] 'tuareg-library-find-file)
    (define-key map [mouse-2] 'tuareg-library-mouse-find-file)
    map))

(defun tuareg-browse-library ()
  "Browse the OCaml library."
  (interactive)
  (let ((buf-name "*ocaml-library*") (opoint)
        (dir (read-from-minibuffer "Library path: " tuareg-library-path)))
    (when (and (file-directory-p dir) (file-readable-p dir))
      (setq tuareg-library-path dir)
      ;; List *.ml and *.mli files
      (with-output-to-temp-buffer buf-name
        (buffer-disable-undo standard-output)
        (with-current-buffer buf-name
          (kill-all-local-variables)
          (set (make-local-variable 'tuareg-library-path) dir)
          ;; Help
          (insert "Directory \"" dir "\".\n")
          (insert "Select a file with middle mouse button or RETURN.\n\n")
          (insert "Interface files (.mli):\n\n")
          (insert-directory (concat dir "/*.mli") "-C" t nil)
          (insert "\n\nImplementation files (.ml):\n\n")
          (insert-directory (concat dir "/*.ml") "-C" t nil)
          ;; '.', '-' and '_' are now letters
          (modify-syntax-entry ?. "w")
          (modify-syntax-entry ?_ "w")
          (modify-syntax-entry ?- "w")
          ;; Every file name is now mouse-sensitive
          (goto-char (point-min))
          (while (< (point) (point-max))
            (re-search-forward "\\.ml.?\\>")
            (setq opoint (point))
            (re-search-backward "\\<" (point-min) 1)
            (put-text-property (point) opoint 'mouse-face 'highlight)
            (goto-char (+ 1 opoint)))
          ;; Activate tuareg-library mode
          (setq major-mode 'tuareg-library-mode)
          (setq mode-name "tuareg-library")
          (use-local-map tuareg-library-mode-map)
          (setq buffer-read-only t))))))

(defun tuareg-library-find-file ()
  "Load the file whose name is near point."
  (interactive)
  (when (text-properties-at (point))    ;FIXME: Why??
    (save-excursion
      (let (beg)
        (re-search-backward "\\<") (setq beg (point))
        (re-search-forward "\\>")
        (find-file-read-only (expand-file-name (buffer-substring-no-properties
                                                beg (point))
                                               tuareg-library-path))))))

(defun tuareg-library-mouse-find-file (event)
  "Visit the file name you click on."
  (interactive "e")
  (let ((owindow (selected-window)))
    (mouse-set-point event)
    (tuareg-library-find-file)
    (select-window owindow)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Definitions List

;; Designed from original code by M. Quercia

(defconst tuareg--id-regexp "[[:alpha:]][_'[:alnum:]]*")

(defconst tuareg-definitions-regexp
  (regexp-opt '("and" "val" "type" "module" "class" "exception" "let") 'words)
  "Regexp matching definition phrases.")

(defconst tuareg-definitions-bind-skip-regexp
  (concat (regexp-opt '("rec" "type" "virtual") 'words) "\\|'"
          tuareg--id-regexp "\\|('.*)")
  "Regexp matching stuff to ignore after a binding keyword.")

(defconst tuareg-identifier-regexp (concat "\\<" tuareg--id-regexp "\\>"))

(defun tuareg-imenu-create-index ()
  (let ((cpt (if (fboundp 'make-progress-reporter)
                 (make-progress-reporter "Searching definitions..."
                                         (point-min) (point-max))
               0))
        (kw) (value-list) (type-list) (module-list) (class-list) (misc-list))
    (goto-char (point-min))
    (tuareg-skip-blank-and-comments)
    (while (not (eobp))
      (when (looking-at tuareg-definitions-regexp)
        (setq kw (tuareg-match-string 0))
        (save-match-data (tuareg-reset-and-kwop kw))
        (when (member kw '("exception" "val"))
          (setq kw "let"))
        ;; Skip optional elements
        (goto-char (match-end 0))
        (tuareg-skip-blank-and-comments)
        (when (looking-at tuareg-definitions-bind-skip-regexp)
          (goto-char (match-end 0)))
        (tuareg-skip-blank-and-comments)
        (when (looking-at tuareg-identifier-regexp)
          (let ((ref (cons (tuareg-match-string 0) (point-marker))))
            (if (not (integerp cpt))
                (progress-reporter-update cpt (point))
              (setq cpt (1+ cpt))
              (message "Searching definitions... (%d)" cpt))
            (cond ((string= kw "let")
                   (setq value-list (cons ref value-list)))
                  ((string= kw "type")
                   (setq type-list (cons ref type-list)))
                  ((string= kw "module")
                   (setq module-list (cons ref module-list)))
                  ((string= kw "class")
                   (setq class-list (cons ref class-list)))
                  (t (setq misc-list (cons ref misc-list)))))))
      ;; Skip to next phrase or next top-level `and'
      (tuareg-forward-char)
      (let ((old-point (point))
            (last-and (progn (tuareg-next-phrase t t) (point))))
        (when (< last-and old-point) (error "Scan error"))
        (save-excursion
          (while (and (re-search-backward "\\<and\\>" old-point t)
                      (not (tuareg-in-literal-or-comment-p))
                      (save-excursion (tuareg-find-and-match)
                                      (>= old-point (point))))
            (setq last-and (point))))
        (goto-char last-and)))
    (if (integerp cpt)
        (message "Searching definitions... done")
      (progress-reporter-done cpt))
    (let ((index ()))
      (when module-list (push (cons "Modules" module-list) index))
      (when type-list   (push (cons "Types" type-list) index))
      (when class-list  (push (cons "Classes" class-list) index))
      (when value-list  (push (cons "Values" value-list) index))
      (when misc-list   (push (cons "Miscellaneous" misc-list) index))
      index)))

(defun tuareg-list-definitions ()
  "Parse the buffer and gather toplevel definitions
for a quick jump via the definitions menu."
  (interactive)
  (let ((defs (save-excursion (tuareg-imenu-create-index)))
        menu)
    ;; Sort and build lists
    (dolist (pair defs)
      (let ((entries (mapcar (lambda (elem)
                               (vector (car elem)
                                       (list 'tuareg-goto (cdr elem))
                                       t))
                             (cdr pair))))
        (setq menu
              (append (tuareg-split-long-list
                       (car pair) (tuareg-sort-definitions entries))
                      menu))))
    ;; Update definitions menu
    (setq tuareg-definitions-menu
          (append menu (list "---"
                             ["Rescan..." tuareg-list-definitions t]))))
  (tuareg-update-definitions-menu))

(defun tuareg-goto (pos)
  (goto-char pos)
  (recenter))

(defun tuareg-sort-definitions (list)
  (let* ((last "") (cpt 1)
         (list (sort (nreverse list)    ;FIXME: Why reverse before sorting?
                     (lambda (p q) (string< (elt p 0) (elt q 0)))))
         (tail list))
    (while tail
      (if (string= (elt (car tail) 0) last)
          (progn
            (setq cpt (1+ cpt))
            (aset (car tail) 0 (format "%s (%d)" last cpt)))
        (setq cpt 1)
        (setq last (elt (car tail) 0)))
      (setq tail (cdr tail)))
    list))

;; Split a definition list if it is too long
(defun tuareg-split-long-list (title list)
  (let ((tail (nthcdr tuareg-definitions-max-items list)))
    (if (null (cdr tail))
        ;; List not too long, cons the title
        (list (cons title list))
      ;; List too long, split and add initials to the title
      (let (lists)
        (while list
          (let ((beg (substring (elt (car list) 0) 0 1))
                (end (substring (elt (car tail) 0) 0 1)))
            (setq lists (cons
                         (cons (format "%s %s-%s" title beg end) list)
                         lists))
            (setq list (cdr tail))
            (setcdr tail nil)
            (setq tail (nthcdr tuareg-definitions-max-items list))))
        (nreverse lists)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Hooks and Exit

(eval-when-compile
  (autoload 'speedbar-add-supported-extension "speedbar"))
(when (require 'speedbar nil t)
  (speedbar-add-supported-extension
   '(".ml" ".mli" ".mll" ".mly" ".ls")))

(provide 'tuareg)

;; Pre-SMIE indentation functions.
;; Load it after providing `tuareg' to avoid circular dependencies.
(require 'tuareg_indent)

;; For compatibility with caml support modes
;; you may also link caml.el to tuareg.el
(provide 'caml)

;;; tuareg.el ends here
