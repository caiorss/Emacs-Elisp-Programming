;;; ack.el --- Use ack where you might usually use grep.

;; Copyright (C) 2008 Philip Jackson

;; Author: Philip Jackson <phil@shellarchive.co.uk>
;; Version: 0.1

;; This file is not currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; ack.el provides a simple compilation mode for the perl grep-a-like
;; ack (http://petdance.com/ack/).

;; If `ack-guess-type' is non-nil and `ack-mode-type-map' has a
;; reasonable value then ack.el will try and guess what you would like
;; in the --type argument for ack.

;; To install/use put ack.el in your load-path and (require 'ack) in
;; your initialisation file. You can then M-x ack and you're off.

(require 'compile)

(defvar ack-guess-type t
  "Setting this value to `t' will have `ack' do its best to fill
in the --type argument to the ack command")

(defvar ack-mode-type-map
  '(((c++-mode) . "cpp")
    ((c-mode) . "cc")
    ((css-mode) . "css")
    ((emacs-lisp-mode) . "elisp")
    ((fortran-mode) . "fortran")
    ((html-mode) . "html")
    ((xml-mode nxml-mode) . "xml")
    ((java-mode) . "java")
    ((lisp-mode) . "lisp")
    ((perl-mode cperl-mode yaml-mode) . "perl"))
  "alist describing how to fill in the '--type=' argument to ack")

(defvar ack-command "ack --nocolor --nogroup "
  "The command to be run by the ack function.")

(defun ack-find-type-for-mode ()
  (catch 'found
    (dolist (mode-type ack-mode-type-map)
      (when (member major-mode (car mode-type))
        (throw 'found (cdr mode-type))))))

(defun ack-build-command ()
  (let ((type (ack-find-type-for-mode)))
    (concat ack-command
            (when (and ack-guess-type type)
              (concat "--type=" type)) " ")))

(define-compilation-mode ack-mode "Ack"
  "Ack compilation mode."
  nil)

;;;###autoload
(defvar ack-history nil)
(defvar ack-host-defaults-alist nil)
(defun ack ()
  "Like grep, but using ack-command as the default"
  (interactive)
  ; Make sure grep has been initialized
  (if (>= emacs-major-version 22)
      (require 'grep)
    (require 'compile))
  ; Close STDIN to keep ack from going into filter mode
  (let ((null-device (format "< %s" null-device))
        (grep-command ack-command)
        (grep-history ack-history)
        (grep-host-defaults-alist ack-host-defaults-alist))
    (call-interactively 'grep)
    (setq ack-history             grep-history
          ack-host-defaults-alist grep-host-defaults-alist)))

(provide 'ack)
