;;; helm-cscope.el --- Helm interface for xcscope.el. -*- lexical-binding: t -*-

;; Copyright (C) 2015 alpha22jp <alpha22jp@gmail.com>

;; Author: alpha22jp <alpha22jp@gmail.com>
;; URL: https://github.com/alpha22jp/helm-cscope.el
;; Package-Version: 20150609.649
;; Keywords: cscope, helm
;; Version: 0.1.1
;; Package-Requires: ((xcscope "1.0") (helm "1.6.7") (cl-lib "0.5") (emacs "24.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `helm-cscope.el' is a `helm' interface for xcscope.el.

;;; Code:

(require 'cl-lib)
(require 'helm)
(require 'xcscope)

(defgroup helm-cscope nil
  "cscope for helm"
  :group 'helm)

(defface helm-cscope-file-face
  '((t :inherit font-lock-keyword-face))
  "Face used to highlight file name in the *helm-cscope* buffer."
  :group 'helm-cscope)

(defface helm-cscope-function-face
  '((t :inherit font-lock-type-face))
  "Face used to highlight function name in the *helm-cscope* buffer."
  :group 'helm-cscope)

(defface helm-cscope-lineno-face
  '((t :inherit font-lock-doc-face))
  "Face used to highlight line number in the *helm-cscope* buffer."
  :group 'helm-cscope)

(defconst helm-cscope--parse-regexp
  "\\`\\([^ ]+\\) \\([^ ]+\\) \\([0-9]+\\) \\(.*\\)")

(defun helm-cscope--search (dir db-name search-type-arg search-text args)
  (let ((cmd-args (append
                   (list "-f" db-name "-L" search-type-arg search-text)
                   args (cscope-construct-custom-options-list))))

    ;; The database file and the directory containing the database file
    ;; must both be writable.
    (if (or (not (file-writable-p (concat dir db-name)))
            (not (file-writable-p dir))
            cscope-option-do-not-update-database)
        (push "-d" cmd-args))

    (with-current-buffer (helm-candidate-buffer 'global)
      (setq default-directory dir)
      (apply #'call-process cscope-program nil t nil cmd-args))))

(defun helm-cscope--goto-line (text line-number)
  ;; this is recommended instead of (goto-line line-number)
  (save-restriction
    (widen)
    (goto-char (point-min))
    (forward-line (1- line-number)))

  (when cscope-fuzzy-search-range
    (let ((fuzzy-search-text-regexp
           (mapconcat 'regexp-quote
                      (split-string text "[ \f\t\n\r\v]+\\|\\b" t) "\\s-*"))
          old-point new-point forward-point backward-point line-end line-length)

      (setq old-point (point))

      ;; Calculate the length of the line specified by cscope.
      (end-of-line)
      (setq line-end (point))
      (goto-char old-point)
      (setq line-length (- line-end old-point))

      ;; Search forward and backward for the pattern.
      (setq forward-point (re-search-forward
                           fuzzy-search-text-regexp
                           (+ old-point
                              cscope-fuzzy-search-range) t))
      (goto-char old-point)
      (setq backward-point (re-search-backward
                            fuzzy-search-text-regexp
                            (- old-point
                               cscope-fuzzy-search-range) t))
      (if forward-point
          (progn
            (if backward-point
                (setq new-point
                      (if (<= (- (- forward-point line-length)
                                 old-point)
                              (- old-point backward-point))
                          forward-point
                        backward-point))
              (setq new-point forward-point)))
        (if backward-point
            (setq new-point backward-point)
          (setq new-point old-point)))
      (goto-char new-point)
      (beginning-of-line))))

(defun helm-cscope--open-file (dir line &optional persistent)
  (when (string-match helm-cscope--parse-regexp line)
    (let ((file (match-string 1 line))
          (line-number (string-to-number (match-string 3 line)))
          (text (match-string 4 line)))
      (unless persistent (ring-insert cscope-marker-ring (point-marker)))
      (find-file (if (file-name-absolute-p file) file (concat dir file)))
      (helm-cscope--goto-line text line-number)
      (if persistent (helm-highlight-current-line)))))

(defun helm-cscope--filter-candidates (candidates _source)
  (cl-remove-if-not
   (lambda (e) (string-match helm-cscope--parse-regexp (cdr e)))
   candidates))

(defun helm-cscope--transform (dir line)
  (when (string-match helm-cscope--parse-regexp line)
    (let ((file (match-string 1 line)))
      (format "%s: %s(%s) %s"
              (propertize (if (file-name-absolute-p file)
                              (replace-regexp-in-string dir "" file)
                            file) 'face 'helm-cscope-file-face)
              (propertize (match-string 2 line) 'face 'helm-cscope-function-face)
              (propertize (match-string 3 line) 'face 'helm-cscope-lineno-face)
              (match-string 4 line)))))

(defun helm-cscope--make-source (dir db-name search-type-arg search-text args)
  (helm-build-in-buffer-source dir
    :init (lambda () (helm-cscope--search
                      dir db-name search-type-arg search-text args))
    :filtered-candidate-transformer 'helm-cscope--filter-candidates
    :real-to-display (lambda (line) (helm-cscope--transform dir line))
    :fuzzy-match t
    :action (lambda (line) (helm-cscope--open-file dir line))
    :persistent-action (lambda (line) (helm-cscope--open-file dir line t))))

(defalias 'helm-cscope-pop-mark 'cscope-pop-mark)

(defun helm-cscope--find-common (search-type-arg search-text)
  (let ((cur-dir (cscope-search-directory-hierarchy
                  (file-name-directory (buffer-file-name))))
        (search-dir-list
         (cl-remove-if-not
          (lambda (e) (and (listp e) (stringp (car e))))
          (cscope-find-info (file-name-directory (buffer-file-name))))))
    (unless (cl-loop for e in search-dir-list
                     thereis (string= cur-dir
                                      (cscope-canonicalize-directory (car e))))
      (push (list cur-dir) search-dir-list))
    (helm :sources
          (mapcar (lambda (e)
                    (helm-cscope--make-source
                     (file-name-directory
                      (cscope-search-directory-hierarchy (car e)))
                     (if (file-regular-p (car e))
                         (file-name-nondirectory (car e)) cscope-database-file)
                     search-type-arg search-text (cadr e)))
                  search-dir-list)
          :buffer "*Helm cscope*")))

;;;###autoload
(defun helm-cscope-find-this-symbol (symbol)
  "Locate a symbol in source code."
  (interactive
   (list (cscope-prompt-for-symbol
          "Find this symbol " nil nil t)))
  (helm-cscope--find-common "-0" symbol))

;;;###autoload
(defun helm-cscope-find-global-definition (symbol)
  "Find a symbol's global definition."
  (interactive
   (list (cscope-prompt-for-symbol
          "Find this global definition " nil nil t)))
  (helm-cscope--find-common "-1" symbol))

;;;###autoload
(defun helm-cscope-find-called-function (symbol)
  "Display functions called by a function."
  (interactive
   (list (cscope-prompt-for-symbol
          "Find functions called by this function " nil nil t)))
  (helm-cscope--find-common "-2" symbol))

;;;###autoload
(defun helm-cscope-find-calling-this-funtcion (symbol)
  "Display functions calling a function."
  (interactive
   (list (cscope-prompt-for-symbol
          "Find functions calling this function " nil nil t)))
  (helm-cscope--find-common "-3" symbol))

;;;###autoload
(defun helm-cscope-find-this-text-string (symbol)
  "Locate where a text string occurs."
  (interactive
   (list (cscope-prompt-for-symbol
          "Find this text string " nil t nil)))
  (helm-cscope--find-common "-4" symbol))

;;;###autoload
(defun helm-cscope-find-egrep-pattern (symbol)
  "Run egrep over the cscope database."
  (interactive
   (list (let (cscope-no-mouse-prompts)
           (cscope-prompt-for-symbol
            "Find this egrep pattern " nil t t))))
  (helm-cscope--find-common "-6" symbol))

;;;###autoload
(defun helm-cscope-find-this-file (symbol)
  "Locate a file."
  (interactive
   (list (let (cscope-no-mouse-prompts)
           (cscope-prompt-for-symbol
            "Find this file " t nil t))))
  (helm-cscope--find-common "-7" symbol))

;;;###autoload
(defun helm-cscope-find-files-including-file (symbol)
  "Locate all files #including a file."
  (interactive
   (list (let (cscope-no-mouse-prompts)
           (cscope-prompt-for-symbol
            "Find files #including this file " t nil nil))))
  (helm-cscope--find-common "-8" symbol))

;;;###autoload
(defun helm-cscope-find-assignments-to-this-symbol (symbol)
  "Locate assignments to a symbol in the source code."
  (interactive
   (list (cscope-prompt-for-symbol
          "Find assignments to this symbol " nil nil t)))
  (helm-cscope--find-common "-9" symbol))

(defvar helm-cscope-mode-name " Helm cscope")
(defvar helm-cscope-mode-map (make-sparse-keymap))

;;;###autoload
(define-minor-mode helm-cscope-mode ()
  "Enable for helm-cscope"
  :group      'helm-cscope
  :init-value nil
  :global     nil
  :keymap     helm-cscope-mode-map
  :lighter    helm-cscope-mode-name
  (if helm-cscope-mode
      (run-hooks 'helm-cscope-mode-hook)))

(provide 'helm-cscope)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-cscope.el ends here
