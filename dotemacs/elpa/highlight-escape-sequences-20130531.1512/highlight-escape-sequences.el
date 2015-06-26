;;; highlight-escape-sequences.el --- Highlight escape sequences -*- lexical-binding: t -*-

;; Author:   Dmitry Gutov <dgutov@yandex.ru>
;; URL:      https://github.com/dgutov/highlight-escape-sequences
;; Package-Version: 20130531.1512
;; Keywords: convenience
;; Version:  0.1

;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This global minor mode highlights escape sequences in strings and
;; other kinds of literals with `font-lock-regexp-grouping-backslash'
;; face when appropriate.

;; It currently supports `ruby-mode' and both main JavaScript modes.

;; To enable it elsewhere, customize `hes-simple-modes'.

;; Put this in the init file:
;;
;; (hes-mode)

;;; Code:

(defgroup hes-mode nil
  "Highlight escape sequences"
  :group 'convenience)

(defconst hes-escape-sequence-re
  "\\(\\\\\\(\\(?:[0-9]\\|x\\)\\(?:[0-9]\\(?:[0-9]\\)?\\)?\\|.\\)\\)"
  "Regexp to match an escape sequence.
Currently handles octals (\\123), hexadecimals (\\x12) and
backslash followed by anything else.")

(defconst hes-ruby-keywords
  `((,hes-escape-sequence-re
     (1 (let* ((state (syntax-ppss))
               (term (nth 3 state)))
          (when (or (and (eq term ?')
                         (member (match-string 2) '("\\" "'")))
                    (if (fboundp 'ruby-syntax-expansion-allowed-p)
                        (ruby-syntax-expansion-allowed-p state)
                      (memq term '(?\" ?/ ?\n ?` t))))
            'font-lock-regexp-grouping-backslash))
        prepend))))

(defconst hes-simple-keywords
  `((,hes-escape-sequence-re
     (1 (when (nth 3 (syntax-ppss))
          'font-lock-regexp-grouping-backslash)
        prepend))))

(defcustom hes-simple-modes '(js-mode js2-mode)
  "Modes where escape sequences can appear in any string literal."
  :type '(repeat function)
  :set (lambda (symbol value)
         (if (bound-and-true-p hes-mode)
             (progn
               (hes-mode -1)
               (set-default symbol value)
               (hes-mode 1))
           (set-default symbol value))))

;;;###autoload
(define-minor-mode hes-mode
  "Toggle highlighting of escape sequences."
  :lighter "" :global t
  (if hes-mode
      (progn
        (font-lock-add-keywords 'ruby-mode hes-ruby-keywords 'append)
        (dolist (mode hes-simple-modes)
          (font-lock-add-keywords mode hes-simple-keywords 'append)))
    (font-lock-remove-keywords 'ruby-mode hes-ruby-keywords)
    (dolist (mode hes-simple-modes)
      (font-lock-remove-keywords mode hes-simple-keywords))))

(provide 'highlight-escape-sequences)

;;; highlight-escape-sequences.el ends here
