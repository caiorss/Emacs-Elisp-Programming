;;; company-ghc.el --- company-mode ghc-mod backend -*- lexical-binding: t -*-

;; Copyright (C) 2014-2015 by Iku Iwasa

;; Author:    Iku Iwasa <iku.iwasa@gmail.com>
;; URL:       https://github.com/iquiw/company-ghc
;; Package-Version: 20150613.123
;; Version:   0.2.2
;; Package-Requires: ((cl-lib "0.5") (company "0.8.0") (ghc "4.1.1") (emacs "24"))
;; Keywords:  haskell, completion
;; Stability: experimental

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `company-mode' back-end for `haskell-mode' via `ghc-mod'.
;;
;; Provide context sensitive completion by using information from `ghc-mod'.
;; Add `company-ghc' to `company-mode' back-ends list.
;;
;;     (add-to-list 'company-backends 'company-ghc)
;;
;; or grouped with other back-ends.
;;
;;     (add-to-list 'company-backends '(company-ghc :with company-dabbrev-code))

;;; Code:

(require 'cl-lib)
(require 'company)
(require 'ghc)

(defgroup company-ghc nil
  "company-mode back-end for haskell-mode."
  :group 'company)

(defcustom company-ghc-show-info 'nomodule
  "Specify how to show type info in minibuffer."
  :type '(choice (const :tag "Off" nil)
                 (const :tag "Show raw output" t)
                 (const :tag "Show in oneline" oneline)
                 (const :tag "Show without module" nomodule)))

(defcustom company-ghc-show-module t
  "Non-nil to show module name as annotation."
  :type 'boolean)

(defcustom company-ghc-hoogle-command (or (and (boundp 'haskell-hoogle-command)
                                               haskell-hoogle-command)
                                          "hoogle")
  "Specify hoogle command name for doc-buffer support.
If `haskell-hoogle-command' is non-nil, the value is used as default."
  :type 'string)

(defcustom company-ghc-autoscan t
  "Non-nil to enable automatic scan module."
  :type 'boolean)

(defcustom company-ghc-hoogle-search-limit 20
  "Specify limit of hoogle search results."
  :type 'number)

(defcustom company-ghc-component-prefix-match nil
  "Non-nil to enable module component prefix match.
e.g. \"C.M\" to match with \"Control.Monad\", etc."
  :type 'boolean)

(defconst company-ghc-pragma-regexp "{-#[[:space:]]*\\([[:upper:]]+\\>\\|\\)")

(defconst company-ghc-langopt-regexp
  (concat "{-#[[:space:]\n]*\\(LANGUAGE\\|OPTIONS_GHC\\)[[:space:]\n]+"
          "\\(?:[^[:space:]]+,[[:space:]\n]*\\)*"
          "\\([^[:space:]]+\\|\\)"))

(defconst company-ghc-import-regexp
  (concat "import[[:space:]\n]+"
          "\\(?:safe[[:space:]\n]+\\)?"
          "\\(?:qualified[[:space:]\n]+\\)?"
          "\\(?:\"[^\"]+\"[[:space:]\n]+\\)?"
          "\\([[:word:].]+\\|\\)"))

(defconst company-ghc-impdecl-regexp
  (concat company-ghc-import-regexp
          "\\(?:[[:space:]\n]+as[[:space:]\n]+\\w+\\)?"
          "[[:space:]\n]*\\(?:hiding[[:space:]\n]\\)*("
          "\\(?:[[:space:]\n]*[[:word:]]+[[:space:]\n]*,\\)*"
          "[[:space:]\n]*\\([[:word:]]+\\_>\\|\\)"))

(defconst company-ghc-module-regexp
  "module[[:space:]]*\\([[:word:].]+\\_>\\|\\)")

(defvar company-ghc--propertized-modules '())
(defvar company-ghc--imported-modules '())
(make-variable-buffer-local 'company-ghc--imported-modules)

(defun company-ghc--find-context ()
  "Find completion context at the current point."
  (cond
   ((company-ghc--in-comment-p)
    (cond
     ((company-grab company-ghc-pragma-regexp)
      '(pragma))
     ((company-grab company-ghc-langopt-regexp)
      (and (looking-at-p "\\([#, [:space:]]\\|$\\)")
           (cons 'langopt (match-string-no-properties 1))))))

   ((company-grab company-ghc-impdecl-regexp)
    (cons 'impspec (match-string-no-properties 1)))

   ((company-grab company-ghc-import-regexp) '(module))

   ((company-grab company-ghc-module-regexp) '(module))

   (t (let ((qcons (company-ghc--grab-qualified)))
        (if qcons
            (cons 'qualified (car qcons))
          '(keyword))))))

(defun company-ghc-prefix ()
  "Provide completion prefix at the current point."
  (let ((ppss (syntax-ppss)) match)
    (cond
     ((nth 3 ppss) 'stop)
     ((nth 4 ppss)
      (if (looking-back company-ghc-pragma-regexp)
          (match-string-no-properties 1)
        (company-grab "[[:space:],]\\([^[:space:]]*\\)" 1)))
     ((looking-back "^[^[:space:]]*") nil)
     ((let ((case-fold-search nil))
        (and (save-excursion
               (forward-line 0)
               (not (looking-at-p "^import\\>")))
             (setq match (company-ghc--grab-qualified))))
      (cons (cdr match) t))
     ((looking-back "[[:word:].]+" nil t)
      (match-string-no-properties 0))
     (t (company-ghc--grab-name)))))

(defun company-ghc-candidates (prefix)
  "Provide completion candidates for the given PREFIX."
  (let ((ctx (company-ghc--find-context)))
    (pcase ctx
      (`(pragma) (all-completions prefix (company-ghc--source-pragmas)))
      (`(langopt . "LANGUAGE")
       (all-completions prefix (company-ghc--source-languages)))
      (`(langopt . "OPTIONS_GHC")
       (all-completions prefix (company-ghc--source-options)))
      (`(impspec . ,mod)
       (all-completions prefix (company-ghc--source-keywords mod)))
      (`(module)
       (if company-ghc-component-prefix-match
           (all-completions
            "" (company-ghc--source-modules)
            (lambda (mod)
              (company-ghc--component-prefix-match-p
               (split-string prefix "\\.") mod)))
         (all-completions prefix (company-ghc--source-modules))))
      (`(qualified . ,alias)
       (let ((mods (company-ghc--list-modules-by-alias alias)))
         (company-ghc--gather-candidates prefix mods)))
      (_ (company-ghc--gather-candidates
          prefix
          (mapcar 'car company-ghc--imported-modules))))))

(defun company-ghc-meta (candidate)
  "Show type info for the given CANDIDATE. Use cached info if any."
  (or (company-ghc--pget candidate :type)
      (when company-ghc-show-info
        (let ((typ (company-ghc--source-info candidate)))
          (when typ
            (company-ghc--pset candidate :type typ))
          typ))))

(defun company-ghc-location (candidate)
  "Return cons of file path and line number of CANDIDATE."
  (company-ghc--pget candidate :location))

(defun company-ghc-doc-buffer (candidate)
  "Display documentation in the docbuffer for the given CANDIDATE."
  (with-temp-buffer
    (let ((mod (company-ghc--pget candidate :module)))
      (call-process company-ghc-hoogle-command nil t nil "search" "--info"
                    (if mod (concat candidate " +" mod) candidate)))
    (company-doc-buffer
     (buffer-substring-no-properties (point-min) (point-max)))))

(defun company-ghc-annotation (candidate)
  "Show module name as annotation where the given CANDIDATE is defined."
  (when company-ghc-show-module
    (concat " " (company-ghc--pget candidate :module))))

(defun company-ghc--gather-candidates (prefix mods)
  "Gather candidates for PREFIX from keywords in MODS and return them sorted."
  (when mods
    (sort (cl-mapcan
           (lambda (mod)
             (all-completions
              prefix (company-ghc--source-keywords mod)))
           mods)
          'string<)))

;;
;; import module parsing
;;
(defun company-ghc-scan-modules ()
  "Scan imported modules in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let (mod (mod-alist '(("Prelude"))))
      (while (setq mod (company-ghc--scan-impdecl))
        (when (consp mod)
          (setq mod-alist
                (cons
                 mod
                 (if (and (assoc-string (car mod) mod-alist) (cdr mod))
                     (delete (assoc-string (car mod) mod-alist) mod-alist)
                   mod-alist)))))
      (setq company-ghc--imported-modules mod-alist))))

(defun company-ghc-turn-on-autoscan ()
  "Turn on automatic scan module after save in the current buffer."
  (interactive)
  (add-hook 'after-save-hook #'company-ghc-scan-modules nil t)
  (message "company-ghc autoscan is enabled"))

(defun company-ghc-turn-off-autoscan ()
  "Turn off automatic scan module after save in the current buffer."
  (interactive)
  (remove-hook 'after-save-hook #'company-ghc-scan-modules t)
  (message "company-ghc autoscan is disabled"))

(defun company-ghc--scan-impdecl ()
  "Scan one import spec and return module alias cons.
If proper import spec is not found, return boolean value whether import spec
continues or not."
  (let* ((beg (company-ghc--search-import-start))
         (end (and beg (company-ghc--search-import-end (cdr beg)))))
    (when end
      (save-restriction
        (narrow-to-region (car beg) (car end))
        (goto-char (point-min))
        (let (chunk prev-chunk attrs mod)
          (while (setq chunk (company-ghc--next-import-chunk))
            (cond
             ((string= chunk "qualified") (push 'qualified attrs))
             ((string= chunk "safe") (push 'safe attrs))
             ((let ((case-fold-search nil))
                (string-match-p "^[[:upper:]]" chunk))
              (cond
               ((not mod) (setq mod (if (memq 'qualified attrs)
                                        (cons chunk chunk)
                                      (cons chunk nil))))
               ((string= prev-chunk "as") (setcdr mod chunk)))))
            (setq prev-chunk chunk))
          (or mod
              (string= (cdr end) "import")))))))

(defun company-ghc--search-import-start ()
  "Search start of import decl and return the point after import and offset."
  (catch 'result
    (while (re-search-forward "^\\([[:space:]]*\\)import\\>" nil t)
      (unless (save-match-data (company-ghc--in-comment-p))
        (throw 'result
               (cons (match-end 0)
                     (string-width (match-string-no-properties 1))))))))

(defun company-ghc--search-import-end (offset)
  "Search end of import decl and return the end point and next token.
If the line is less offset than OFFSET, it finishes the search."
  (forward-line)
  (catch 'result
    (let ((p (point)))
      (while (not (eobp))
        (cond
         ((company-ghc--in-comment-p) nil)
         ((looking-at "^[[:space:]]*$") nil)
         ((looking-at "^#") nil)
         ((looking-at "^\\([[:space:]]*\\)\\([^[:space:]\n]*\\)")
          (unless (< offset (string-width (match-string-no-properties 1)))
            (throw 'result (cons p (match-string-no-properties 2))))))
        (forward-line)
        (setq p (point)))
      (throw 'result (cons p nil)))))

(defun company-ghc--next-import-chunk ()
  "Return next chunk in the current import spec."
  (catch 'result
    (while (and (skip-chars-forward " \t\n") (not (eobp)))
      (cond
       ((or (looking-at-p "{-") (looking-at-p "--"))
        (forward-comment 1))
       ((looking-at-p "(")
        (throw 'result (ignore-errors
                         (buffer-substring-no-properties
                          (point) (progn (forward-sexp) (point))))))
       ((looking-at-p "\"")
        (re-search-forward "\"\\([^\"]\\|\\\\\"\\)*\"")
        (throw 'result (match-string-no-properties 0)))
       ((re-search-forward "\\=.[[:alnum:].]*" nil t)
        (throw 'result (match-string-no-properties 0)))
       (t (throw 'result nil))))))

;;
;; Utilities
;;
(defun company-ghc--pget (s prop)
  "Get property value of PROP from the keyword S."
  (get-text-property 0 prop s))

(defun company-ghc--pset (s prop val)
  "Set property PROP of the keywork S to VAL."
  (put-text-property 0 1 prop val s)
  s)

(defun company-ghc--in-comment-p ()
  "Return whether the point is in comment or not."
  (let ((ppss (syntax-ppss))) (nth 4 ppss)))

(defun company-ghc--list-modules-by-alias (alias)
  "Return list of imported modules that have ALIAS."
  (let (mods)
    (cl-dolist (pair company-ghc--imported-modules)
      (when (string= (cdr pair) alias)
        (setq mods (cons (car pair) mods))))
    mods))

(defun company-ghc--propertize-candidate (candidate &rest props)
  "Propertize CANDIDATE according to its string format and given PROPS."
  (let ((len (length candidate))
        (case-fold-search nil)
        knd)
    (cond
     ((and (> len 2) (eq (string-to-char candidate) ?\())
      (setq candidate (substring candidate 1 (- len 1)))
      (setq knd 'operator))
     ((string-match-p "^[[:upper:]]" candidate)
      (setq knd 'constructor))
     (t
      (setq knd 'identifier)))
    (apply #'add-text-properties
           0 1 (append (list :kind knd) props) (list candidate))
    candidate))

(defun company-ghc--grab-name ()
  "Grap identifier or operator name backward from the current point."
  (save-excursion
   (buffer-substring-no-properties
    (point)
    (progn
      (let* ((c (char-before))
             (syn (and c (char-syntax c))))
        (when (member syn '(?w ?.))
          (skip-syntax-backward (string syn)))
        (point))))))

(defun company-ghc--grab-qualified ()
  "Grab cons of qualified specifier and keyword backward from the current point.
Return nil if none found."
  (save-excursion
    (let ((prefix
           (buffer-substring-no-properties
            (point)
            (progn
              (skip-chars-backward "[:word:]")
              (point))))
          (case-fold-search nil)
          end)
      (setq end (- (point) 1))
      (when (and (equal (char-before) ?.)
                 (< (skip-chars-backward ".[:word:]") 0)
                 (looking-at-p "[[:upper:]]"))
        (cons
         (buffer-substring-no-properties (point) end)
         prefix)))))

(defun company-ghc--component-prefix-match-p (pcomps module)
  "Return non-nil if each component of PCOMPS is prefix of each component of
MODULE split by '.'."
  (let ((mcomps (split-string module "\\.")))
    (catch 'result
      (dolist (p pcomps)
        (when (or (null mcomps)
                  (not (string-prefix-p p (car mcomps))))
          (throw 'result nil))
        (setq mcomps (cdr mcomps)))
      (throw 'result t))))

;;
;; ghc-mod completion sources
;;
(defvar company-ghc--source-modules nil)
(defun company-ghc--source-modules ()
  "Return sorted module names, cached one if any."
  (or company-ghc--source-modules
      (setq company-ghc--source-modules
            (sort (cl-copy-list ghc-module-names) 'string<))))

(defvar company-ghc--source-pragmas nil)
(defun company-ghc--source-pragmas ()
  "Return sorted pragma names, cached one if any."
  (or company-ghc--source-pragmas
      (setq company-ghc--source-pragmas
            (sort (cl-copy-list ghc-pragma-names) 'string<))))

(defvar company-ghc--source-languages nil)
(defun company-ghc--source-languages ()
  "Return sorted language extensions, cached one if any."
  (or company-ghc--source-languages
      (setq company-ghc--source-languages
            (sort (cl-copy-list ghc-language-extensions) 'string<))))

(defvar company-ghc--source-options nil)
(defun company-ghc--source-options ()
  "Return sorted option flags, cached one if any."
  (or company-ghc--source-options
      (setq company-ghc--source-options
            (sort (cl-copy-list ghc-option-flags) 'string<))))

(defun company-ghc--source-keywords (mod)
  "Get defined keywords in the specified module MOD."
  (let ((sym (ghc-module-symbol mod))
        result)
    (when (and (not (boundp sym))
               (listp (setq result (ghc-load-module mod))))
      (set sym result))
    (when (boundp sym)
      (if (member mod company-ghc--propertized-modules)
          (ghc-module-keyword mod)
        (push mod company-ghc--propertized-modules)
        (mapcar (lambda (k) (company-ghc--pset k :module mod))
                (ghc-module-keyword mod))))))

(defun company-ghc--source-info (candidate)
  "Show type info for the given CANDIDATE by `ghc-show-info'."
  (let* ((mod (company-ghc--pget candidate :module))
         (pair (and mod (assoc-string mod company-ghc--imported-modules)))
         (qualifier (or (cdr pair) mod)))
    (when qualifier
      (let ((info (ghc-get-info (concat qualifier "." candidate))))
        (when (stringp info)
          (when (string-match
                 "-- Defined at \\(.*\\):\\([[:digit:]]+\\):[[:digit:]]+$"
                 info)
            (company-ghc--pset candidate :location
                               (cons (match-string-no-properties 1 info)
                                     (string-to-number
                                      (match-string-no-properties 2 info)))))
          (pcase company-ghc-show-info
            (`t info)
            (`oneline (replace-regexp-in-string "\n" "" info))
            (`nomodule
             (when (string-match "\\(?:[^[:space:]]+\\.\\)?\\([^\t]+\\)\t" info)
               (replace-regexp-in-string
                "\n" "" (match-string-no-properties 1 info))))))))))

;;;###autoload
(defun company-ghc (command &optional arg &rest ignored)
  "`company-mode' completion back-end for `haskell-mode' via ghc-mod.
Provide completion info according to COMMAND and ARG.  IGNORED, not used."
  (interactive (list 'interactive))
  (cl-case command
    (init (when (and (derived-mode-p 'haskell-mode) company-ghc-autoscan)
            (company-ghc-scan-modules)
            (add-hook 'after-save-hook #'company-ghc-scan-modules nil t)))
    (interactive (company-begin-backend 'company-ghc))
    (prefix (and (derived-mode-p 'haskell-mode)
                 (company-ghc-prefix)))
    (candidates (company-ghc-candidates arg))
    (meta (company-ghc-meta arg))
    (doc-buffer (company-ghc-doc-buffer arg))
    (location (company-ghc-location arg))
    (annotation (company-ghc-annotation arg))
    (sorted t)))

;;;###autoload
(defun company-ghc-diagnose ()
  "Show diagnostic info of the current buffer in other buffer."
  (interactive)
  (if (not (derived-mode-p 'haskell-mode))
      (message "Not in haskell-mode")
    (let ((be (catch 'result
                (cl-dolist (x company-backends)
                  (cond
                   ((and (listp x) (memq 'company-ghc x)) (throw 'result x))
                   ((eq x 'company-ghc) (throw 'result x))))))
          (autoscan (memq 'company-ghc-scan-modules after-save-hook))
          (mods company-ghc--imported-modules))
      (switch-to-buffer-other-window "**company-ghc diagnostic info**")
      (erase-buffer)
      (if be
          (insert (format "* company-ghc backend found: %s\n" be))
        (insert "* company-ghc backend not found\n"))
      (if autoscan
          (insert "* automatic scan module is enabled\n")
        (insert "* automatic scan module is disabled.\n"
                "  You need to run either 'M-x company-ghc-turn-on-autoscan' once\n"
                "  or 'M-x company-ghc-scan-modules' when it is necessary.\n"))
      (if (null ghc-module-names)
          (insert "* ghc-boot process has not been done\n")
        (if (null ghc-language-extensions)
            (insert "* ghc-boot process seems to have failed\n")
          (insert "* ghc-boot process has been done\n")))
      (insert "\n")
      (insert "Module")
      (move-to-column 40 t)
      (insert "Alias")
      (move-to-column 60 t)
      (insert "Candidates\n")
      (insert-char ?- 79)
      (insert "\n")
      (cl-dolist (pair mods)
        (let* ((mod (car pair))
               (alias (cdr pair))
               (sym (ghc-module-symbol mod))
               (len (or (and (boundp sym)
                             (length (ghc-module-keyword mod)))
                        nil)))
          (insert mod)
          (move-to-column 40 t)
          (delete-region (point) (line-end-position))
          (insert (or alias "-"))
          (move-to-column 65 t)
          (delete-region (point) (line-end-position))
          (insert (format "%s\n" len))))
      (help-mode-setup)
      (goto-char (point-min)))))

;;
;; in-module completion
;;
(defun company-ghc-complete-in-module (mod)
  "Complete keywords defined in the specified MOD.
When called interactively, MOD is specified in minibuffer."
  (interactive (list (completing-read
                      "Module: " company-ghc--imported-modules nil nil)))
  (company-begin-backend
   (lambda (command &optional arg &rest ignored)
     (cl-case command
       (prefix (company-ghc--grab-name))
       (candidates (company-ghc--gather-candidates arg (list mod)))
       (meta (company-ghc-meta arg))
       (doc-buffer (company-ghc-doc-buffer arg))
       (annotation (company-ghc-annotation arg))
       (sorted t)))))

;;
;; hoogle search completion
;;
(defun company-ghc-complete-by-hoogle (query)
  "Complete keywords searched by hoogle with the specified QUERY.
When called interactively, QUERY is specified in minibuffer."
  (interactive "sHoogle: ")
  (company-begin-backend
   (lambda (command &optional arg &rest ignored)
     (cl-case command
       (prefix (company-ghc--grab-name))
       (candidates (company-ghc--hoogle-candidates query))
       (meta (company-ghc--pget arg :type))
       (doc-buffer (company-ghc-doc-buffer arg))
       (annotation (company-ghc-annotation arg))
       (sorted t)))))

(defun company-ghc--hoogle-candidates (query)
  "Provide hoogle search results for QUERY."
  (with-temp-buffer
    (call-process company-ghc-hoogle-command nil t nil
                  "search"
                  "-n" (number-to-string company-ghc-hoogle-search-limit)
                  query)
    (company-ghc--hoogle-parse-results)))

(defun company-ghc--hoogle-parse-results ()
  "Parse hoogle search results in the current buffer."
  (let (result)
    (goto-char (point-min))
    (unless (or (looking-at-p "^No results found$")
                (looking-at-p "^Could not find some databases:"))
      (while (re-search-forward
              "^\\([^[:space:]]+\\) \\([^[:space:]\n]+\\)\\(.*\\)$" nil t)
        (let* ((mod (match-string 1))
               (fun (match-string 2))
               (typ (concat fun (match-string 3))))
          (unless
              (or (member mod '("package" "keyword"))
                  (member fun '("class" "data" "module" "newtype" "type")))
            (push (company-ghc--propertize-candidate
                   fun :module mod :type typ) result))))
      (nreverse result))))

(provide 'company-ghc)
;;; company-ghc.el ends here
