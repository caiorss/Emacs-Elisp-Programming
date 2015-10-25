;;
;; Autor: Caio Rodrigues
;; URL:   http://tinyurl.com/emacsinabox
;;
;;
;; Emacs Toolbox
;;
;; Useful Macros and Functions for Emacs - Elisp
;;
;;
;;
;;--------------------------------------------------------


(defun unique (list)
  (let (tmp-list head)
    (while list
      (setq head (pop list))
      (unless (equal head (car list))
        (push head tmp-list)))
    (reverse tmp-list)))

(defun string/starts-with (s begins)
  "Return non-nil if string S starts with BEGINS."
  (cond ((>= (length s) (length begins))
         (string-equal (substring s 0 (length begins)) begins))
        (t nil)))

(defun string/starts-with (string prefix)
  "Return t if STRING starts with prefix."
  (and (string-match (rx-to-string `(: bos ,prefix) t)
                     string)
       t))

(defun string/join (glue-char  string-list)
  "
  Example:

     ELISP> (string/join \",\" '(\"10.23\" \"Emacs\" \"Lisp\" \"Rocks\"))
     \"10.23,Emacs,Lisp,Rocks\"
     ELISP>

  "
  (mapconcat 'identity string-list glue-char))


(defun string/split (glue-char str)
  (split-string str glue-char)
  )

(defun string/quote (str)
  (format "\"%s\"" str)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Plist to Alist
(defun plist->alist (plist)
  (if (null plist)
      '()
      (cons
       (list (car plist) (cadr plist))
       (plist->alist (cddr plist)))))

;;; Convert association list to plist
(defun alist->plist (assocl)
  (if (null assocl)
      '()
    (let
    ((hd (car assocl))
     (tl (cdr assocl)))
      (cons (car hd)
        (cons (cadr hd)
          (alist->plist tl))))))


;; Separates a property list into two lists of keys and values.
;;
(defun plist->kv (plist)
  (let ((alist (plist->alist plist)))
    (cons
     (mapcar #'car alist)
     (mapcar #'cdr alist))))


(defun map (fun xs)
  (if (null xs)
      '()
    (cons (funcall fun (car xs))
          (map fun (cdr xs)))))

;;
;; Scheme for-each function.
;;
(defun for-each (fun xs)
  (dolist (x xs) (funcall fun x)))


(defun for-each-appply (fun xss)
  (for-each (lambda (xs) (apply fun xs))  xss))



(defun filter (fun xs)
  (let ((acc  nil))
    (dolist (x xs)
      (if (funcall fun x)
	  (setq acc (cons x acc))
	))
      (reverse acc)
    ))


(defun reject (fun xs)
  (let ((acc  nil))
    (dolist (x xs)
      (if (not (funcall fun x))
	  (setq acc (cons x acc))
	))
      (reverse acc)
    ))



(defun take (n xs)
  (if (or (null xs) (zerop n))
      '()
    (cons (car xs)
      (take (- n 1) (cdr xs)))))


(defun drop (n xs)
  (if (or (null xs) (zerop n))
      xs
    (drop (- n 1)  (cdr xs))))



(defun map-apply (fun xss)
  (mapcar (lambda (xs) (apply fun xs)) xss))

(defun zip (&rest xss)
    (if (null (car xss))
    '()
      (cons
       (mapcar #'car xss)
       (apply #'zip (mapcar #'cdr xss)))))

(defun zipwith (f &rest xss)
  (map-apply f (apply #'zip xss)))


(defun foldr (f acc xss)
  (if (null xss)
       ;; foldr f z []     = z
      acc
       ;; foldr f z (x:xs) = f x (foldr f z xs)
    (funcall f (car xss)
             (foldr f acc (cdr xss)))))

;;; f :: acc -> x -> acc
(defun foldl (f acc xss)
  (if (null xss)
      acc
    (foldl f (funcall f acc (car xss)) (cdr xss))))

;;
;; Function Composition Macro
;;
(defmacro fcomp (&rest funlist)
  "
  Forward function composition:

  Example:

   ELISP> (fcomp 1+ number-to-string print)
  (lambda
    (__x__)
    (print
      (number-to-string
       (1+ __x__))))


   ELISP> (funcall (fcomp 1+ number-to-string print) 10)

   \"11\"

  "
  `(lambda (__x__)
     ,(foldl
       (lambda (a b) `(,b ,a)) '__x__ funlist)))

(defun map-pair (func xs)
  "
 Example:

   ELISP> (map-pair #'log10 '(1 10 100 1000 10000))
    ((1 . 0.0)
    (10 . 1.0)
    (100 . 2.0)
   (1000 . 3.0)
   (10000 . 4.0))
  "
  (mapcar (lambda (x) (cons x (funcall func x))) xs))


(defun map-xypair (func-x func-y xs)
  "
  Example:

   map-xypair fx fy [x0, x1, x2, ... xn]

   Returns:
           [(fx x0, fy x0), (fx x1, fy x1), (fx x2, fy x2) ...]

   ELISP> (map-xypair #'buffer-name #'buffer-mode (buffer-list))
   ((\"*ielm*\" . inferior-emacs-lisp-mode)
    (\"*scratch*\" . lisp-interaction-mode)
    (\"*Backtrace*\" . debugger-mode)
    ...
  "
  (mapcar
   (lambda (x)
     (cons (funcall func-x x) (funcall func-y x)))
   xs))

(defmacro juxt (&rest xs_f)
  "
  ELISP> (funcall (juxt #'buffer-file-name  #'buffer-name #'buffer-mode) (current-buffer))
  (nil \"*ielm*\" inferior-emacs-lisp-mode)
  "
  `(lambda (x)
     (list ,@(mapcar (lambda (f) `(funcall ,f x)) xs_f))))


(defun remove-from-list (reject-list xlist)
  "Remove all elements from xlist that are in the reject-list

   Example:

   ELISP> (remove-from-list '(z y x) '(a b c x y z a x))
   (a b c a)

  "
  (filter (lambda (q)
              ($n member q reject-list))
          xlist))


(defun replace_sym (targ subst lst)
  (if (null lst)
      '()
    (let ((hd (car lst))
      (tl (cdr lst)))
      (if (equal targ hd)
      (cons subst (replace_sym targ subst tl))
    (cons (if (listp hd) (replace_sym targ subst hd) hd)
          (replace_sym targ subst tl))))))




;;;;; Clojure Macros ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defmacro fn (args body)
 `(lambda ,args ,body))

(defmacro $f  ( &rest body)
  `(lambda (%) ,body))

;;; Delay expression
(defmacro $fd (func &rest params)
  (lambda () (,func ,@params)))

(defmacro def (name value)
   `(setq ,name ,value))

(defmacro defn (name args body)
  `(defun ,name ,args ,body))

(defmacro if-not (condition false-clause true-clause)
  `(if (not ,condition)
       ,false-clause
     ,true-clause))

;; Invert S-expression logical value
;;
(defmacro $n (pred &rest args)
  `(not (,pred ,@args)))

(defmacro letc (bindings &rest body)
  "
   Example:

   (letc
         (
          a 10
          b (+ a 3)
          c (+ a b)
          )
         (list a b c))

    Result: (10 13 23)
  "
  `(let*
       ,(plist->alist bindings)
     ,@body))

(defun pass-result (x sexp)
  (if (listp sexp)
      `(,(car sexp) ,x ,@(cdr sexp))
      `(,sexp ,x)))

(defun pass-result-last (x sexp)
  (if (listp sexp)
    `(,(car sexp) ,@(cdr sexp) ,x)
    `(,sexp ,x)))

(defun pass-result-subst (x sexp)
  (if (listp sexp)
     (replace_sym '$ x sexp)
    `(,sexp ,x)))

(defmacro -> (x &rest exprs)
  "
   ;; The expression below is expanded to:
  ;;
   (->
    5
    exp
    (/ 20)
    (+ 10 20)
    (- 3)
    log10)

  (log10  (-  (+   (/  (exp 5)   20) 10 20)  3))
  "
  (foldl #'pass-result x exprs))


(defmacro --> (x &rest exprs)
  "
   (-->
      5
      (/ 20)
      (+ 10 20)
      (- 16))

   ;; Macro expansion

   (- 16 (+ 10 20 (/ 20 5))) = -18
  "
  (foldl #'pass-result-last x exprs))



(defmacro $-> (x &rest exprs)
  "
  ($->
    500
    (/ $ 20 )
    (- 40 $)
    sqrt)

   Expansion: (sqrt  (- 40 (/ 500 20)))
   Exprected result: 3.872983346207417
  "
  (foldl #'pass-result-subst x exprs))



(defmacro $dbg (func &rest params)
   "
   Usage:

    ELISP> ($dbg + 10 ($dbg * 10 30))
    \"(* 10 30) = 300\"
    \"(+ 10 ($debug * 10 30)) = 310\"

    Output: 310

   "
  `(let
      ((__r (,func ,@params)))
       (progn
     (print (format "%s = %s"
      (quote (,func ,@params))
       __r))
     __r)))



;;;;;;;;;;;;;;;; Emacs API Wrappers ;;;;;;;;;;;;;;;;;;;;

;;
;; Symbol Generator
;;
(defun make-sym-suffix (sym suffix)
  (-> sym
      symbol-name
      (concat suffix)
      intern))


(defun buffer-content (name)
    (with-current-buffer name
      (buffer-substring-no-properties (point-min) (point-max)  )))

(defun get-selection ()
  "Get the text selected in current buffer as string"
  (interactive)
  (buffer-substring-no-properties (region-beginning) (region-end)))



(defun replace-regexp-entire-buffer (pattern replacement)
  "Perform regular-expression replacement throughout buffer."
  (interactive
   (let ((args (query-replace-read-args "Replace" t)))
     (setcdr (cdr args) nil)    ; remove third value returned from query---args
     args))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward pattern nil t)
      (replace-match replacement))))

(defun show-doc (function)
  (princ (documentation function)))

(defun mode/show ()
  "  Returns all modes associated with files

     To query the file extesions associated with a mode
     use:
         > (mode/ftypes 'markdown-mode)

     for example.
  "
  (for-each #'print
	    (unique (reject #'listp (map #'cdr auto-mode-alist)))))


(defun mode/ftypes (mode)
  "
  Get all file extension associated with a mode.

  Usage:

  ELISP> (get-mode-ftypes 'markdown-mode)
  ((\"\\.md\\'\" . markdown-mode)
  (\"\\.text\\'\" . markdown-mode)
  (\"\\.markdown\\'\" . markdown-mode)

  "
  (remove-if-not
   (lambda (al)
     (equal (cdr al) mode))
   auto-mode-alist))


;; Set may keys at same time. A macro in Clojure-style
;; with minimum amount of parenthesis as possible.
;;
(defmacro define-global-keys (&rest keylist)
  `(progn
     ,@(map-apply (lambda (key fun)
            `(global-set-key (kbd ,key) ,fun))
                  (plist->alist keylist))))

;; (add-hook 'emacs-lisp-mode-hook
;;           (lambda ()
;;             (progn

(defmacro add-hooks (hookname &rest functions)
  `(progn
    ,@(map (lambda (f) `(add-hook ,hookname ,f))
           functions)))


(defmacro define-mode-keys (modename &rest key-functions-pairs)
  "
  Define Key binding for a lisp mode

  Example:

    (define-mode-keys emacs-lisp-mode-map
     \"C-c C-c\" #'eval-defun
     \"C-c C-b\" #'eval-buffer
    )
  "
  (let
      ;;((symname  (make-sym-suffix modename "-map")))
      ((symname modename))
   `(progn
      ,@(map-apply (lambda (key func)
                     `(define-key ,symname (kbd ,key) ,func))
                   (plist->alist key-functions-pairs)))))


(defmacro define-global-menu (menu-name &rest label-actions-plist)
  "
  Example:

  (define-global-menu  \"Color Themes\"
    \"Adwaita\"       (load-theme 'adwaita)
    \"Deeper-blue\"   (load-theme 'deeper-blue)
    \"Dichromacy\"    (load-theme 'dichromacy)
    \"Leuven\"        (load-theme 'leuven)
    \"light-blue\"    (load-theme 'light-blue)
    )

  "
  `(easy-menu-define djcb-menu global-map ,menu-name
     (quote (,menu-name
      ,@(map-apply #'vector
                   (plist->alist label-actions-plist))))))


(defun add-repository (path)
  "Add repository path and its sub directories and
   files to the load-path variable."
  (setq load-path
        (append
         load-path
         (plist-get (dir-list/abs-tags path) :dirs)
         (list path))))

;;;;;;;;;;;;;; Http Request ;;;;;;;;;;;

(defun url-http-post (url args)
  "Send ARGS to URL as a POST request."
  (let (
        (response-string nil)
        (url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/x-www-form-urlencoded")))
        (url-request-data
         (mapconcat (lambda (arg)
                      (concat (url-hexify-string (car arg))
                              "="
                              (url-hexify-string (cdr arg))))
                    args
                    "&")))
    (switch-to-buffer
     (url-retrieve-synchronously url))
    (goto-char (point-min))
    (re-search-forward "\n\n")
    (setq response-string
          (buffer-substring-no-properties (point) (point-max)))
    (kill-buffer (current-buffer))
    response-string))

(defun url-http-get (url args)
  "Send ARGS to URL as a GET request."
  (let (
        (response-string nil)
        (url-request-method "GET")
        (url-request-data
         (mapconcat (lambda (arg)
                      (concat (url-hexify-string (car arg))
                              "="
                              (url-hexify-string (cdr arg))))
                    args
                    "&")))
    (switch-to-buffer
     (url-retrieve-synchronously
      (concat url "?" url-request-data)))
    (goto-char (point-min))
    (re-search-forward "\n\n")
    (setq response-string
          (buffer-substring-no-properties
           (point) (point-max)))
    (kill-buffer (current-buffer))
    response-string))

;;;;;;;;;;;;;; Files and Directories  ;;;;;;;;;;;;;;;;;


(defun concat-path (base relpath)
  (concat (file-name-as-directory base) relpath))

;; Usage: M-x reload-init-file
;;
(defun reload-init-file ()
  "Reload init.el file"
  (interactive)
  (load user-init-file)
  (message "Reloaded init.el OK."))

;; Usage: M-x open-init-file
;;
(defun open-init-file ()
    (interactive)
    (find-file user-init-file)
)

(defun open-file-manager ()
  "Open buffer directory in file manager (Linux Only)"
  (interactive)
  (call-process "pcmanfm"))




(defun read-file (filename)
  (interactive "fFind file: ")
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun write-file (filename text)
  (append-to-file text nil filename))

(defun current-dir ()
  "Show current directory"
  (interactive)
  (nth 1 (split-string (pwd))))

(defun current-file ()
  "Show Current File"
  (interactive)
  (buffer-file-name (current-buffer)))



(defun dir/list-abs (dirpath)
  " List directory with absolute path"
  (letc
    (abs-dirpath  (expand-file-name dirpath))
    (-->
     (directory-files abs-dirpath)
     (remove-from-list '("." ".."))
     (map ($f concat-path abs-dirpath %)))))


(defun dir/list-abs-tags (dirpath)
  "Returns a plist (:files filelist dirs: dirlist)"
  (letc
   (content  (dir-list/abs dirpath))
   (list
    :files (reject #'file-directory-p content)
    :dirs  (filter #'file-directory-p content))))





(defun files-in-below-directory (directory)
  "List the .el files in DIRECTORY and in its sub-directories."
  ;; Although the function will be used non-interactively,
  ;; it will be easier to test if we make it interactive.
  ;; The directory will have a name such as
  ;;  "/usr/local/share/emacs/22.1.1/lisp/"
  (interactive "DDirectory name: ")

  (let (el-files-list
        (current-directory-list
         (directory-files-and-attributes directory t)))
    ;; while we are in the current directory
    (while current-directory-list

      (cond
       ;; check to see whether filename ends in `.el'
       ;; and if so, append its name to a list.
       ((equal ".el" (substring (car (car current-directory-list)) -3))
        (setq el-files-list
              (cons (car (car current-directory-list)) el-files-list)))

       ;; check whether filename is that of a directory
       ((eq t (car (cdr (car current-directory-list))))
        ;; decide whether to skip or recurse
        (if
            (equal "."
                   (substring (car (car current-directory-list)) -1))
            ;; then do nothing since filename is that of
            ;;   current directory or parent, "." or ".."
            ()

          ;; else descend into the directory and repeat the process
          (setq el-files-list
                (append
                 (files-in-below-directory
                  (car (car current-directory-list)))
                 el-files-list)))))
      ;; move to the next filename in the list; this also
      ;; shortens the list so the while loop eventually comes to an end
      (setq current-directory-list (cdr current-directory-list)))
    ;; return the filenames
    el-files-list))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;      S expressions                        ;;
;;-------------------------------------------;;

(defun file->sexp (filename)
  (read (read-file filename)))

(defun str->sexp (str)
  (read str))

(defun sexp->str (sexp)
  (prin1-to-string sexp))

(defun sexp->file (sexp filename)
  (--> sexp
       sexp->str
       (write-file filename)))

(defun url->sexp (url)
  "Read multiple S-expression from a URL path"
  ($-> url
       (url-http-get $ nil)
       (read $)))

(defun url->sexps (url)
  "Read multiple S-expression from a URL path"
  ($-> url
       (url-http-get $ nil)
       (concat "( " $ " ) ") ;; Wrap parenthesis
       (read $)))

(defun load-url (url)
  "Load an emacs source code from a URL"
  (eval (cons 'progn (url->sexps url))))


(defun eval-string (str)
  (eval (read str)))

(defun url->filename (url)
  (-> url
      (split-string "/")
      last
      car))

(defun url-download (url dirpath filename)
  "
   Download a file:

  Usage:

  To download a file from a url to a given directory
  with a given filename.

  > (url-download url dirpath filename)

  or

  To download to the current directory with a
  defined filename.

  > (url-download url nil filename)

  or

  To download a file to current directory, the file name
  will be extracted from the url.

  > (url-download url nil nil)

  "
  (letc
   (dir    (if (null dirpath)
               (current-dir)
                dirpath)

   fname   (if (null filename)
                (url->filename url)
             filename)

   fpath    (concat-path dir fname))

  (princ (list dir fname fpath))
  ($-> url
      (url-http-get $ nil)
      (write-file fpath $))))


;;
;; Src: http://web.ics.purdue.edu/~dogbe/static/emacs_config_file.html
;;
(defun google ()
  "Googles a query or region if any"
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (if mark-active
        (buffer-substring (region-beginning) (region-end))
      (read-string "Google: ")))))

(defun decode-url ()
  "
   Usage: Select the url and type: A-x decode-url
   It will print the URL parameters in the buffer,
   example:

    When this Url is selected and the user type: A-x decode-url

    http://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=1&cad=rja&ved=0CCwQFjAA&url=http%3A%2F%2Fen.wikipedia.org%2Fwiki%2FEditor_war&ei=k9ktUuz2Kvaj4APCmoGIBw&usg=AFQjCNGrgP7q1TYCPmuQnkgcfMIJKpFHOg&sig2=a6GUXFARKXj6r1JyiNkQ8w&bvm=bv.51773540,d.dmg

  It will be printed in the current buffer:

  sa = t
  rct = j
  q =
  esrc = s
  source = web
  cd = 1
  cad = rja
  ved = 0CCwQFjAA
  url = http%3A%2F%2Fen.wikipedia.org%2Fwiki%2FEditor_war
  ei = k9ktUuz2Kvaj4APCmoGIBw
  usg = AFQjCNGrgP7q1TYCPmuQnkgcfMIJKpFHOg
  sig2 = a6GUXFARKXj6r1JyiNkQ8w
  bvm = bv.51773540,d.dmg


  "
  (interactive)
  (-> (get-selection)
      get-url-params
      insert
      ))

(defun get-url-params (url)
  "

  "
  (interactive)
  ($->  url
        (split-string (cadr (split-string $  "?" )) "&")
        (map ($f split-string % "=") $)
        (map ($f  string/join " = " %) $)
        (string/join "\n" $)
        (concat "\n\n" $)
        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;      Cursor Functions                     ;;
;;-------------------------------------------;;


;;; This function can be called by typing M-x launch-terminal in any buffer.
;;;
(defun launch-terminal ()
  (interactive)
  (shell-command "lxterminal"))

(defun url-at-point ()
  (interactive)
  (browse-url (thing-at-point 'url)))

(defun open-file-at-point ()
  (interactive)
  (find-file (thing-at-point 'filename)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Save / Restore Views             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(setq winconf nil)

(defun save-view ()
  "Save current window configuration"
  (interactive)
  (setq winconf  (current-window-configuration))
  (message "View Saved - Press to restore"))

(defun restore-view ()
  "Restore saved window configuration"
  (interactive)
  (set-window-configuration winconf)
  (message "View loaded"))

;;; Copy String to Clipboard

(defun copy-to-clipboard (astring)
 (with-temp-buffer
  (insert astring)
  (clipboard-kill-region (point-min) (point-max))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    KEY BINDINGS                     ;;
;;-------------------------------------;;

(defun delete-word ()
    "Delete word next at cursor (point) - can be associated
     with a key binb.
    "
    (interactive)
    (let ((beg (point)))
      (forward-word 1)
      (delete-region beg (point))))



;;
;; Set global keys for all modes
;;
(define-global-keys
  "C-c M-u"  #'url-at-point
  "C-c M-f"  #'open-file-at-point
  "C-c M-r"  #'launch-terminal
  "<f8>"     #'execute-extended-command  ;; Same as A-x
  "C-<f8>"   #'open-file-manager
  "C-<f9>"   #'save-view
  "M-<f9>"   #'restore-view
  "C-d"      #'delete-word

  ;; using the meta key to jump between windows
  "M-0"     #'delete-window
  "M-1"     #'delete-other-windows
  "M-2"     #'split-window-vertically
  "M-3"     #'split-window-horizontally
  "M-o"     #'other-window

  ;; Join Lines
  "M-j"      (lambda () (interactive) (joinr-line -1))
  )


;; Delete trailing whitespace before saving a file
;;
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Emacs Lisp Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun copy-sexp-at-point ()
    "Copy s-expression where is the cursor to clipboard"
    (interactive)
    (copy-to-clipboard (thing-at-point 'sexp)))

(defun macro-expand-at-point ()
    (interactive)
    (-> (thing-at-point 'sexp)
        str->sexp
        macroexpand
        sexp->str
        insert
        ))

(defun eval-selection ()
  (interactive)
  ($->
        (get-selection)
        (concat "(" $ ")")
        (progn (print $)  $)
        read
        (cons 'progn $)
        eval))

;; http://emacs.wordpress.com/2007/01/17/eval-and-replace-anywhere/
(defun eval-and-replace-last-sexp ()
  "Replace the preceding sexp with its value."
  (interactive "*")
  (save-excursion
    (with-syntax-table emacs-lisp-mode-syntax-table
      (backward-kill-sexp)))
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))


(define-mode-keys emacs-lisp-mode-map
    "C-c C-c" #'eval-defun
    "C-c C-b" #'eval-buffer
    "C-c C-r" #'eval-and-replace-last-sexp
    "C-j"     #'eval-print-last-sexp
    "M-m"     #'macro-expand-at-point
    "C-M-j"   #'eval-selection
    "C-c C-y" #'copy-sexp-at-point
    )



(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq show-paren-style 'expression)
            (setq show-paren-delay 0)
             (electric-pair-mode 1)
             (show-paren-mode 1)
             (turn-on-auto-fill)
             (turn-on-eldoc-mode)
             ))



;; Disable E-shell banner message
(setq eshell-banner-message "")

(add-hooks 'eshell-mode-hook
           (lambda ()
             (add-to-list 'eshell-visual-commands "ssh")
             (add-to-list 'eshell-visual-commands "htop")
             (add-to-list 'eshell-visual-commands "ncmpcpp")
             (add-to-list 'eshell-visual-commands "tail")
           ))

(cua-mode)


;; (progn
;;  (load-theme 'wombat)
;;  (set-face-attribute 'default nil :height 94)
;;  (set-face-attribute 'fringe nil :background "#2d2d2d")
;;  (set-face-attribute 'default nil :family "Source Code Pro")
;;  (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
;;  (set-face-attribute 'font-lock-comment-face nil :weight 'semibold)
;;  (set-fontset-font "fontset-default" 'unicode "DejaVu Sans Mono for Powerline")
;;  )                                      ;
