;;
;; Autor: Caio Rodrigues
;; URL:   http://tinyurl.com/emacsinabox
;;
;;
;; Emacs Toolbox
;;
;; This toolbox provides many reusable functions without global state
;; to manipulate Emacs objects like buffers, windows, frames ... 
;;
;; It also provides many reusable macros to make Emacs programming easier.
;;
;;
;;-----------------------------------------------------------------

;; Load Common Lisp Emulation Library 
(require 'cl)

(defun repeat (n obj)
  "Repeate any object n times, example:
 
  > (repeat 3 'x)
  (x x x)
  " 
  (loop for i from 1 to n collecting obj))

(defmacro constantly (a)
  `(lambda (b) ,a))

;; identity - builtin function to Emacs


(defun unique (xs)
  "Remove repeated elements from list xs
 
  Example:
  
  > (unique '(x y a b 21 21 10 21 x y a ))
  (x y a b 21 10)
  "
  (let
    ((result nil))   

    (dolist (x xs)
      (if (not (member x result))
          (push x result)         
        ))
    (reverse result)
    ))


 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; String Functions          ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 

(defalias 'string/concat #'concat)
(defalias 'string/equal  #'string-equal)

(defun string/replace-regexp (pat sub str)
  (replace-regexp-in-string pat sub str t))


(defun string/replace-seq  (list-of-patterns replacement str)
  "
  Example:

   > (string/replace-seq 
           '(\"a\" \"b\" \"<html>\") \"-\" \"The a b c yy <html> \"  )

   Output:
		\"The - - c yy - \"		
  "
  (foldl
   (lambda (acc x)
     (string/replace-regexp x replacement acc)
     ) 
   str
   list-of-patterns
   ))


(defun string/replace-pairs  (patterns-pairs str)
  "
  Example:

	>	(string/replace-pairs		
		 '(		
		   (\"lang\"     .  \"Lisp\")		
		   (\"xxy\"      .  \"Hacker\")		
		   (\"name\"     . \"John\")		
		   )		
		
		 \"The lang xxy name\"		
		 )		
   
   Output:

	\"The Lisp Hacker John\"			
  "
  (foldl
   (lambda (acc x)
     (string/replace-regexp (car x) (cdr x) acc)
     ) 
   str
   patterns-pairs
   ))


(defun string/find-all (regexp str &optional start-pos)
  "Example:
 
  ELISP> (string-find-all \"{\\\\(.+?\\\\)}\" 
              \"The name of file is {NAME} and its location is {PATH}/{NAME}\")
   (\"NAME\" \"PATH\" \"NAME\")

  "
  (cl-loop for match-pos = (string-match regexp str start-pos)
           while match-pos
           collect (match-string 1 str)
           do (setf start-pos (1+ match-pos))))

(defun string/ends-with (string suffix)
  "Return t if STRING ends with SUFFIX."
  (and (string-match (rx-to-string `(: ,suffix eos) t)
                     string)
       t))

(defun string/starts-with (string prefix)
  "Return t if STRING starts with prefix."
  (and (string-match (rx-to-string `(: bos ,prefix) t)
                     string)
       t))

(defun string/reverse (str)
  "Reverse the str where str is a string"
  (apply #'string 
	     (reverse 
	      (string-to-list str))))


(defun string/join (glue-char  string-list)
  "
  Example:

     ELISP> (string/join \",\" '(\"10.23\" \"Emacs\" \"Lisp\" \"Rocks\"))
     \"10.23,Emacs,Lisp,Rocks\"
     ELISP>

  "
  (mapconcat 'identity string-list glue-char))


(defun string/split (sep str)
  "
  Split string with separtor
  
  Example:

  > (string/split \",\" \"232,1000,400,545\")

  "
  (split-string str sep)
  )

(defun string/chop-suffix (suffix s)
  "Remove a string suffix

  Example:
  > (string/chop-suffix \".html\" \"filename.html\")
  \"filename\"
   "
  (let ((pos (- (length suffix))))
    (if (and (>= (length s) (length suffix))
             (string= suffix (substring s pos)))
        (substring s 0 pos)
      s)))


(defun string/quote (str)
  (format "\"%s\"" str)
  )

(defun string/escape (str)
  (replace-regexp-in-string "\"" "\\\\\"" str))

(defun string/unescape (str)
   (replace-regexp-in-string  "\\\\\"" "\"" str))


(defun string/template (str &optional plist)
  "
  Evaluate string template

  Example:

  >> (string/template
   \"My name is {:NAME} {:SURNAME} 
   
   The sum of 1 2 3 4 5 is <% (+ 1 2 3 4 5 ) %> 
   \"
   '(:NAME \"Abraham\" :SURNAME \"Lincol\"))

 Output:

  \"My name is Abraham Lincol 
   
   The sum of 1 2 3 4 5 is 15 
   \"
  "
  (letc
   (
    keys    (map #'intern-soft
                 (unique (string/find-all  "{\\(.+?\\)}" str )))
            result  str              
    )

   (dolist (k keys)
     (setq result
           (replace-regexp-in-string
            (format "\{%s\}" (symbol-name k))
            (plist-get plist k) result t)))


   ;; Evaluates everything between <% %> tags and replace with the result of
   ;; evaluation
   ;;
   (foldl 
    (fn (s x) (string/replace-regexp
               (regexp-quote (car x))
               (string/replace-regexp "\"" ""
                   (sexp->str (cdr x)))    ;; Convert anything to string
               s))
    result     
    (map-pair
     (fcomp
      ($f string/replace-regexp "<%" "" %)  ;; Remove <%
      ($f string/replace-regexp "%>" "" %)  ;; Remove %>
      #'eval-string
      )
     (string/find-all "\\(<%.+?%>\\)" result)))

   ))

(defun string/trim-all (str)
  "
  Eliminates all white spaces (tabs and spaces)
  from the beggining, end and from the beggining of each 
  line from a string.

  "
    (string/replace-pairs
     '(
       ;; Eliminates all white spaces from the top
       ("\n[\t|\s]+" . "\n")
       
       ;; Eliminates all white spaces from the
       ;; beggining of each line
       ("^[\t|\s\n]+" . "")
       
       ;; Eliminates all white spaces from the bottom
       ("[\n\t\s]+$" . ""))    
     str))

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
  "
  Example:

		> (plist->kv 
           '(:x 10 :y 20 :desc-x \"Point x\" :desc-y \"Point y\"))		
    
       Output:
		((:x :y :desc-x :desc-y)		
		 (10 20 \"Point x\" \"Point y\"))			
  "
  (let ((alist (plist->alist plist)))
    (list
     (mapcar #'car alist)
     (mapcar #'cadr alist))))


(defalias 'map 'mapcar)
(defalias 'filter 'remove-if-not)
(defalias 'reject 'remove-if)

;;
;; Scheme for-each function.
;;
(defun for-each (fun xs)
  (dolist (x xs) (funcall fun x)))


(defun for-each-appply (fun xss)
  (for-each (lambda (xs) (apply fun xs))  xss))


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

;;; f :: x -> acc -> acc
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

(defun bind-nil (f x)
  (if x (funcall f x) nil))
  

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
  (let ((sym-var (gensym)))
  
    `(lambda (,sym-var)
       ,(foldl
         (lambda (a b) `(bind-nil ,b ,a))
         sym-var
         funlist))))
        


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
  ELISP> (funcall (juxt #'buffer-file-name
                        #'buffer-name
                        #'buffer-mode)
          (current-buffer))
  Output:

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
         (plist-get (dir/list-abs-tags path) :dirs)
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


;;;;;;;;;;;;;;;;; FILE API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defalias 'file/extension         'file-name-extension)
(defalias 'file/extension-sans    'file-name-sans-extension)
(defalias 'file/path-expand       'expand-file-name)
(defalias 'file/filename          'file-name-nondirectory)
(defalias 'file/path-relative     'file-relative-name)
(defalias 'file/rename            'rename-file)
(defalias 'file/delete            'delete-file)
(defalias 'file/copy              'copy-file)

(defun file/has-extension (filename ext)
  (string-equal ext (file/extension filename))
  )

;; 
;; Open a file and return a buffer
(defalias 'file/open              'find-file-noselect)

;; Close file. kill the buffer bounded to the file.
;;
(defun file/close (filename)
  (bind-nil #'kill-buffer (get-file-buffer filename))
  )

(defun file/concat-path (base relpath)
  (concat (file-name-as-directory base) relpath))

(defun file/emacs-dir (filename)
  "Returns a file with relative path to user emacs directory,
   generally ~/.emacs.d
  "
  (file/concat-path user-emacs-directory filename))

(defun file/delete (filename)
  (delete-file filename))

(defun file/copy (filename dest)
  (copy-file filename dest))

(defun file/path (filename)
  (file-name-directory filename))

(defun file/abs-path (filename)
  (file-name-directory
    (expand-file-name filename)))

(defun file/basename (filename)
  (file-name-base filename))

(defun file/replace-ext (filename extension)
  "Change extension of filename

  Example:

   ELISP> (file/replace-ext \"/tmp/tools.el\" \".html\")
   \"/tmp/tools.html\"

   ELISP> (file/replace-ext \"tools.el\" \".html\")
   \"tools.html\"

  "
  (letc
   (path     (file/path filename)
    newname  (concat (file/basename filename) extension))

    (if path
        (file/concat-path path  newname)
         newname)))



(defun file/exists (filename)
  (file-exists-p filename))

(defun file/is_file (filename)
  (not (file-directory-p filename)))

(defun file/is_dir (filename)
  (file-directory-p filename))

(defun file/read (filename)
  (interactive "fFind file: ")
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun file/write (filename content)
  (progn
   (when (file-exists-p filename) (delete-file filename))
   (append-to-file content nil filename)))

;; Append Content to a file
(defun file/write-append (filename content)
  (append-to-file content nil filename))

(defun file/dir-list (dirpath)
  " List directory with absolute path"
  (letc
    (abs-dirpath  (expand-file-name dirpath))
    (-->
     (directory-files abs-dirpath)
     (remove-from-list '("." ".."))
     (map ($f file/concat-path abs-dirpath %)))))


(defun dir/list-abs (dirpath)
  " List directory with absolute path"
  (letc
    (abs-dirpath  (expand-file-name dirpath))
    (-->
     (directory-files abs-dirpath)
     (remove-from-list '("." ".."))
     (map ($f file/concat-path abs-dirpath %)))))


(defun dir/list-abs-tags (dirpath)
  "Returns a plist (:files filelist dirs: dirlist)"
  (letc
   (content  (dir/list-abs dirpath))
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
  (read (file/read filename)))

(defun str->sexp (str)
  (read str))

(defun sexp->str (sexp)
  (prin1-to-string sexp))

(defun sexp->file (sexp filename)
  (--> sexp
       sexp->str
       (file/write filename)))

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
  "Evaluate a string containing a emacs-lisp code"
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

   fpath    (file/concat-path dir fname))

  (princ (list dir fname fpath))
  ($-> url
      (url-http-get $ nil)
      (file/write fpath $))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @SECTION: Buffer Utils. Useful Functions to manipulate buffers.
;;
;;

(defun open-as-root (filename)
  (interactive)
  (find-file (concat "/sudo:root@localhost:"  filename)))

(defalias 'buffer/at-point 'thing-at-point)
(defalias 'buffer/switch   'switch-to-buffer)
(defalias 'buffer/set      'buffer-set)
(defalias 'buffer/current  'current-buffer)

(defun buffer/get (&optional arg)
  (if (not arg)
      (current-buffer)
    (if (bufferp arg)
        arg
      (get-buffer arg))))

(defun buffer/insert (buf text)
  "Insert a text in a buffer, buf is a buffer object or text
   if buf is nill it will insert the text in current buffer
  "
  (with-current-buffer (buffer/get buf)
    (insert text)))


(defun buffer/scratch ()
  (interactive)
  "Create an scratch buffer in Emascs 
   Lisp mode and Switch to it, or if the buffer 
    exists, only switch to it.

  Usage M-x buffer/scratch
  "
  (let ((buf (get-buffer-create "*scratch*")))
    (switch-to-buffer buf)
    (emacs-lisp-mode)))


(defun buffer/name (&optional arg)
  (buffer-name (buffer/get arg)))

(defun buffer/file (&optional buffer-or-name)
  "Return name of file bounded to buffer"
  (interactive)
  (buffer-file-name (buffer/get buffer-or-name)))

(defun buffer/dir (&optional buffer-or-name)
  "Return name of file bounded to buffer"
  (interactive)
  (bind-nil #'buffer-file-name
            (buffer/get buffer-or-name)))


(defun buffer/save (&optional buf)
  (with-current-buffer (buffer/get buf)
    (save-buffer (buffer/get buf)))
  )

(defun buffer/save-kill (&optional buf)
  "Save a buffer and then delete it, if no argument is given,
   it will happen to current buffer"
  
  (with-current-buffer (buffer/get buf)
    (save-buffer)
    (kill-buffer)))

(defun buffer/close-all ()
  "Save and close all files and close all directories (dired-mode)"
  (interactive)
  (progn
    (mapc (fcomp car buffer/save-kill) (buffer/list-files))
    (buffer/save-kill-mode 'dired-mode)))
  

(defun buffer/text (&optional buffer-or-name)
  "Returns all the buffer content as string"
  (with-current-buffer (buffer/get buffer-or-name)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun buffer/region ()
  "Get the text selected by the user in current buffer as string"
  (interactive)
  (buffer-substring-no-properties (region-beginning) (region-end)))

(defun buffer/line ()
  "Return the current line at the cursor position"
  (interactive)
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))



(defun buffer/write-file (filename &optional buffer-or-name)
  (file/write filename (buffer/text buffer-or-name)))

(defun buffer/erase (&optional buffer-or-name)
  (interactive)
  (with-current-buffer (buffer/get buffer-or-name)
    (erase-buffer)))

(defun buffer/major-mode (&optional buffer-or-name)
  (with-current-buffer (buffer/get buffer-or-name)
    major-mode))

(defun buffer/copy ()
  "Copy entire buffer content to clipboard"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max))
  (message "Copied to clipboard. Ok."))

(defun buffer/copy-path ()
  "Copy buffer file path to clipboard (kill-ring)"
  (interactive)
  (clipboard/copy (buffer/file)))

(defun buffer/insert-file-path ()
  "Insert the path of current buffer file at point."
  (interactive)
  (insert (buffer/file))
  )

(defun buffer/insert-dir-path ()
  "Insert the path of current buffer directory at point"
  (interactive)
  (insert (file/path (buffer/file)))
  )

;; (buffer/major-mode "*Occur*")

(defun buffer/edit-as-root ()
  "Edit current buffer as root"
  (interactive)
  (let
      (
       ;; Get the current buffer file name
       (filename (buffer-file-name (current-buffer)))
       ;; Get the current file name
       (bufname  (buffer-name (current-buffer)))
       )
    (progn
      (kill-buffer bufname)         ;; Kill current buffer
      (open-as-root filename))))    ;; Open File as root

(defun buffer/list-files ()
 "List all buffers bounded to a file"
  (interactive)
  ($-> (buffer-list)

       (filter #'buffer-file-name $)

       (mapcar (juxt #'identity
                     #'buffer-name
                     #'buffer-file-name) $)
       ))

(defun buffer/list ()
  "List the names of all buffers"
  (interactive)
  ($-> (buffer-list)
       (mapcar (juxt #'identity
                     #'buffer/name
                     #'buffer/file
                     #'buffer/major-mode)
               $)))

(defun buffer/open-dir ()
  "Open the directory of current buffer."
  (interactive)
  (find-file (file/path (buffer/file)))
  )

(defun buffer/rename-file ()
  "
  Prompt the user for a new file name to the current buffer file.
  Usage: M-x buffer/rename-file
  "
  (interactive)
  (let (
        (current-name (buffer-file-name))
        (new-name    (read-string "Enter new file name: "))        
        )        
    (rename-file current-name new-name 1)
    (kill-buffer (current-buffer))
    (find-file new-name)
    (message (format "Buffer file name changed from %s to %s" current-name new-name))
    ))


(defun buffer/kill (&optional buffer-or-name)
  (interactive)
  (kill-buffer (buffer/get buffer-or-name)))

(defun buffer/save-kill-mode (mode)
   "Kill all buffer with the same major mode, 
    and save them all"

   (map (fcomp car buffer/save-kill)
        (filter (lambda (a) (equal mode (cdr a)))
               (map-pair #'buffer/major-mode (buffer-list)))))


(defun buffer/attributes (&optional buffer-or-name)
  (interactive)
  (let
      ((buf (buffer/get buffer-or-name)))

    (list
     :name (buffer/name buf)
     :file (buffer/file buf)
     :dir  (buffer/dir  buf)
     :mode (buffer/major-mode buf)
     :buffer buf
     :window-list (get-buffer-window-list buf)
     )))

(defun buffer/list-all-attrs  ()
  (map #'buffer/attributes (buffer-list)))

(defun buffer/make-scratch  (&optional mode_)
  "Create a scratch buffer in any mode not bounded to a file. 
  It is useful to interact with a repl without create 
   new files.

  Example:

  > (buffer/make-scratch 'python-mode) 

  it will create the buffer  *scratch-python-mode*
  "
  (interactive)
  (let* ((mode           (if mode_ mode_ (read (read-string "Scratch buffer mode :"))))
         (buffer-name    (concat "*scratch-" (symbol-name mode) "*"))
         (buffer         (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (funcall mode))
    (if mode_
        ;; Called in non interactive mode return the buffer object
        buffer
      ;; Called in interactive mode M-x buffer/make-scratch
      ;; switch to buffer
        (switch-to-buffer buffer))))


(defun buffer/delete-line ()
  "Delete line at cursor position in current buffer"
  (interactive)
  (delete-region (line-beginning-position)
                 (line-end-position))
  )

(defun buffer/delete-region ()
  "Delete region (selection) in current buffer"
  (interactive)
  (delete-region (region-beginning)
                 (region-end))
  )

(defun buffer/replace-line (line-transf)
  "
  Replace the current line by appliying the
  line-trasf, Line Transformation Function
  to current line.
  "
  (save-excursion
    (let
      ((line (buffer/line)))
      (progn
        (buffer/delete-line)
        (insert (funcall line-transf line))
        ))))

(defun buffer/replace-line (region-transf)
  "
  Replace the current region by appliying the
  region-trasf  - region transformation function
  to current line.
  "
  (save-excursion
    (let
      ((region (buffer/region)))

      (progn
        (buffer/delete-region)
        (insert (funcall region-transf region))
        ))))



(defun buffer/delete-at-point  (thing)
  (let*
      ((bounds (bounds-of-thing-at-point thing))
       (pmin   (car bounds))
       (pmax   (cdr bounds))
       )

    (when (and pmin pmax)
      (delete-region pmin pmax))
    ))

(defun buffer/replace-at-point (thing transf-func)
       (let
           ((content (buffer/at-point thing)))

         (save-excursion
             (buffer/delete-at-point thing)
             (insert (funcall transf-func content)))))


(defun buffer/replace-region (transf)
  "
  Apply the function transf to the selected text in current buffer
  and applies the function transf to the selected text and then 
  replaces it by the result of function application.
  "
  (interactive)
  (save-excursion
    (letc
     (
      rmin      (region-beginning)
      rmax      (region-end)
      content   (buffer-substring-no-properties rmin rmax))

     (delete-region rmin rmax)
     (insert (funcall transf content)))))
                                        ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;  @SECTION: Regions.
;;

(defun region/escape ()
  "
  Escape a region, select the text and type
  M-x region/escape. It is useful to insert
  example int the function docstring.

  "
  (interactive)
  (buffer/replace-region  #'string/escape))


(defun region/unescape ()
  "
  Unescape a region, select the text and type
  M-x region/unescape. 
  "
  (interactive)
  (buffer/replace-region #'string/unescape))


(defun region/shift-right ()
  "
  Interactive function, that shifts the selected text (region)
  n times to right from the beggining of each line.

  Usage: M-x region/shift-right 
  "
  (interactive)
  (letc
   (n  (string-to-number (read-string "shift right ? times : ")))
   
   (buffer/replace-region
    ($f string/replace-regexp "^\\|\n"
        (apply #'concat (repeat n "\t")) %))))

(defun region/shift-left ()
  "
  Interactive function, that shifts the selected text (region)
  to left n times from the beggining of each line.

  Usage: M-x region/shift-left
  "
  (interactive)
  (letc
   (
   n    (string-to-number (read-string "shift right ? times : "))
   tabs (apply #'concat (repeat n "\t"))
   )
   
   (buffer/replace-region
    (fcomp ($f string/replace-regexp (concat "^" tabs) ""    %)
           ($f string/replace-regexp (concat "\n" tabs) "\n" %)))))


(defun region/space-right ()
  "
  Interactive function, that shifts the selected text (region)
  1 space  to right from the beggining of each line.

  Usage: M-x region/space-right
  "
  (interactive)
  
   (buffer/replace-region
    ($f string/replace-regexp "^\\|\n" " "  %)))


(defun region/space-left ()
  "
  Interactive function, that shifts the selected text (region)
  to left n times from the beggining of each line.

  Usage: M-x region/shift-left
  "
  (interactive)
     
   (buffer/replace-region
    (fcomp ($f string/replace-regexp  "^ "  ""  %)
           ($f string/replace-regexp "\n " "\n" %))))


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
  (-> (buffer/region)
      get-url-params
      insert
      ))

(defun get-url-params (url)
  "

  "
  (interactive)
  ($->  url
        (split-string (cadr (split-string $  "?" )) "&")
        (mapcar ($f split-string % "=") $)
        (mapcar ($f  string/join " = " %) $)
        (string/join "\n" $)
        (concat "\n\n" $)
        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   @SECTION:   Cursor Functions                     ;;


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

(defun clipboard/copy (astring)
  "Copy a string to clipboard"
 (with-temp-buffer
  (insert astring)
  (clipboard-kill-region (point-min) (point-max))))

(defun clipboard/paste ()
  "Return the content of clipboard as string"
  (with-temp-buffer
    (clipboard-yank)
    (buffer-substring-no-properties (point-min) (point-max))))



(defun elisp/eval-selection ()
  (interactive)
  ($->
   (buffer/region)
   (concat "(" $ ")")
   (progn (print $)  $)
   read
   (cons 'progn $)
   eval))

;; http://emacs.wordpress.com/2007/01/17/eval-and-replace-anywhere/
(defun elisp/eval-and-replace-last-sexp ()
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

(defun elisp/function-at-point-exists ()
  "Test if variable at the cursor is a function"
  (interactive)
    (if (functionp (intern (thing-at-point 'filename)))
        (message "Function exists OK.")
      (message "Function not found")))

(defun elisp/variable-at-point-exists ()
  "Test if symbol at cursor is a variable"
  (interactive)
    (if (boundp (intern (thing-at-point 'filename)))
        (message "Variable exists OK.")
        (message "Variable not found")))

(defun elisp/value-at-point ()
  "Show the value of variable at point. If it exists."
  (interactive)
  (letc (str       (thing-at-point 'filename)
         sym       (if str (intern str) nil)
         value     (if sym (symbol-value sym) nil))
         (print value)))

(defun elisp/find-defuns ()
  "Find all defun statements in the current buffer"
    (interactive)
    (occur "defun"))

(defun elisp/find-macros ()
  "Find all macros defined in a elisp file"
  (interactive)
  (occur "defmacro"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  @SECTION: Org Mode Tools
;;

(defvar __org-mode-html-buffer "*Org HTML Export*")

(defun org-html (&optional buffer-or-name)
  (interactive)
  (with-current-buffer (buffer/get buffer-or-name)
    (org-html-export-as-html)
    (buffer/write-file
     (file/replace-ext (buffer/file buffer-or-name) ".html")
     __org-mode-html-buffer)
    (buffer/kill __org-mode-html-buffer)
    (message "Ok.")
  ))

(defun org-download-to-buffer  (&optional url_)
  (interactive)
  "
  Dowload an org file document to a temporary buffer in
  mode org-mode.

  Example:

  (org-download-to-buffer
 \"https://raw.githubusercontent.com/erikriverson/org-mode-R-tutorial/master/org-mode-R-tutorial.org\")

  Or M-x org-download-to-buffer

  "
  (letc
   (
    temp    (get-buffer-create "org-view")
    url     (if url_
                url_
                (read-string "url :"))

    content (url-http-get url nil)
    )

   (switch-to-buffer temp)
   (erase-buffer)
   (save-excursion
     (org-mode)
     (insert content))))


(defun org-tools/create-link-from-url (url)
  (format "[[%s][%s]]"
          url

       (capitalize
        (replace-regexp-in-string ".asp" ""
        (replace-regexp-in-string ".asp" ""
        (replace-regexp-in-string ".html" ""
        (replace-regexp-in-string ".htm" ""
        (replace-regexp-in-string "%20" " "
        (replace-regexp-in-string "[_|-]" " "
        (car (last (string/split "/"
           (replace-regexp-in-string "/$" "" url)
        ))))))))))))


(defun org-tools/paste-url-link ()
  "Paste URL in Org-mode"
  (interactive)
  (save-excursion
    (insert
     (org-tools/create-link-from-url (clipboard/get)))))


(defun org/replace-url-for-link ()
  (interactive)
  (buffer/replace-at-point
   'url
   #'org-tools/create-link-from-url))

(defun org-toc ()
  (interactive)
  (occur "^\\*+\s+"))

(defun insert-header ()
  "Insert a header in the file."
  (interactive)
  (save-excursion
    
    (goto-char (point-min))
    
    (insert
     (string/template
      (string/trim-all
        "  
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;
        ;; File:     <% (file/filename (buffer/file)) %>		
	    ;; Author:   <% user-full-name %>		
		;; Email:    caiorss.rodrigues@gmail.com		
		;;		
		;; Created: <% (format-time-string \"%Y-%m-%d\") %>		
		;; 
        ;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        "
     )))))


(defun paste-at-point ()
  (interactive)
  (letc
   (
    offset     (- (point) (line-beginning-position))
    spaces     (apply #'concat "\n" (repeat offset " " ))  
    )
   
   (save-excursion
     (insert
      (string/replace-regexp
       "\n"
       spaces
       (clipboard/paste )
   )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;@section: ast - Tools to generate documentation of lisp code.
;;


(defun ast/str->ast (str)
  (str->sexp (format "\n( %s \n)" str)))

(defun ast/read-file (filename)
  "
   Convert a lisp source code to AST, abstract syntax tree,
   in other words, parses a lisp file.
   "
   (ast/str->ast (file/read filename)))

(defun ast/get-functions (ast)
  "Get all functions from an Elisp AST"
   (map #'cadr (filter ($f equal 'defun (car %))  ast))) 

(defun ast/get-functions-docstring (ast)
  (map ($f list
           :name   (cadr %)
           :params (caddr %)
           :doc    (cadddr %))
   (filter
    ($f and (equal 'defun (car %)) (stringp  (cadddr %)))
    ast)))

(defun ast/ast->elispdoc (ast &optional buffer-name)
  (let*
      ((buf  (get-buffer-create (or buffer-name "*doc*"))))
    
  (with-current-buffer  buf
    (org-mode)
    (buffer/erase)
    (save-excursion
    (mapc
     (lambda (%)
       (progn
         (insert (format "** %s\n" (plist-get % :name )))
         (insert (format "Parameters:  %s\n" (plist-get % :params )))
         (insert (format "Doc:\n  %s\n" (plist-get % :doc)))))
     (ast/get-functions-docstring ast)))

  buf)))

(defun ast/buffer-doc ()
  "Generate documentation of function of current buffer, 
   it is like Java doc for Emacs Lisp.

  Usage: M-x ast/buf
  "
  (interactive)
  (switch-to-buffer-other-window
   (ast/ast->elispdoc
    (ast/str->ast (buffer/text)))))



;; (define-mode-keys emacs-lisp-mode-map
;;     "C-c C-c" #'eval-defun
;;     "C-c C-b" #'eval-buffer
;;     "C-c C-r" #'eval-and-replace-last-sexp
;;     "C-j"     #'eval-print-last-sexp
;;     "M-m"     #'macro-expand-at-point
;;     "C-M-j"   #'eval-selection
;;     "C-c C-y" #'copy-sexp-at-point
;;     )



;; (add-hook 'emacs-lisp-mode-hook
;;           (lambda ()
;;             (setq show-paren-style 'expression)
;;             (setq show-paren-delay 0)
;;              (electric-pair-mode 1)
;;              (show-paren-mode 1)
;;              (turn-on-auto-fill)
;;              (turn-on-eldoc-mode)
;;              ))



;; ;; Disable E-shell banner message
;; (setq eshell-banner-message "")

;; (add-hooks 'eshell-mode-hook
;;            (lambda ()
;;              (add-to-list 'eshell-visual-commands "ssh")
;;              (add-to-list 'eshell-visual-commands "htop")
;;              (add-to-list 'eshell-visual-commands "ncmpcpp")
;;              (add-to-list 'eshell-visual-commands "tail")
;;            ))
