;;
;; Emacs Toolbox
;;
;; Useful Macros and Functions for Emacs - Elisp
;;
;;
;;--------------------------------------------------------



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
  (if (null xs)
      nil
    (progn
      (funcall fun (car xs))
      (for-each fun (cdr xs)))))

(defun filter (fun xs)
  (if (null xs)
      '()
    (let ((hd (car xs))
          (tl (cdr xs)))
      (if (funcall fun hd)
          (cons hd (filter fun tl))
        (filter fun tl)))))

(defun reject (fun xs)
  (if (null xs)
      '()
    (let ((hd (car xs))
          (tl (cdr xs)))
      (if (not (funcall fun hd))
          (cons hd (reject fun tl))
        (reject fun tl)))))

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

(defmacro f$  ( &rest body)
  `(lambda (%) ,body))

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



;; Set may keys at same time. A macro in Clojure-style
;; with minimum amount of parenthesis as possible.
;;
(defmacro set-gl-keys (&rest keylist)
  `(progn
     ,@(map-apply (lambda (key fun)
            `(global-set-key (kbd ,key) ,fun))
          (plist->alist keylist))))

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



(defun dir-list/abs (dirpath)
  " List directory with absolute path"
  (letc
    (abs-dirpath  (expand-file-name dirpath))
    (-->
     (directory-files abs-dirpath)
     (remove-from-list '("." ".."))
     (map (f$ concat-path abs-dirpath %)))))


(defun dir-list/abs-tags (dirpath)
  "Returns a plist (:files filelist dirs: dirlist)"
  (letc
   (content  (dir-list/abs dirpath))
   (list
    :files (reject #'file-directory-p content)
    :dirs  (filter #'file-directory-p content))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;      S expressions                        ;;
;;-------------------------------------------;;

(defun file->sexp (filename)
  (read (read-file filename)))

(defun str->sexp (str)
  (read str))

(defun sexp->str (sexp)
  (format "%s" sexp))

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
       
       
    

(defun eval-string (str)
  (eval (read str)))



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

;;
;; Set global keys for all modes
;;
(set-gl-keys
  "C-c M-u"  #'url-at-point
  "C-c M-f"  #'open-file-at-point
  "C-c M-r"  #'launch-terminal
  "<f8>"     #'execute-extended-command  ;; Same as A-x
  "C-<f8>"   #'open-file-manager
  "C-<f9>"   #'save-view
  "M-<f9>"   #'restore-view
  )





