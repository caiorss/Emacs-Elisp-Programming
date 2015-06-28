# Emacs - Elisp Programming and Customization

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**  *generated with [DocToc](https://github.com/thlorenz/doctoc)*

- [Elisp](#elisp)
  - [Ielm - Elisp shell](#ielm---elisp-shell)
  - [Basic Syntax](#basic-syntax)
    - [Basic Operations](#basic-operations)
    - [Defining Variables](#defining-variables)
    - [Defining Functions](#defining-functions)
    - [List Operations](#list-operations)
    - [Association Lists](#association-lists)
    - [Strings](#strings)
    - [Eval](#eval)
    - [Control Structures](#control-structures)
  - [Functional Programming](#functional-programming)
  - [Bufffers](#bufffers)
  - [Files and Directories and OS Interface](#files-and-directories-and-os-interface)
  - [Windows Functions](#windows-functions)
- [Discoverability / Get Documentation](#discoverability--get-documentation)
- [Bytecodes](#bytecodes)
- [Documentation](#documentation)
- [Customization](#customization)
  - [Hide / Show Emacs Widgets](#hide--show-emacs-widgets)
  - [Themes](#themes)
  - [Misc](#misc)
- [Solutions](#solutions)
  - [Refresh/ Reload File](#refresh-reload-file)
  - [Extract Function Documentation](#extract-function-documentation)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->


Emacs is an scriptable text editor written in the lisp dialect: Elisp.

* http://homepage1.nifty.com/bmonkey/emacs/elisp/completing-help.el
* http://www.reallysoft.de/code/emacs/snippets.html#b4ac15 

**Configuration File**

``` 
~/.emacs.d/init.el
```

## Elisp

### Ielm - Elisp shell

Keys

```
M - Meta (Alt)
C - Ctrl 
```

Emacs Elisp: Shell Ieml

```
M-x ielm
```

### Basic Syntax

#### Basic Operations

```elisp
ELISP> (+ 20 30)
50 (#o62, #x32, ?2)
ELISP> (- 100 80)
20 (#o24, #x14, ?\C-t)
ELISP> (+ 1 2 3 4 5 6)
21 (#o25, #x15, ?\C-u)
ELISP> (* 1 2 3 4 5 6)
720 (#o1320, #x2d0, ?ː)
ELISP> (/ 1 100) 
0 (#o0, #x0, ?\C-@)

ELISP> (> 10 1) ;; ?? 10 > 1 
t
ELISP> (< 2 8) ;; ?? 2 < 8 
t
ELISP> (< 8 2) ;; ?? 8 < 2
nil

ELISP> (= 2 2)
t
ELISP> (= 2 4)
nil

ELISP> (/= 2 2)
nil
ELISP> (exp -1)
0.36787944117144233
ELISP> (log 10)
2.302585092994046
ELISP> (sin pi)
1.2246467991473532e-16
ELISP> (cos pi)
-1.0
ELISP> (tan (/ pi 2))
1.633123935319537e+16
ELISP> 
```

Lists

```
ELISP> 
ELISP> '(10 20 30 40)
(10 20 30 40)

ELISP> '(10 203 40 "hello" () ("empty" 65))
(10 203 40 "hello" nil
    ("empty" 65))

ELISP> 
```



#### Defining Variables

```elisp
ELISP> (setq x 10)
10 (#o12, #xa, ?\C-j)
ELISP> (set avar "hello world")

ELISP> (setq avar "hello world")
"hello world"

ELISP> x
10 (#o12, #xa, ?\C-j)

ELISP> avar
"hello world"
ELISP>

ELISP> (setq my-list '(10 20 30 40))
(10 20 30 40)

ELISP> my-list
(10 20 30 40)

;; Dynamic Scoping
;; 
;;
ELISP> (let ((x 1) (y 10)) (+ (* 4 x) (* 5 y)) )
54 (#o66, #x36, ?6)
ELISP> x
10 (#o12, #xa, ?\C-j)
ELISP> y
*** Eval error ***  Symbol's value as variable is void: y
ELISP> 
```

#### Defining Functions


```elisp
ELISP> (defun afunction (a b c) (+ a b c))
afunction

ELISP> (afunction 10 20 30)
60 (#o74, #x3c, ?<)

ELISP> (defun myfun () (message "Hello Emacs"))
myfun
ELISP> (myfun)
"Hello Emacs"
ELISP> 
```
#### List Operations

See also: http://www.fincher.org/tips/Languages/Emacs.shtml


```elisp

;; Defining a List
;; 
;; A emacs list can contain elements of almost any type.
;;
ELISP> '( "a" 2323 "b" 21.2323 "hello" "emacs"   nil () (34 134) '(+ 2 3 5))
("a" 2323 "b" 21.2323 "hello" "emacs" nil nil
 (34 134)
 '(+ 2 3 5))

ELISP> (quote  (1 3 3 4 5))
(1 3 3 4 5)

;;;;; Empty List
;;
ELISP> nil
nil
ELISP> '()
nil
ELISP> 

;; Length of a list
ELISP> (length '(1 2 3 4 5 6))
6 (#o6, #x6, ?\C-f)
ELISP> 


;; nth element of a list
;;
ELISP> (nth 0 '(0 1 2 3 4 5))
0 (#o0, #x0, ?\C-@)
ELISP> (nth 2 '(0 1 2 3 4 5))
2 (#o2, #x2, ?\C-b)
ELISP> (nth 5 '(0 1 2 3 4 5))
5 (#o5, #x5, ?\C-e)
ELISP> (nth 10 '(0 1 2 3 4 5))
nil
ELISP>


;; Membership test
;; mem returns null if the element is not member of the list
;;
ELISP> (member 2 '(0 1 2 3 4 5))
ELISP> (member 2 '(0 1 2 3 4 5))
(2 3 4 5)

ELISP> (member 10 '(0 1 2 3 4 5))
nil
ELISP> 

;; Postion of list element
;;
ELISP> (position 7 '(5 6 7 8))
2 (#o2, #x2, ?\C-b)

ELISP> (position 17 '(5 6 7 8))
nil
ELISP> 

;; cdr
;;
;; Removes first element of the list, returns the list tail.
;;
ELISP> (cdr '(1 2 3 4 5))
(2 3 4 5)

;; car 
;; 
;; Returns the first list element
;;
ELISP> (car '(1 2 3 4 5))
1 (#o1, #x1, ?\C-a)
ELISP> 


;; cons 
;;
;; List constructor
;;
ELISP> (cons 10 '(1 2 3 4))
(10 1 2 3 4)

ELISP> (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 '())))))
(1 2 3 4 5)

;; Last element of a list
;;
;;
ELISP> (car (last '(1 2 3 4 5)))
5 (#o5, #x5, ?\C-e)
ELISP> 


;; Reverse a list
;;
ELISP> (reverse '(1 2 3 4 5))
(5 4 3 2 1)


;; Append lists
;; 
;; Note: nil also means an empty list
;;
ELISP> (append '(1 2) '( "a" "b" "c" "d"))
(1 2 "a" "b" "c" "d")

ELISP> (append '(1 2) nil '( "a" "b" "c" "d") nil)
(1 2 "a" "b" "c" "d")



;; Filter list elements given a predicate function
;;
;;
ELISP> (remove-if-not (lambda (x) (> x 2))     '(1 2 3 4 5 6 7 8 9 10))
(3 4 5 6 7 8 9 10)


;; Drop the firsts n elements of a list
;;
;;
ELISP> (nthcdr 2 '(1 2 3 4))
(3 4)

ELISP> (nthcdr 3 '(1 2 3 4))
(4)

ELISP> (nthcdr 13 '(1 2 3 4))
nil
ELISP> 

;; Delete an element of a list
;;
;;
ELISP> (delq 1 '(1 2 3 4))
(2 3 4)


ELISP> (delq 10 '(1 2 3 4))
(1 2 3 4)

;; It doesn't work to delete sublists
;;
ELISP> (delq (5) '(1 2 (5) 3 4))
*** Eval error ***  Invalid function: 5
ELISP> (delq '(5) '(1 2 (5) 3 4))
(1 2
   (5)
   3 4)

ELISP> (delete '(5) '(1 2 (5) 3 4))
(1 2 3 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Convert Vector to List
;;
;;
ELISP> (coerce [1 2 3] 'list)
(1 2 3)

;; Convert List to Vector
;;
ELISP> (coerce '(1 2 3) 'vector)
[1 2 3]
```

#### Association Lists

Reference: [Emacs Manual / Association Lists](http://www.delorie.com/gnu/docs/elisp-manual-21/elisp_89.html)

```emacs

ELISP> (setq dict   
'((pine . cones)
 (oak . acorns)
 (maple . seeds)))
((pine . cones)
 (oak . acorns)
 (maple . seeds))

ELISP> dict
((pine . cones)
 (oak . acorns)
 (maple . seeds))

;; Get a cell associated with a key
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ELISP> 
ELISP> (assoc 'oak dict)
(oak . acorns)

ELISP> (assoc 'wrong dict)
nil

;; Get a Key
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ELISP> (car (assoc 'oak dict))
oak
ELISP> (cdr (assoc 'oak dict))
acorns
ELISP> 


ELISP> (car (assoc 'oak dict))
oak
ELISP> 

;; Get all keys
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ELISP> (mapcar (lambda (cell)( car cell)) dict)
(pine oak maple)

;; Get all values
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ELISP> (mapcar (lambda (cell)(cdr cell)) dict)
(cones acorns seeds)

```


#### Strings

Text Formating

```elisp
ELISP> (format-time-string "%Y/%m/%d %H:%M:%S" (current-time))
"2015/06/26 06:10:04"
ELISP> 
ELISP> 
ELISP> (mapconcat 'identity '("aaa" "bbb" "ccc") ",")
"aaa,bbb,ccc"
ELISP> (split-string "aaa,bbb,ccc" ",") 
ELISP> (split-string "aaa,bbb,ccc" ",")
("aaa" "bbb" "ccc")

ELISP> (string-width "hello world")
11 (#o13, #xb, ?\C-k)
ELISP> 
ELISP> (substring "Freedom Land" 0 5)
"Freed"
ELISP> 
ELISP> (string-match "ce" "central park")
0 (#o0, #x0, ?\C-@)
ELISP> (string-match "gt" "central park")
nil
ELISP> 
```

#### Eval

**Eval Sexp or S-expressions**

```elisp
ELISP> (eval '(+ 1 2 3 4 5))
15 (#o17, #xf, ?\C-o)
ELISP> 


ELISP> '(defun func1(x)(* 10 x))
(defun func1
    (x)
  (* 10 x))

ELISP> 

ELISP> '((+ 1 3) (* 4 5) (- 8 9))
((+ 1 3)
 (* 4 5)
 (- 8 9))

ELISP> (eval '(defun func1(x)(* 10 x)))
func1
ELISP> (func1 5)
50 (#o62, #x32, ?2)
ELISP> 


ELISP> (mapcar 'eval '((+ 1 3) (* 4 5) (- 8 9)))
(4 20 -1)

```

**Eval Strings**

```elisp
ELISP> (defun eval-string (str) (eval (read str)))
eval-string

ELISP> (eval-string "(+ 1 2 3 4 5 6)")
21 (#o25, #x15, ?\C-u)
ELISP> 

ELISP> (eval-string "(defun func2(x)(* 10 x)))")
func2
ELISP> (func2 6)
60 (#o74, #x3c, ?<)
ELISP> 
```

**Enter Emacs Lisp mode**

```
M-x emacs-lisp-mode
```

Or

```
emacs-lisp-mode
```

**Eval Commands in Elisp mode**

* Reference: [Evaluating Emacs Lisp Expressions](http://www.gnu.org/software/emacs/manual/html_node/emacs/Lisp-Eval.html)

Evaluate the defun containing or after point, and print the value in the echo area (eval-defun).

```
M-x eval-defun

or

(eval-defun)
```

Evaluate all the Emacs Lisp expressions in the region.
```
M-x eval-region

or

(eval-region)
```

Evaluate all the Emacs Lisp expressions in the current buffer/ window.
```
M-x eval-buffer

or

(eval-buffer)
```

Open a prompt, request user input in current buffer and evalutes.
```
M-x eval-expression
```

Eval/ Load a File
```
M-x load-file

or

(load-file "/path/my_lisp_commands.el")
```



#### Control Structures

**Conditional Statement**

```

;;
;; Any value different from nil or '() is true, otherwise false.
;;

;; True
;;
ELISP> (if t 5 6)
5 (#o5, #x5, ?\C-e)
ELISP> (if 10 5 6)
5 (#o5, #x5, ?\C-e)
ELISP> (if 0 5 6)
5 (#o5, #x5, ?\C-e)

;; False
ELISP> (if nil 5 6)
6 (#o6, #x6, ?\C-f)
ELISP> (if '() 5 6)
6 (#o6, #x6, ?\C-f)
ELISP> 

;; Inverting Predicate
;;
ELISP> (if (not t) 5 6)
6 (#o6, #x6, ?\C-f)
ELISP> (if (not nil) 5 6)
5 (#o5, #x5, ?\C-e)
ELISP> 

ELISP> (if (< 5 10)  (message "less than 10") (message "greater or equal to 10") )
"less than 10"
ELISP> (if (< 30 10)  (message "less than 10") (message "greater or equal to 10") )
"greater or equal to 10"
ELISP> 


ELISP> (when t 3)
3 (#o3, #x3, ?\C-c)
ELISP> (when nil 3)
nil
ELISP> 


ELISP> (setq a 3)       ;; a = 3
3 (#o3, #x3, ?\C-c)
ELISP> 

ELISP> (cond
        ((evenp a) a)       ;; if   (a % 2 == 0)    ==> a
        ((> a 7) (/ a 2))   ;; elif (a > 7)         ==> a/2
        ((< a 5) (- a 1))   ;; elif (a < 5)         ==> a-1
        (t 7)               ;; else                 ==> 7
        )
2 (#o2, #x2, ?\C-b)
ELISP> 
```

**Loops**

```elisp

ELISP> (setq a 4)
4 (#o4, #x4, ?\C-d)

ELISP> (loop
        (setq a (+ a 1))
        (when (> a 7) (return a)))
8 (#o10, #x8, ?\C-h)

ELISP> a
8 (#o10, #x8, ?\C-h)
ELISP> 

ELISP> (loop
   (setq a (- a 1))
   (when (< a 3) (return)))
nil
ELISP> a
2 (#o2, #x2, ?\C-b)
ELISP> 


```


Multiple S expressions

```elisp

```

### Functional Programming

See also: [Dash Library Github repository](https://github.com/magnars/dash.el)

Dash is functional programming library to Emacs with many useful higher order functions.


**Mapcar / Equivalent to map**

```elisp
ELISP> (mapcar (lambda (x) (* x x))   '(1 2 3 4 5 6))
(1 4 9 16 25 36)

ELISP>
```

Anonymous/ Lambda function

```elisp
ELISP> (lambda (x)(* x 10))
(lambda
  (x)
  (* x 10))

ELISP>

ELISP> (funcall (lambda (x)(* x 10)) 5)
50 (#o62, #x32, ?2)
ELISP>

ELISP> (setq my-lambda (lambda (x) (+ (* x 10) 5))) ;; 10 * x + 5
(lambda
  (x)
  (+
   (* x 10)
   5))

ELISP> (funcall my-lambda 10)
105 (#o151, #x69, ?i)
ELISP> (mapcar my-lambda '(1 2 3 4 5))
(15 25 35 45 55)


ELISP>  (setq double (function (lambda (x) (+ x x)) ))
(lambda
  (x)
  (+ x x))

ELISP> (funcall double 22)
44 (#o54, #x2c, ?,)
ELISP> 


;;
;; Apply a function to a list of arguments
;;
;;;;;;;;;;;

ELISP> (apply #'+ '(1 2 3 4 5))
15 (#o17, #xf, ?\C-o)
ELISP> 

ELISP> 
ELISP> (defun f (x y z) (+ (* 10 x) (* -4 y) (* 5 z)))
f
ELISP> (f 2 3 5)
33 (#o41, #x21, ?!)

ELISP> (apply 'f '(2 3 5))
33 (#o41, #x21, ?!)


ELISP> (mapcar (lambda (x) (apply 'f x)) '( (2 3 5) (4 5 6) (8 9 5)))
(33 50 69)



;; Create Higher Order Functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

```

**Function Composition**

From: [Elisp Function Composition](http://nullprogram.com/blog/2010/11/15/)


```elisp
ELISP> ;; ID: f0c736a9-afec-3e3f-455c-40997023e130
(defun compose (&rest funs)
  "Return function composed of FUNS."
  (lexical-let ((lex-funs funs))
    (lambda (&rest args)
      (reduce 'funcall (butlast lex-funs)
              :from-end t
              :initial-value (apply (car (last lex-funs)) args)))))
              compose
              
ELISP> (funcall (compose 'prin1-to-string 'random* 'exp) 10)
"4757.245739507558"
ELISP> 

```

**Interactive Functions**

Interactive functions can be called using: M-x <function>. The user can create custom emacs commands with interactive functions.

```elisp
(defun some-interactive-function ()
   "Documentation"
  (interactive)
  ...)
```

Execute the function

```
M-x some-interactive-function>
```


### Bufffers

```emacs
;; List of Buffers

ELISP> (buffer-list)
(#<buffer *ielm*> #<buffer Emacs.md> #<buffer *Help*> #<buffer  *Minibuf-1*>
    #<buffer *shell*> #<buffer init.el> #<buffer *markdown-output*> #<buffer *Popup Shell*>
    #<buffer dummy.el> #<buffer  *Minibuf-0*> #<buffer  *code-conversion-work*> #<buffer
    *Echo Area 0*> #<buffer  *Echo Area 1*> #<buffer  *code-converting-work*> #<buffer pad>
    #<buffer *scratch*> #<buffer *Messages*>
    #<buffer *Flycheck error messages*> #<buffer *Completions*>)

;; Show Current Buffer
;; 
ELISP> (current-buffer)
    #<buffer *ielm*>
ELISP>

;; Name of all buffers
;;
ELISP> (mapcar (lambda (b)(buffer-name b)) (buffer-list))
("*ielm*" "Emacs.md" "*Help*" " *Minibuf-1*" "*shell*" "init.el" "*markdown-output*"
"*Popup Shell*" "dummy.el" " *Minibuf-0*" " *code-conversion-work*" "
*Echo Area 0*" " *Echo Area 1*" " *code-converting-work*" "pad" "*scratch*"
"*Messages*" "*Flycheck error messages*" "*Completions*")

;; File names of all buffers
;;
;;
ELISP> (mapcar (lambda (b)(buffer-file-name b)) (buffer-list))
(nil "/home/tux/.emacs.d/Emacs.md" nil nil nil
"/home/tux/.emacs.d/init.el" nil nil
"/home/tux/tmp/dummy.el"
nil nil nil nil nil nil nil nil nil nil)


;; Kill Buffer

ELISP> (kill-buffer "pad")
t
ELISP> 

ELISP> (get-buffer "*scratch*")
    #<buffer *scratch*>


;; Create a New Buffer
;;
;;
;; This function returns a buffer named  buffer-or-name.
;; The buffer returned does not become the current
;; buffer—this function does not change which buffer is current.
;;

ELISP> (get-buffer-create "foobar")
    #<buffer foobar>
ELISP> 

;;
;;  Divide the screen in two windows, and switch to the new buffer
;;  window
;;
ELISP> (switch-to-buffer-other-window "foobar")
    #<buffer foobar>
ELISP> 

;; Clean Current Buffer
;;
ELISP> (erase-buffer)
nil
ELISP> 

;;  Edit another buffer and go back to the old buffer
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ELISP> (defun within-buffer (name function) 
         (let (curbuff (current-buffer))
         (switch-to-buffer name)
         (funcall function)
         (switch-to-buffer current-buffer)
       ))

ELISP> (within-buffer "foobar" (lambda () (insert "dummy")))
    #<buffer *ielm*>
    ELISP> 
    ELISP> (lambda (x)(* x 10))
    (lambda
      (x)
      (* x 10))

;;;; Translated from: http://d.hatena.ne.jp/rubikitch/20100201/elispsyntax
;;
ELISP> ;; test-buffer Create a buffer named, to write a variety of content
(with-current-buffer (get-buffer-create "test-buffer")
  ;; Empty the contents of the buffer
  (erase-buffer)
  ;; /tmp/foo.txt Make the contents inserted
  (insert-file-contents "/etc/fstab")
  ;; Insert a string
  (insert "End\n")
  ;; Write the contents of a buffer to a file
  (write-region (point-min) (point-max) "/tmp/bar.txt"))
nil
ELISP> 

```

### Files and Directories and OS Interface

```elisp
;; Get and Set current directory

ELISP> (pwd)
"Directory /home/tux/tmp/"

ELISP> (cd "/etc/")
"/etc/"

ELISP> (pwd)
"Directory /etc/"
ELISP> 


ELISP> (file-name-directory "/etc/hosts")
"/etc/"

;; Expand File Name
;;
ELISP> (expand-file-name "~/")
"/home/tux/"
ELISP> (expand-file-name ".")
"/home/tux/tmp"
ELISP> (expand-file-name "..")
"/home/tux"
ELISP> 


;;;;; Create a Directory
;;;
ELISP> (mkdir "dummy")
nil
ELISP> (mkdir "dummy")
*** Eval error ***  File exists: /home/tux/dummy
ELISP>

;;; List Directory
;;;;
;;;
ELISP> (directory-files "/home/tux/PycharmProjects/Haskell/")
("." ".." ".git" ".gitignore" ".idea" "LICENSE" "Make" "Makefile"
"README.back.md" "README.html" "README.md" "Test.html" "build.sh" "clean.sh"
"codes" "dict.sh" "haskell" "ocaml" "papers" "tags" "tmp")

;;;
;;; Print Current Time    
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (current-time-string)
;;;;;;;;;;
"Sun Jun 21 06:10:28 2015"

;; Year-Month-Day:
(insert (format-time-string "%Y-%m-%d"))

;; Hour:Minutes:Seconds
(insert (format-time-string "%H-%M-%S"))


;; Format Current Time
;;
;;;;;;;
ELISP> (format-time-string "%d/%m/%Y %H:%M:%S" (current-time))
"27/06/2015 22:05:10"
ELISP> 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Call External Command
;;;;;;
;; It will launch Lxterminal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;...
ELISP> (call-process "lxterminal")
0 (#o0, #x0, ?\C-@)
ELISP>
 

;; Shell Command to String
;;;;;;;
ELISP> (shell-command-to-string "pwd")
"/home/tux/PycharmProjects/ocaml/prelude\n"
ELISP
ELISP> (shell-command-to-string "uname" )
"Linux\n"
ELISP> (shell-command-to-string "uname -a" )
"Linux tuxhorse 3.19.0-18-generic #18-Ubuntu SMP Tue May 19 18:30:59 UTC 2015 i686 i686 i686 GNU/Linux\n"
ELISP> 


;; Environment Variables
;;
ELISP> (getenv "PATH")
"/home/tux/.opam/4.02.1/bin:/home/tux/bin:/home/tux/.opam/4.02.1/bin
:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games
:/usr/local/games:/opt/java:/opt/java/bin:/home/tux/bin:/home/tux/usr/bin
:/home/tux/.apps:/opt/jython:/opt/jython/bin:/opt/jython/Lib"

ELISP> (getenv "HOME")
"/home/tux"


;; Detect Operating System 
;;
;;
ELISP> system-type
gnu/linux
ELISP> 

;; Test if the operating system is Linux
ELISP> (eq system-type 'gnu/linux)
t
ELISP> 


```

* [Current Buffer](http://www.gnu.org/software/emacs/manual/html_node/elisp/Current-Buffer.html)
* [Creating New Buffer](http://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Buffers.html)


### Windows Functions

```
balance-windows
delete-other-windows
delete-window
delete-windows-on
display-buffer
shrink-window-if-larger-than-buffer
split-window
split-window-horizontally
split-window-vertically
switch-to-buffer
switch-to-buffer-other-window
other-window
other-window-for-scrolling

;; Open a new Emacs Window
(make-frame)
```

* http://ecb.sourceforge.net/docs/The-edit_002darea.html 
* http://www.delorie.com/gnu/docs/elisp-manual-21/elisp_432.html 
* http://www.delorie.com/gnu/docs/elisp-manual-21/elisp_441.html 
* http://www.chemie.fu-berlin.de/chemnet/use/info/elisp/elisp_26.html 


## Discoverability / Get Documentation


```
M-x <apropos command>
```

Apropos Commands

```
apropos
apropos-command
apropos-documentation
info-apropos
apropos-library
apropos-variable
apropos-value
```

## Bytecodes

* http://www.chemie.fu-berlin.de/chemnet/use/info/elisp/elisp_15.html

* [Emacs Bytecodes internals](http://nullprogram.com/blog/2014/01/04/)

## Documentation


* [GNU Emacs Lisp Reference Manual](http://www.delorie.com/gnu/docs/elisp-manual-21/elisp_toc.html#SEC_Contents)

* [Rosetta Code/ Category:Emacs Lisp](http://rosettacode.org/wiki/Category:Emacs_Lisp)

* [Elisp Cookbook](http://emacswiki.org/emacs/ElispCookbook)
* [ErgoEmacs](http://ergoemacs.org/)
* [Essential Elisp Libraries - Functional Programmin in Elisp](http://www.wilfred.me.uk/blog/2013/03/31/essential-elisp-libraries/)


* [Emacs / Arch Wiki](https://wiki.archlinux.org/index.php/Emacs)

* [Emacs Lisp for Perl Programmers](http://obsidianrook.com/devnotes/elisp-for-perl-programmers.html)

* [Hyperglot / Lisp: Common Lisp, Racket, Clojure, Emacs Lisp](http://hyperpolyglot.org/lisp)

**Github**

* [Learn Elisp the Hard Way](https://github.com/hypernumbers/learn_elisp_the_hard_way)

**Lexical Scope**

* [On elisp and programming in general](http://prog-elisp.blogspot.com.br/2012/05/lexical-scope.html)



## Customization

See also: http://www.aaronbedra.com/emacs.d/

### Hide / Show Emacs Widgets

**Hide / Show Menu bar**

Hide Menu Bar

```
(menu-bar-mode 0)
```

Show Menu Bar

```
(menu-bar-mode 1)
```

**Hide / Show Toolbar**

Show Tool Bar

```
(tool-bar-mode 1)
```

Hide Tool Bar

```
(tool-bar-mode 0)
```


**Hide / Show Scroll Bar**

Show

```
(scroll-bar-mode 1)
```

Hide

```
(scroll-bar-mode -1)
```

### Themes

Load a color theme

```
(load-theme 'wombat t)
```

List all available themes

```
ELISP> (custom-available-themes)
(cyberpunk adwaita deeper-blue dichromacy leuven light-blue manoj-dark 
misterioso tango-dark tango tsdh-dark tsdh-light wheatgrass 
whiteboard wombat)

or

M-x custom-available-themes
```

Print Color Theme Info (elisp code)

```
ELISP> (color-theme-print)
"Pretty printing current color theme function... done"

or

M-x color-theme-print
```

### Misc

**Disable/Enable Blink Cursor**

Enable Blink Cursor

```elisp
(blink-cursor-mode 1)
```

Stop Blink Cursor

```elisp
(blink-cursor-mode 0)
```

**Space / Tabs - Identation**

Set identation with spaces instead of tabs with 4 spaces

```
(setq tab-width 4 indent-tabs-mode nil)
```      

**Disable / Enable Backup Files**

Enable

```
(setq make-backup-files t)
```

Disable

```
(setq make-backup-files nil)
```

**Type y/n instead of yes and no**

```
(defalias 'yes-or-no-p 'y-or-n-p)
```

**Show Match Parenthesis**

```
ELISP> (show-paren-mode 1)
t
```

## Solutions

### Refresh/ Reload File

```
M-x reload
```

or

```
;; Source: http://www.emacswiki.org/emacs-en/download/misc-cmds.el
(defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer t t))
```

### Extract Function Documentation

Source:

* [Generate emacs-lisp documentation](http://kitchingroup.cheme.cmu.edu/blog/2014/10/17/Generate-emacs-lisp-documentation/)


```emacs
ELISP> 
ELISP> (defun sample-function (a b c)
           "Function Docstring"
         (+ a (* 5 b) (* 3 c)))
sample-function
ELISP> 

;; Extract Documentation
;;
ELISP> (documentation 'sample-function)
"Function Docstring"

;; Extract Code
;;
ELISP> (symbol-function 'sample-function)
(lambda
  (a b c)
  "Function Docstring"
  (+ a
     (* 5 b)
     (* 3 c)))

;; Extract Arguments
ELISP> (help-function-arglist 'sample-function)
(a b c)

ELISP> 


ELISP> (fun2org 'sample-function)
    "** sample-function (a b c)\nFunction Docstring\n\n#+BEGIN_SRC emacs-lisp\n(lambda (a b c) \"Function Docstring\" (+ a (* 5 b) (* 3 c)))\n#+END_SRC\n"
    ELISP> 
    ELISP> (defun fun2org (function-symbol)
      (let ((args (help-function-arglist function-symbol))
            (doc  (documentation function-symbol))
            (code (symbol-function function-symbol)))
        (print (format "** %s %s
    %s

    #+BEGIN_SRC emacs-lisp
    %S
    #+END_SRC
    " function-symbol args doc code))))
    fun2org
    ELISP> (fun2org 'sample-function)

    "** sample-function (a b c)
    Function Docstring

    #+BEGIN_SRC emacs-lisp
    (lambda (a b c) \"Function Docstring\" (+ a (* 5 b) (* 3 c)))
    #+END_SRC
    "
```


