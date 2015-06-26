<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**  *generated with [DocToc](https://github.com/thlorenz/doctoc)*

- [Emacs - Elisp Programming and Customization](#emacs---elisp-programming-and-customization)
  - [Elisp](#elisp)
    - [Ielm - Elisp shell](#ielm---elisp-shell)
    - [Basic Syntax](#basic-syntax)
      - [Basic Operations](#basic-operations)
      - [Defining Variables](#defining-variables)
      - [Defining Functions](#defining-functions)
      - [Strings](#strings)
      - [Control Structures](#control-structures)
    - [Functional Programming](#functional-programming)
    - [Bufffers](#bufffers)
    - [Files and Directories and OS Interface](#files-and-directories-and-os-interface)
  - [Documentation](#documentation)
  - [Solutions](#solutions)
    - [Refresh/ Reload File](#refresh-reload-file)
    - [Extract Function Documentation](#extract-function-documentation)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Emacs - Elisp Programming and Customization

Emacs is an scriptable text editor written in the lisp dialect: Elisp.

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

#### Control Structures

If-else statement

```
ELISP> (if (< 5 10)  (message "less than 10") (message "greater or equal to 10") )
"less than 10"
ELISP> (if (< 30 10)  (message "less than 10") (message "greater or equal to 10") )
"greater or equal to 10"
ELISP> 
```

Multiple S expressions

```elisp

```


### Functional Programming

Mapcar / Equivalent to map

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

;; Create Higher Order Functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

```

Function Composition

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

From: [Elisp Function Composition](http://nullprogram.com/blog/2010/11/15/)

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

```

* [Current Buffer](http://www.gnu.org/software/emacs/manual/html_node/elisp/Current-Buffer.html)
* [Creating New Buffer](http://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Buffers.html)


## Documentation

* [Rosetta Code/ Category:Emacs Lisp](http://rosettacode.org/wiki/Category:Emacs_Lisp)

* [Elisp Cookbook](http://emacswiki.org/emacs/ElispCookbook)
* [ErgoEmacs](http://ergoemacs.org/)
* [Essential Elisp Libraries - Functional Programmin in Elisp](http://www.wilfred.me.uk/blog/2013/03/31/essential-elisp-libraries/)

* [Emacs Lisp for Perl Programmers](http://obsidianrook.com/devnotes/elisp-for-perl-programmers.html)

* [Hyperglot / Lisp: Common Lisp, Racket, Clojure, Emacs Lisp](http://hyperpolyglot.org/lisp)

**Github**

* [Learn Elisp the Hard Way](https://github.com/hypernumbers/learn_elisp_the_hard_way)

**Lexical Scope**

* [On elisp and programming in general](http://prog-elisp.blogspot.com.br/2012/05/lexical-scope.html)

* []()

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


