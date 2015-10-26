# Comint Mode

Notes about Comint Mode.


## Comint Mode / REPL / Shell 

Almost all Emacs interpreters interfaces Eshell, IELM, Ruby inferior mode and  Python inferior mode are built with comint mode.

Goal: 

* Find out how to integrate Emacs to Asynchronous process, shells, Repls in a portable and easier to use way. 
* Create Emacs User Interfaces to external apps.
* Integrate REPLs to Emacs.


### Clear Comint Buffer

Credits: [Emacs Detux - Clear Comint Buffers - http://emacsredux.com](http://emacsredux.com/blog/2015/01/18/clear-comint-buffers/) 

```elisp 
(defun comint-clear-buffer ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

;; let's bind the new command to a keycombo
(define-key comint-mode-map "\C-c\M-o" #'comint-clear-buffer)
```

### Dos Interpreter with Comint

Credits: [The magic of comint / curiousprogrammer.wordpress.com](https://curiousprogrammer.wordpress.com/2009/03/27/emacs-comint/)

```elisp 
(require 'comint)

(progn
  (apply 'make-comint "cmd" "cmd" nil '())
  (delete-other-windows)
  (switch-to-buffer-other-window "*cmd*")
  (other-window -1))

(comint-send-string (get-buffer-process "*cmd*") "dir\n")

```

### Prolog Interpreter Interface with comint

Source: 
* [www.geocities.co.jp/SiliconValley-Bay](http://www.geocities.co.jp/SiliconValley-Bay/9285/ELISP-JA/elisp_196.html)

```
(defun run-prolog ()
  "Run an inferior Prolog process, with I/O via buffer *prolog*."
  (interactive)
  (require 'comint)
  (switch-to-buffer (make-comint "prolog" prolog-program-name))
  (inferior-prolog-mode))

```

### Handy functions for navigating history

* Source: http://emacswiki.org/emacs/ComintMode

```elisp 
(defun comint-jump-to-input-ring ()
"Jump to the buffer containing the input history."
    (interactive)
    (progn
        (comint-dynamic-list-input-ring)
        (other-window 1)))

```

### Customizing colors in comint package in Emacs

*  Source: [http://royontechnology.blogspot.com](http://royontechnology.blogspot.com/2008/03/customizing-colors-in-comint-package-in.html)


It will change the color of an emacs shell prompt to yellow, like in Eshell, IELM ...

```elisp
copy-face 'default 'comint-highlight-prompt)
(set-face-foreground 'comint-highlight-prompt "yellow")
```


**Emacs の shell-mode で zsh を有効活用Add Star**

* http://d.hatena.ne.jp/mooz/20090613/p1

```elisp 
(autoload 'ansi-color-for-comint-mode-on "ansi-color"
  "Set `ansi-color-for-comint-mode' to t." t)

(setq ansi-color-names-vector
      ["#000000"           ; black
       "#ff6565"           ; red
       "#93d44f"           ; green
       "#eab93d"           ; yellow
       "#204a87"           ; blue
       "#ce5c00"           ; magenta
       "#89b6e2"           ; cyan
       "#ffffff"]          ; white
      )
(ansi-color-for-comint-mode-on)


;; zsh のヒストリファイル名を設定
(setq comint-input-ring-file-name "~/.histfile")
;; ヒストリの最大数
(setq comint-input-ring-size 1024)


;; 既存の zsh ヒストリファイルを読み込み
(comint-read-input-ring t)

(local-set-key "\M-p" 'comint-previous-matching-input-from-input)
(local-set-key "\M-n" 'comint-next-matching-input-from-input)

;; コントロールシーケンスを利用した色指定が使えるように
(autoload 'ansi-color-for-comint-mode-on "ansi-color"
  "Set `ansi-color-for-comint-mode' to t." t)

(add-hook 'shell-mode-hook
          '(lambda ()
             ;; zsh のヒストリファイル名を設定
             (setq comint-input-ring-file-name "~/.histfile")
             ;; ヒストリの最大数
             (setq comint-input-ring-size 1024)
             ;; 既存の zsh ヒストリファイルを読み込み
             (comint-read-input-ring t)
             ;; zsh like completion (history-beginning-search)
             (local-set-key "\M-p" 'comint-previous-matching-input-from-input)
             (local-set-key "\M-n" 'comint-next-matching-input-from-input)
             ;; 色の設定
             (setq ansi-color-names-vector
                   ["#000000"           ; black
                    "#ff6565"           ; red
                    "#93d44f"           ; green
                    "#eab93d"           ; yellow
                    "#204a87"           ; blue
                    "#ce5c00"           ; magenta
                    "#89b6e2"           ; cyan
                    "#ffffff"]          ; white
                   )
             (ansi-color-for-comint-mode-on)
             )
          )
```

### Selected Source Codes

#### Source run-forth.el

* https://www.complang.tuwien.ac.at/forth/run-forth.el


```elisp 
(defun run-forth (cmd)
  "Run an inferior Forth process, input and output via buffer *forth*.
If there is a process already running in *forth*, just switch to that buffer.
With argument, allows you to edit the command line (default is value
of forth-program-name).  Runs the hooks from inferior-forth-mode-hook
\(after the comint-mode-hook is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)"

  (interactive (list (if current-prefix-arg
             (read-string "Run Forth: " forth-program-name)
             forth-program-name)))
  (if (not (comint-check-proc "*forth*"))
      (let ((cmdlist (split-string cmd "[\t ]")))
    (set-buffer (apply 'make-comint "forth" (car cmdlist)
               nil (cdr cmdlist)))
    (inferior-forth-mode)))
  (setq forth-buffer "*forth*")
  (switch-to-buffer "*forth*"))

(defun forth-send-region (start end)
  "Send the current region to the inferior Forth process."
  (interactive "r")
  (comint-send-region (forth-proc) start end)
  (comint-send-string (forth-proc) "\n"))


(defun forth-send-definition ()
  "Send the current definition to the inferior Forth process."
  (interactive)
  (save-excursion
   (end-of-definition)
   (let ((end (point)))
     (beginning-of-definition)
     (forth-send-region (point) end))))

(defun forth-send-line ()
  "Send the current line to the inferior Forth process."
  (interactive)
  (save-excursion
    (end-of-line)
    (let ((end (point)))
      (beginning-of-line)
      (forth-send-region (point) end)))
      
(defun forth-send-buffer ()
  "Send the current buffer to the inferior Forth process."
  (interactive)
  (forth-send-region (point-min) (point-max)))

(defun forth-send-region-and-go (start end)
  "Send the current region to the inferior Forth process,
and switch to the process buffer."
  (interactive "r")
  (forth-send-region start end)
  (switch-to-forth t))
  
  
(defun forth-proc ()
  "Returns the current forth process. See variable forth-buffer."
  (let ((proc (get-buffer-process (if (eq major-mode 'inferior-forth-mode)
                      (current-buffer)
                    forth-buffer))))
    (or proc
    (error "No current process. See variable forth-buffer"))))

(run-hooks 'run-forth-load-hook)        
```

#### Source: inferior-coq.el

https://web.math.unifi.it/~maggesi/mechanized/emacs/inferior-coq.el

```elisp 

(defun coq-proc ()
  "Return the current coq process.  See variable `coq-buffer'."
  (let ((proc (get-buffer-process (if (eq major-mode 'inferior-coq-mode)
                      (current-buffer)
                      coq-buffer))))
    (or proc
    (error "No current process.  See variable `coq-buffer'"))))


(defun coq-check-region (start end)
  "Run the commmand \"Check\" on the current region."
  (interactive "r")
  (comint-send-string (coq-proc) "Check ")
  (comint-send-region (coq-proc) start end)
  (comint-send-string (coq-proc) ".\n"))



(defun switch-to-coq (eob-p)
  "Switch to the coq process buffer.
With argument, position cursor at end of buffer."
  (interactive "P")
  (if (get-buffer coq-buffer)
      (pop-to-buffer coq-buffer)
      (error "No current process buffer.  See variable `coq-buffer'"))
  (cond (eob-p
     (push-mark)
     (goto-char (point-max)))))

(defun coq-send-region (start end)
  "Send the current region to the inferior Coq process."
  (interactive "r")
  (comint-send-region (coq-proc) start end)
  (comint-send-string (coq-proc) "\n"))

```

### Comint Functions


```
ger-buffer         (<buffer-name>)
get-buffer-process (buffer)

make-comint                          Create a commint based shell 
comint-truncate-buffer              Clean Commint (Inferior process buffer)
comint-send-string                  Send string to buffer in the process
comint-preoutput-filter-functions

;;; Write commint history to a file.
(comint-write-output FILENAME &optional APPEND MUSTBENEW)

(setq ansi-color-for-comint-mode 'filter)

 comint-input-ignoredups        t  ; Ignore duplicates in Comint history

;;;; Use a different Shell: 
;;
;; Set the variable
;;
(setq explicit-shell-file-name "C:/cygwin/bin/bash.exe")


(customize-set-variable 'comint-prompt-read-only t)
(customize-set-variable 'compilation-buffer-name-function
            'sbt-build-buffer-name)
(customize-set-variable 'compilation-error-regexp-alist
            (list scala-compile-error-regex)) 
(set 'compilation-auto-jump-to-first-error t)

(ansi-color-for-comint-mode-on)


  (comint-send-string (coq-proc) "Check ")
  (comint-send-region (coq-proc) start end)
  
  
  
```

### Non Categorized

**Commint Scheme**

http://www.eecs.ucf.edu/~leavens/ui54/WWW/scheme.shtml

```
 key             binding                           
 ---             -------                           
                           
 DEL             backward-delete-char-untabify     
 C-x             Prefix Command                    
 C-d             comint-delchar-or-maybe-eof       
 RET             comint-send-input                 
 C-c             Prefix Command                    
 C-down          comint-next-input                 
 C-up            comint-previous-input             
 ESC             Prefix Command                    
                           
 C-x C-e         scheme-send-last-sexp             
                           
 C-c C-k         scheme-compile-file               
 C-c C-d         comint-send-eof                   
 C-c C-p         comint-previous-prompt            
 C-c C-n         comint-next-prompt                
 C-c C-l         scheme-load-file                  
 C-c C-e         comint-show-maximum-output        
 C-c C-r         comint-show-output                
 C-c C-o         comint-kill-output                
 C-c RET         comint-copy-old-input             
 C-c C-\         comint-quit-subjob                
 C-c C-z         comint-stop-subjob                
 C-c C-c         comint-interrupt-subjob           
 C-c C-w         backward-kill-word                
 C-c C-u         comint-kill-input                 
 C-c C-a         comint-bol-or-process-mark        
 C-c C-x         comint-get-next-from-history      
 C-c SPC         comint-accumulate                 
 C-c ESC         Prefix Command                    
                           
 ESC C-q         indent-sexp                       
 ESC C-x         scheme-send-definition            
 ESC C-l         comint-show-output                
 ESC s           comint-next-matching-input        
 ESC r           comint-previous-matching-input    
 ESC n           comint-next-input                 
 ESC p           comint-previous-input             

```

**Standard Key bindings**

```
;; M-p     comint-previous-input       Cycle backwards in input history
;; M-n     comint-next-input           Cycle forwards
;; M-r     comint-history-isearch-backward-regexp  Isearch input regexp backward
;; M-C-l   comint-show-output          Show last batch of process output
;; RET     comint-send-input
;; C-d     comint-delchar-or-maybe-eof     Delete char unless at end of buff
;; C-c C-a comint-bol-or-process-mark      First time, move point to bol;
;;                      second time, move to process-mark.
;; C-c C-u comint-kill-input            ^u
;; C-c C-w backward-kill-word           ^w
;; C-c C-c comint-interrupt-subjob      ^c
;; C-c C-z comint-stop-subjob           ^z
;; C-c C-\ comint-quit-subjob           ^\
;; C-c C-o comint-delete-output         Delete last batch of process output
;; C-c C-r comint-show-output           Show last batch of process output
;; C-c C-l comint-dynamic-list-input-ring  List input history
```

### Links

* [The story of comint mode by Edward O’Connor on 28 July 2010](http://edward.oconnor.cx/2010/07/comint)

* [Comint Source code: comint.el](https://github.com/emacs-mirror/emacs/blob/master/lisp/comint.el)

* https://stat.duke.edu/courses/Spring98/sta114/comint-extra.el

* [The Magic of Emacs Comint - A Simple Emacs Comint Subscriber](http://web.archive.org/web/20141120121352/http://jareddavison2009.hubpages.com/hub/emacs-comint)

* [3 Interactive Functions that work on Regions](http://tonyballantyne.com/tech/3-interactive-functions-that-work-on-regions/)

* [Matlab and emacs, reading command history](http://www.mathworks.com/matlabcentral/newsreader/view_thread/38988)

* [Comint Mode](http://emacswiki.org/emacs/ComintMode)

* [The magic of comint / curiousprogrammer.wordpress.com](https://curiousprogrammer.wordpress.com/2009/03/27/emacs-comint/)

* [Emacs: Binding Mac Command key combinations in comint/shell mode](http://www.patchworkbeast.com/2010/12/emacs-binding-mac-command-key.html)

* [EMACS and SQLITE as a rapid application development environment](http://lostnation.us/blog/2015/03/01/emacs-and-sqlite-as-a-rapid-application-development-environment/)

* [EMACS Sqlite Mode](http://lostnation.us/blog/2015/03/01/emacs-sqlite-mode/)

* [Experiment Manager](http://lostnation.us/blog//2015/04/03/experiment-manager/)

* [Run PowerShell as a shell within Emacs](http://blogs.msdn.com/b/dotnetinterop/archive/2008/04/10/run-powershell-as-a-shell-within-emacs.aspx)

* [SQL mode](https://alexschroeder.ch/geocities/kensanata/emacs-sql.html)

* [Emacs Tips and Tricks](http://gurmeet.net/computer-science/emacs-tips-and-tricks/)

* [Customizing colors in comint package in Emacs](http://royontechnology.blogspot.com.br/2008/03/customizing-colors-in-comint-package-in.html)

* [Emacs Comint Windows Subprocess](http://ftp.heanet.ie/disk1/www.gnu.org/software/emacs/windows/Sub_002dprocesses.html)

* [Using Emacs and Bash or Csh under MS Windows](http://www.henrykautz.org/Computers/using_the_gnu_development_enviro.htm)

* [Why Use Emacs 1 - Emacs Speaks Statistics](http://blog.yitang.uk/2015/01/28/why-use-emacs-1--emacs-speaks-statistics-ess/)

* http://echidna.maths.usyd.edu.au/kohel/alg/emacs/magma.el

* http://homepage1.nifty.com/blankspace/emacs/sql.html
