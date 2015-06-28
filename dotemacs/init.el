;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            Emacs Settings                 ;;
;;                                           ;;
;;  Emacs Settings                           ;;
;;                                           ;;
;;                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Home Directory on Unix Systems
(defvar HOME (expand-file-name "~"))

;; .init.el file
(defvar dotemacs "~/.emacs.d/init.el")

(setq file-manager "pcmanfm")

(load-file (expand-file-name "~/.emacs.d/elutils.el"))

;; Disable Auto Save
(setq auto-save-default nil)

(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
						 ("org" . "http://orgmode.org/elpa/")
						 ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" .  "http://marmalade-repo.org/packages/")
                        ))



;; Functional Programming Support for E-lisp
;;
;; https://github.com/caiorss/dash.el
;;
(eval-after-load "dash" '(dash-enable-font-lock))

;; Disable Features in Shell Mode
;;
(add-hook 'shell-mode-hook (lambda()
      (progn
      (yas-minor-mode -1) ;; Disable Yasnippet
      (evil-mode 0)       ;; Disable Evil
)))

;; Disable evil for certain major-modes
(setq evil-disabled-modes-list
      '(eshell-mode
        wl-summary-mode
        compilation-mode
        completion-list-mode
        help-mode))

(when nil
  (defun evil-initialize ()
    (unless (or (minibufferp) (member major-mode evil-disabled-modes-list))
      (evil-local-mode 1)))
  )


;;(load-file "~/.emacs.d/utop.el")

;; Load packages
(require 'desktop)
(require 'tar-mode)


; Enable company-mode
; Use company in Haskell buffers
; (add-hook 'haskell-mode-hook 'company-mode)
                                        ; Use company in all buffers
(add-to-list 'load-path "~/.emacs.d/company")
(add-to-list 'load-path "~/.emacs.d/company-ghc") ;;no need with 24
 (add-hook 'after-init-hook 'global-company-modeo)
(require 'company)

(with-eval-after-load 'company
(add-to-list 'company-backends 'company-ghc))


;; Highlight escape sequences
(add-to-list 'load-path "~/.emacs.d/elpa/highlight-escape-sequences-20130531.1512")
(require 'highlight-escape-sequences)
(hes-mode)
(put 'font-lock-regexp-grouping-backslash 'face-alias 'font-lock-builtin-face)


(add-hook 'after-init-hook 'global-company-mode)
(add-to-list 'company-backends 'company-ghc)
(custom-set-variables '(company-ghc-show-info t))

;; Haskell Settings
;;-------------------------------------------------------------
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(autoload 'ghc-init "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))


(add-to-list 'load-path "~/.emacs.d/evil") ;;no need with 24
(require 'evil)
(evil-mode 0)

;; Enable Navigation Bar
(add-to-list 'load-path "~/.emacs.d/emacs-nav")
(require 'nav)
(nav-disable-overeager-window-splitting)


;; 
;; Load fycheck syntax checker
;;
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Enable IDO Mode
(require 'ido)
(ido-mode t)
'(ido-enable-flex-matching t)

;; Setup Yasnippet
(add-to-list 'load-path
"~/.emacs.d/elpa/yasnippet-20150415.244")
(require 'yasnippet)
(yas-global-mode 1)
(yas/initialize)
(add-hook 'prog-mode-hook #'yas-minor-mode)

;; Show Line numbers in the file
(global-linum-mode t)

;;; Shut up compile saves
(setq compilation-ask-about-save nil)
;;; Don't save *anything*
(setq compilation-save-buffers-predicate '(lambda () nil))


;; Permanent display of line and column numbers is handy.
(setq-default line-number-mode 't)
(setq-default column-number-mode 't)

;; File path clickable
(add-hook 'prog-mode-hook 'compilation-shell-minor-mode)


(setq blink-cursor-mode nil)       ;; stop cursor from blinking
(savehist-mode)                    ;; to save minibuffer history
(setq visible-bell t)              ;; Turn beep off
(setq-default visible-bell t)      ;; Don't beep

;; titlebar = buffer unless filename
(setq frame-title-format '(buffer-file-name "%f" ("%b")))

;; mark current line:
(global-hl-line-mode 1)

;; Disable Splash Screen
(setq inhibit-startup-message t)

;; force emacs to always use spaces instead of tab characters
(setq-default indent-tabs-mode nil)

;; show line and column numbers in mode line
(line-number-mode 1)
(column-number-mode 1)

;; set default tab width to 4 spaces
(setq default-tab-width 4)
(setq tab-width 4)

;; set background of current window (emacs's “frame”) to pale tinge
;;(set-background-color "honeydew")

;(load-theme 'cyberpunk t)
(add-hook 'after-init-hook 
      (lambda () (load-theme 'cyberpunk t)))

;; cua mode on - Ctrl-C cCtrl+v to copy paste
(cua-mode t)
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(transient-mark-mode 1)               ;; No region when it is not highlighted
(setq cua-keep-region-after-copy t) 

;; Unique buffer name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; If we read a compressed file, uncompress it on the fly:
;; (this works with .tar.gz and .tgz file as well)
(auto-compression-mode 1)

;; Disable binding of C-z to iconify a window.
(global-unset-key "\C-z")

;; The following key-binding quits emacs -- we disable it too:
(global-unset-key "\C-x\C-c")

;; Use font-lock everywhere.
(global-font-lock-mode t)

;; We have CPU to spare; highlight all syntax categories.
(setq font-lock-maximum-decoration t)

;; cycle through buffers with Ctrl-Tab (like Firefox)
(global-set-key (kbd "<C-tab>") 'bury-buffer)


;; Initial Emacs Position

    ;; (setq initial-frame-alist
    ;;      '((top . 20)
    ;;        (left . 10)
    ;;        (width . 140)
    ;;        (height . 39) ))

;; Show Matching Parenthesis
(show-paren-mode t)

;; Menu containing recent files
;;
;; recentf stuff
;; recentf - F5
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 500)
(setq recentf-max-menu-items 60)
(global-set-key [f5] 'recentf-open-files)
(defun xsteve-ido-choose-from-recentf ()
  "Use ido to select a recently opened file from the `recentf-list'"
  (interactive)
  (let ((home (expand-file-name (getenv "HOME"))))
    (find-file
     (ido-completing-read "Recentf open: "
                          (mapcar (lambda (path)
                                    (replace-regexp-in-string home "~" path))
                                  recentf-list)
                          nil t)))) 


(defun ido-choose-from-recentf ()
  "Use ido to select a recently visited file from the `recentf-list'"
  (interactive)
  (find-file (ido-completing-read "Open file: " recentf-list nil t)))
 
  ;;; bind it to "C-c f"
(global-set-key (kbd "C-c f") 'ido-choose-from-recentf)



(add-hook 'tuareg-mode-hook 'tuareg-imenu-set-imenu)
(setq auto-mode-alist
      (append '(("\\.ml[ily]?$" . tuareg-mode)
                ("\\.topml$" . tuareg-mode))
              auto-mode-alist)) 
(autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)
(add-hook 'tuareg-mode-hook 'utop-setup-ocaml-buffer)
(add-hook 'tuareg-mode-hook 'merlin-mode)
(setq merlin-use-auto-complete-mode t)
(setq merlin-error-after-save nil)


;; make OCaml-generated files invisible to filename completion
(mapc #'(lambda (ext) (add-to-list 'completion-ignored-extensions ext))
  '(".cmo" ".cmx" ".cma" ".cmxa" ".cmi" ".cmxs" ".cmt" ".annot"))



;========== Show Nav Bar ================== ;;
;(add-to-list 'load-path "~/.emacs.d/nav")
;(require 'nav)
;(nav-disable-overeager-window-splitting)
;;; Optional: set up a quick key to toggle nav
;(global-set-key [f8] 'nav-toggle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Highlighting "TODO", "FIXME" and friends
;;
(add-hook 'prog-mode-hook
               (lambda ()
                (font-lock-add-keywords nil
                 '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))))


;; remembering your position in a file

(setq save-place-file "~/.emacs.d/saveplace") 	;; keep my ~/ clean
(setq-default save-place t)                   	;; activate it for all buffers
(require 'saveplace)                          	;; get the package


(defun ielm-other-window ()
  "Run ielm on other window"
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create "*ielm*"))
  (call-interactively 'ielm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom Functions                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (load-file "~/emacs.d/elutils.el")



              

(defun cmd-prompt (command)
  (interactive "sCommand: ")
  (command-to-buffer command)
)







;; (require 'flycheck)
;(add-hook 'after-init-hook #'global-flycheck-mode)
;(require 'flymake-haskell-multi)
;(add-hook 'haskell-mode-hook 'flymake-haskell-multi-load)


;; (add-to-list 'company-backends 'company-ghc)

; Make Emacs look in Cabal directory for binaries
(let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
  (setenv "PATH" (concat my-cabal-path ":" (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))


; Choose indentation mode
;; Use haskell-mode indentation
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

; Add F8 key combination for going to imports block
(eval-after-load 'haskell-mode
  '(define-key haskell-mode-map [f8] 'haskell-navigate-imports))


;;;;;;;;;;;;;;;;;; Sematic Factor ;;;;;;;;;
;;
;; https://github.com/tuhdo/semantic-refactor
;;

;; (require 'srefactor)
;; (require 'srefactor-lisp)

;; OPTIONAL: ADD IT ONLY IF YOU USE C/C++. 
;; (semantic-mode 1) ;; -> this is optional for Lisp

;(define-key c-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
;(define-key c++-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
;(global-set-key (kbd "M-RET o") 'srefactor-lisp-one-line)
;(global-set-key (kbd "M-RET m") 'srefactor-lisp-format-sexp)
;(global-set-key (kbd "M-RET d") 'srefactor-lisp-format-defun)
;(global-set-key (kbd "M-RET b") 'srefactor-lisp-format-buffer)
