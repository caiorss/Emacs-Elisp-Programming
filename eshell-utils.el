;; Package: eshell-utils.el
;;
;; Author: Caio Rodrigues - caiorss@gamil.com
;;
;; This package provides utils for Eshell:
;;
;;
;;

(require 'esh-mode)
(require 'ansi-color)

;; (defun eshell-handle-ansi-color ()
;;   (ansi-color-apply-on-region eshell-last-output-start
;;                                   eshell-last-output-end))

;; (add-to-list 'eshell-output-filter-functions 'eshell-handle-ansi-color)

(setq
      ;;eshell-cmpl-cycle-completions nil
      eshell-save-history-on-exit t
      eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'")


(defun eshell/println (x)
   (eshell-print (concat x "\n")))

(defun eshell/e (file)
  "Open file in this window"
      (find-file file))

(defun eshell/ee (file)
  "Open file in another window"
  (find-file-other-window file))


(defun eshell/fe (file)
  "Open file in another frame"
      (find-file-other-frame file))


(defun eshell/list-files ()
  "Returns a list of all files opened in current section"
    (mapcar #'buffer-name (filter #'buffer-file-name (buffer-list))))


(defun eshell/cd-buffer (buffer-or-name)
  (eshell/cd
   (file-name-directory  (buffer-file-name (get-buffer buffer-or-name)))))

(defun eshell/cd-bw ()
  "Enter in the directory of buffer in other window"
  (eshell/cd 
   (file-name-directory
    (buffer-file-name
     (save-selected-window (window-buffer (other-window 1)))))))
 
  

(defun eshell/files ()
  (dolist (f (eshell/list-files)) (eshell/println f)))

(defun eshell/clear ()
  "clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun eshell/cd-home ()
   (cd (expand-file-name "~")))

(defun eshell/cd-root ()
   (cd "/"))

(defun eshell/exec-path ()
  "Show exec-path $PATH variable in Eshell"
  
    (dolist (p exec-path)
       (eshell-print (concat p "\n"))))

(defun eshell/add-exec-path (path)
  "Add path to exec-path"
  
  (add-to-list 'exec-path path))

(defun eshell/mcd (dir)
  "make a directory and cd into it"
  (interactive)
  (eshell/mkdir "-p" dir)
  (eshell/cd dir))


(defun eshell/frame ()
  "Launch a new frame running eshell:
   Usage: M-x eshell/frame
  "
  (interactive)
  (select-frame (new-frame))
  (eshell))

;;
;; Set eshell prompt
;;
(setq eshell-prompt-function
  (lambda nil
    (concat
     "[" (user-login-name) "@" (getenv "HOSTNAME") ":"
     (eshell/pwd)
     "]\n$ ")))



;; scroll to the bottom
(setq eshell-scroll-to-bottom-on-output t)
(setq eshell-scroll-show-maximum-output t)
;; (add-to-list
;;  'eshell-output-filter-functions
;;  'eshell-postoutput-scroll-to-bottom)

(setq eshell-history-size 512)
(setq eshell-cmpl-cycle-completions nil)
(setq eshell-hist-ignoredups t)
(setq eshell-save-history-on-exit t)


(provide 'eshell-utils)

