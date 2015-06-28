;;
;; Author: Caio Rodrigues
;;
;; Emasc Library With Usefun Functions to deal with buffers and files
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; Home Directory on Unix Systems
(defvar HOME (expand-file-name "~"))

;; .init.el file
(defvar dotemacs "~/.emacs.d/init.el")

(setq file-manager "pcmanfm")


(defun reload ()
  "Reload init.el file"
  (interactive)
  (load dotemacs)
  (message "Reloaded OK.")
)

(defun current-dir ()
  "Show current directory"
  (interactive)
  (nth 1 (split-string (pwd))))

(defun current-file ()
  (interactive)
  (buffer-file-name (current-buffer)))


;;;;;;;;;;;;;;;;;;;;;;;
;; Eval Functions
;;;;;;;;;;;;;;;;;;;;;;

(defun read-xclip ()
  "Get Xclip output"
  (interactive)
  (shell-command-to-string "xclip -o -selection clipboard"))

(defun eval-string (str) (eval (read str)))

(defun eval-xclip () (eval-string (shell-command-to-string "xclip -o -selection clipboard")))

(defun copy-to-clipboard (astring)
 (with-temp-buffer
  (insert astring)
  (clipboard-kill-region (point-min) (point-max))))


;;;;;;;;;;;;;;;;;;;;;;;
;;  Open Functions
;;;;;;;;;;;;;;;;;;;;;;

(defun open-as-root (filename)
  (interactive)
  (find-file (concat "/sudo:root@localhost:"  filename)))

(defun open-current-buffer-as-root ()
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


(defun open-current-dir ()
  "Open directory of current buffer"
  (interactive)
  (find-file (file-name-directory (buffer-file-name))))


(defun open-file-manager ()
  "Open buffer directory in file manager (Linux Only)"
  (interactive)
  (call-process file-manager))



;; It will open lxterminal, but you can change the
;; terminal emulator, in Windows to cmd.exe for example.
;;
(defun open-terminal ()
  "Open terminal in file directory"
  (interactive)
  (call-process "lxterminal"
                nil
                (format "--working-directory='%s'"
                        (file-name-directory (buffer-file-name)))))


(defun create-scratch-buffer ()
  "create a scratch buffer"
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

(defun shell-to-buffer (command) (
   "Execute a shell command and print it to a temporary buffer"
    (interactive)
    (switch-to-buffer (get-buffer-create "pad"))
    (erase-buffer)
    (insert (shell-command-to-string command))
)) 



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  FILE FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Write New File and overwrite the old content
;;
(defun write-file-new (filename content)
  (progn
   (when (file-exists-p filename) (delete-file filename)) 
   (append-to-file content nil filename)))

;; Append Content to a file
;;
(defun write-file-append (filename content)
  (append-to-file content nil filename))

(defun read-file-contents (filename)
  (interactive "fFind file: ")
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun opened-files ()
  "List all opened files in the current session"
  (interactive)
  (remove-if 'null (mapcar 'buffer-file-name  (buffer-list))))
 
(defun copy-all ()
  "Copy entire buffer to clipboard"
  (interactive)
  (clipboard-kill-ring-save (point-min)
                            (point-max)))



(setq session-file "~/.emacs.d/lastsession.el")

(defun make-session-code ()
     (interactive)
     (format "(setq last-session-files '%S)" (remove-if 'null (mapcar 'buffer-file-name  (buffer-list)))))


(defun test-session-code ()
  (interactive)
  (insert (make-session-code)))          

(defun save-session () 
    "Save Current Session"
    (interactive)
    (write-file-new session-file (make-session-code)))


(defun load-session ()
  (interactive)
  (load-file session-file)
  (mapcar 'find-file last-session-files)
)

;;;;;;;;;;;;;;;;


(defun evil-tog ()
  "Toggle Evil mode - Vim Emulator mode"
  (if (not evil-mode)
      (progn (message "Evil mode ON" )  (evil-mode 1))
      (progn (message "Evil mode Off")  (evil-mode 0))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; POPUP terminal - Hit F12
;;
;; https://tsdh.wordpress.com/2011/10/12/a-quick-pop-up-shell-for-emacs/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar th-shell-popup-buffer nil)

(defun th-shell-popup ()
  "Toggle a shell popup buffer with the current file's directory as cwd."
  (interactive)
  (unless (buffer-live-p th-shell-popup-buffer)
    (save-window-excursion (shell "*Popup Shell*"))
    (setq th-shell-popup-buffer (get-buffer "*Popup Shell*")))
  (let ((win (get-buffer-window th-shell-popup-buffer))
	(dir (file-name-directory (or (buffer-file-name)
				      ;; dired
				      dired-directory
				      ;; use HOME
				      "~/"))))
    (if win
	(quit-window nil win)
      (pop-to-buffer th-shell-popup-buffer nil t)
      (comint-send-string nil (concat "cd " dir "\n")))))

(global-set-key (kbd "<f12>") 'th-shell-popup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    M E N U S                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 'easymenu)

                  
(easy-menu-define djcb-menu global-map "Utils"
  '("Utils"

     ;; http://emacs-fu.blogspot.com/2008/12/running-console-programs-inside-emacs.html
     ("Shells" ;; submenu
       ["Ielm   - Emacs Lisp Shell"       (ielm)]
       ["Eshell - Emacs Buitin Shell"    (eshell)]
       ["Native Shell "                  (shell)]
       ["Python: run-python"  ( run-python )]
       ["Octave: run-octave"  ( run-octave)]
       
      );; End of shells menu   

     ("Emacs /Elisp"  ;; submenu
       
      ["Edit  init.el" (find-file   dotemacs)]
      ["Edit  .emacs.d/elutils.el" (find-file "~/.emacs.d/elutils.el")]
      ["Open .emac.d dir" (find-file "~/.emacs.d")]
      ["Reload init.el" (reload)]
      ["Ielm   - Emacs Lisp Shell"  (ielm)]
      ["List packages"     (list-packages)]
      ["Install package"   (package-install)]
      ["Eval buffer"   ( eval-buffer ) ]
      ["Emacs command history "   ( list-command-history ) ]
      ["Describe key"  ( describe-key )  ]      
      
     );; End of Emacs Settings


     ("File Utils" ;; submenu
      ["Toggle Evil Mode"                  (evil-tog)]
      ["Create Scratch Buffer"             (create-scratch-buffer)]
      ["Navigation Bar - Navbar"           (nav-toggle)]
      ["Open Directory of Current Buffer"  (open-current-dir) ]
      ["Open Current Buffer directory in File Manager" (open-file-manager)]
      ["Open Terminal"  (open-terminal)]
      ["Copy Buffer to Clipboard" (copy-all)]
      ["Copy Absolute Path to current file" (copy-to-clipboard (current-file))]
      ["Copy Absolute Path to current directory" (copy-to-clipboard (current-dir))]
      
     );; End of File Utils

     ("Sessions" ;;
      ["Save current session"       (save-session)]
      ["Load last session"          (load-session)]
     );;
     
    ("System" ;; submenu
       
      ["Edit .bashrc" (find-file  "~/.bashrc")]
      ["Edit .profile" (find-file "~/.profile")]
      ["Edit .Xresources" (find-file "~/.Xresources")]
      ["Edit .xsessin"    (find-file "~/.xsession")]
      ["See all GNU MAN pages" ( info)]
      ["See a specific Man Page" (woman)]
      
     );; End of Emacs Settings   
       
 )) ;; End of Custom Menu




(message "Reload eltutils.el")

