;; ## added by OPAM user-setup for emacs / base ## 9332ec09ce81285ad181558c43a2e68a ## you can edit, but keep this line
(provide 'opam-user-setup)

;; Base configuration for OPAM

(defun opam-shell-command-to-string (command)
  "Similar to shell-command-to-string, but returns nil unless the process
  returned 0 (shell-command-to-string ignores return value)"
  (let* ((return-value 0)
         (return-string
          (with-output-to-string
            (setq return-value
                  (with-current-buffer standard-output
                    (process-file shell-file-name nil t nil
                                  shell-command-switch command))))))
    (if (= return-value 0) return-string nil)))

(defun opam-update-env (switch)
  "Update the environment to follow current OPAM switch configuration"
  (interactive "sopam switch (empty to keep current setting): ")
  (let* ((switch-arg (if (= 0 (length switch)) "" (concat "--switch " switch)))
         (command (concat "opam config env --sexp " switch-arg))
         (env (opam-shell-command-to-string command)))
    (when env
      (dolist (var (car (read-from-string env)))
        (setenv (car var) (cadr var))
        (when (string= (car var) "PATH")
          (setq exec-path (split-string (cadr var) path-separator)))))))

(opam-update-env nil)

(setq opam-share
  (let ((reply (opam-shell-command-to-string "opam config var share")))
    (when reply (substring reply 0 -1))))

(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
;; OPAM-installed tools automated detection and initialisation

(defun opam-setup-tuareg ()
  (add-to-list 'load-path (concat opam-share "/tuareg") t)
  (load "tuareg-site-file"))

(defun opam-setup-ocp-indent ()
  (require 'ocp-indent))

(defun opam-setup-ocp-index ()
  (require 'ocp-index))

(defun opam-setup-merlin ()
  (require 'merlin)
  (add-hook 'tuareg-mode-hook 'merlin-mode t)
  (add-hook 'caml-mode-hook 'merlin-mode t)
  (set-default 'ocp-index-use-auto-complete nil)
  (set-default 'merlin-use-auto-complete-mode 'easy)
  ;; So you can do it on a mac, where `C-<up>` and `C-<down>` are used
  ;; by spaces.
  (define-key merlin-mode-map
    (kbd "C-c <up>") 'merlin-type-enclosing-go-up)
  (define-key merlin-mode-map
    (kbd "C-c <down>") 'merlin-type-enclosing-go-down)
  (set-face-background 'merlin-type-face "skyblue"))

(defun opam-setup-utop ()
  (autoload 'utop "utop" "Toplevel for OCaml" t)
  (autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)
  (add-hook 'tuareg-mode-hook 'utop-setup-ocaml-buffer))

(setq opam-tools
  '(("tuareg" . opam-setup-tuareg)
    ("ocp-indent" . opam-setup-ocp-indent)
    ("ocp-index" . opam-setup-ocp-index)
    ("merlin" . opam-setup-merlin)
    ("utop" . opam-setup-utop)))

(defun opam-detect-installed-tools ()
  (let*
      ((command "opam list --installed --short --safe --color=never")
       (names (mapcar 'car opam-tools))
       (command-string (mapconcat 'identity (cons command names) " "))
       (reply (opam-shell-command-to-string command-string)))
    (when reply (split-string reply))))

(setq opam-tools-installed (opam-detect-installed-tools))

(defun opam-auto-tools-setup ()
  (interactive)
  (dolist (tool opam-tools)
    (when (member (car tool) opam-tools-installed)
     (funcall (symbol-function (cdr tool))))))

(opam-auto-tools-setup)
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
