;;; gitconfig-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "gitconfig-mode" "gitconfig-mode.el" (21896
;;;;;;  4771 842304 35000))
;;; Generated autoloads from gitconfig-mode.el

(autoload 'gitconfig-mode "gitconfig-mode" "\
A major mode for editing .gitconfig files.

\(fn)" t nil)

(dolist (pattern '("/\\.gitconfig\\'" "/\\.git/config\\'" "/git/config\\'" "/\\.gitmodules\\'")) (add-to-list 'auto-mode-alist (cons pattern 'gitconfig-mode)))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; gitconfig-mode-autoloads.el ends here
