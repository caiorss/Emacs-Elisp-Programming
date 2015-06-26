;;; caml-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (camldebug) "camldebug" "camldebug.el" (21865 26332
;;;;;;  686782 211000))
;;; Generated autoloads from camldebug.el

(defvar camldebug-command-name "ocamldebug" "\
*Pathname for executing camldebug.")

(autoload 'camldebug "camldebug" "\
Run camldebug on program FILE in buffer *camldebug-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for camldebug.  If you wish to change this, use
the camldebug commands `cd DIR' and `directory'.

\(fn PATH)" t nil)

;;;***

;;;### (autoloads nil nil ("caml-compat.el" "caml-emacs.el" "caml-font-old.el"
;;;;;;  "caml-font.el" "caml-help.el" "caml-hilit.el" "caml-pkg.el"
;;;;;;  "caml-types.el" "caml-xemacs.el" "caml.el" "inf-caml.el")
;;;;;;  (21865 26332 994766 938000))

;;;***

(provide 'caml-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; caml-autoloads.el ends here
