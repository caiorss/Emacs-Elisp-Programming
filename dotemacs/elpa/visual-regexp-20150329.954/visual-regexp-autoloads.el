;;; visual-regexp-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "visual-regexp" "visual-regexp.el" (21896 4564
;;;;;;  191797 368000))
;;; Generated autoloads from visual-regexp.el

(autoload 'vr/mc-mark "visual-regexp" "\
Convert regexp selection to multiple cursors.

\(fn REGEXP START END)" nil nil)

(autoload 'vr/replace "visual-regexp" "\
Regexp-replace with live visual feedback.

\(fn REGEXP REPLACE START END)" t nil)

(autoload 'vr/query-replace "visual-regexp" "\
Use vr/query-replace like you would use query-replace-regexp.

\(fn REGEXP REPLACE START END)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; visual-regexp-autoloads.el ends here
