;;; gist-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "gist" "gist.el" (21896 4599 267545 63000))
;;; Generated autoloads from gist.el

(autoload 'gist-region "gist" "\
Post the current region as a new paste at gist.github.com
Copies the URL into the kill ring.

With a prefix argument, makes a private paste.

\(fn BEGIN END &optional PRIVATE CALLBACK)" t nil)

(autoload 'gist-region-private "gist" "\
Post the current region as a new private paste at gist.github.com
Copies the URL into the kill ring.

\(fn BEGIN END)" t nil)

(autoload 'gist-buffer "gist" "\
Post the current buffer as a new paste at gist.github.com.
Copies the URL into the kill ring.

With a prefix argument, makes a private paste.

\(fn &optional PRIVATE)" t nil)

(autoload 'gist-buffer-private "gist" "\
Post the current buffer as a new private paste at gist.github.com.
Copies the URL into the kill ring.

\(fn)" t nil)

(autoload 'gist-region-or-buffer "gist" "\
Post either the current region, or if mark is not set, the
  current buffer as a new paste at gist.github.com

Copies the URL into the kill ring.

With a prefix argument, makes a private paste.

\(fn &optional PRIVATE)" t nil)

(autoload 'gist-region-or-buffer-private "gist" "\
Post either the current region, or if mark is not set, the
  current buffer as a new private paste at gist.github.com

Copies the URL into the kill ring.

\(fn)" t nil)

(autoload 'gist-list "gist" "\
Displays a list of all of the current user's gists in a new buffer.

\(fn &optional FORCE-RELOAD BACKGROUND)" t nil)

(autoload 'gist-fetch "gist" "\


\(fn ID)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; gist-autoloads.el ends here
