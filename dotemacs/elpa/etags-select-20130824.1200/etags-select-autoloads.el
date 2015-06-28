;;; etags-select-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "etags-select" "etags-select.el" (21903 56440
;;;;;;  314645 650000))
;;; Generated autoloads from etags-select.el

(let ((loads (get 'etags-select-mode 'custom-loads))) (if (member '"etags-select" loads) nil (put 'etags-select-mode 'custom-loads (cons '"etags-select" loads))))

(defvar etags-select-no-select-for-one-match t "\
*If non-nil, don't open the selection window if there is only one
matching tag.")

(custom-autoload 'etags-select-no-select-for-one-match "etags-select" t)

(defvar etags-select-mode-hook nil "\
*List of functions to call on entry to etags-select-mode mode.")

(custom-autoload 'etags-select-mode-hook "etags-select" t)

(defvar etags-select-highlight-tag-after-jump t "\
*If non-nil, temporarily highlight the tag after you jump to it.")

(custom-autoload 'etags-select-highlight-tag-after-jump "etags-select" t)

(defvar etags-select-highlight-delay 1.0 "\
*How long to highlight the tag.")

(custom-autoload 'etags-select-highlight-delay "etags-select" t)

(defface etags-select-highlight-tag-face '((t (:foreground "white" :background "cadetblue4" :bold t))) "\
Font Lock mode face used to highlight tags." :group (quote etags-select-mode))

(defvar etags-select-use-short-name-completion nil "\
*Use short tag names during completion.  For example, say you
have a function named foobar in several classes and you invoke
`etags-select-find-tag'.  If this variable is nil, you would have
to type ClassA::foo<TAB> to start completion.  Since avoiding
knowing which class a function is in is the basic idea of this
package, if you set this to t you can just type foo<TAB>.

Only works with GNU Emacs.")

(custom-autoload 'etags-select-use-short-name-completion "etags-select" t)

(defvar etags-select-go-if-unambiguous nil "\
*If non-nil, jump by tag number if it is unambiguous.")

(custom-autoload 'etags-select-go-if-unambiguous "etags-select" t)

(autoload 'etags-select-find-tag-at-point "etags-select" "\
Do a find-tag-at-point, and display all exact matches.  If only one match is
found, see the `etags-select-no-select-for-one-match' variable to decide what
to do.

\(fn)" t nil)

(autoload 'etags-select-find-tag "etags-select" "\
Do a find-tag, and display all exact matches.  If only one match is
found, see the `etags-select-no-select-for-one-match' variable to decide what
to do.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; etags-select-autoloads.el ends here
