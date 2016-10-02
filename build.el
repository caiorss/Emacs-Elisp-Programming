;; Minimal script to build the *.html files and export them
;; to the ./dist directory 
;;
;; Usage: Run with
;;        $ emacs --batch -q -l build.el --kill 
;; 
;;

(setq package-archives
      '(	
	;;("melpa" . "https://melpa.milkbox.net/packages/")
	;;("popkit" . "http://elpa.popkit.org/packages/")
	("melpa" . "https://melpa.org/packages/")		
	("gnu"       . "http://elpa.gnu.org/packages/")
	))

(package-initialize)


(defun packages-require (&rest packs)
  "Install and load a package. If the package is not available
   installs it automaticaly. 
  "
  (mapc  (lambda (package)
           (unless (package-installed-p package)
                   (package-install package)    
                   ;;#'package-require
                   ))

         packs
         
         ))


(packages-require  'htmlize)

(require 'org)
(require 'htmlize)
(require 'ox-publish)


;; Htmlize output set by css 
(setq org-html-htmlize-output-type 'css)
   ;; (setq org-html-htmlize-output-type 'inline-css) ;; default

;;  Org-html htmlize font prefix
(setq org-html-htmlize-font-prefix "org-")

(org-publish '("html"
              :base-directory       "."
              :base-extension        "org"
              :publishing-directory  "./dist"
              :publishing-function    org-html-publish-to-html
              )

             t
             )






