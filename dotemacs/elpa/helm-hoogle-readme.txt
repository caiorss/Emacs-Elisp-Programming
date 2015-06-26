cabal install hoogle, then run M-x helm-hoogle

(require 'helm)
(eval-when-compile
  (require 'cl))

(defgroup helm-hoogle nil
  "Use helm to navigate query results from Hoogle"
  :group 'helm)

(defvar helm-c-source-hoogle
  '((name . "Hoogle")
    (candidates . helm-c-hoogle-set-candidates)
    (action . (("Lookup Entry" . browse-url)))
    (filtered-candidate-transformer . (lambda (candidates source)
                                        candidates))
    (volatile)
    (delayed)))

(defun helm-c-hoogle-set-candidates (&optional request-prefix)
  (let* ((pattern (or (and request-prefix
                           (concat request-prefix
                                   " " helm-pattern))
                      helm-pattern))
         (short-pattern
          (if (string-match "\\`\\([a-zA-Z_][a-zA-Z0-9_]*\\) " pattern)
              (match-string 1 pattern)
            pattern))
         (lim helm-candidate-number-limit)
         (args (append (list "search" "-l")
                       (and nil lim (list "-n" (int-to-string lim)))
                       (list short-pattern))))
    (let (candidates)
      (with-temp-buffer
        (apply #'call-process "hoogle" nil t nil args)
        (goto-char (point-min))
        (while (not (eobp))
          (if (looking-at "\\(.+?\\) -- \\(.+\\)")
              (push (cons (match-string 1)
                          (match-string-no-properties 2))
                    candidates))
          (forward-line 1)))
      (nreverse candidates))))

###autoload
(defun helm-hoogle ()
  (interactive)
  (helm :sources 'helm-c-source-hoogle
        :input ""
        :prompt "Hoogle: "
        :buffer "*Hoogle search*"))

(provide 'helm-hoogle)

helm-hoogle.el ends here
