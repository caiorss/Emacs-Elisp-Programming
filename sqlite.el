;;
;; Based on:
;; https://www.masteringemacs.org/article/comint-writing-command-interpreter
;;
;;

(defvar sqlite3-cli-file-path "/usr/bin/sqlite3"
  "Path to the program used by `run-sqlite3'")

(defvar sqlite3-cli-arguments '()
  "Commandline arguments to pass to `sqlite3-cli'")

(defvar sqlite3-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    ;; example definition
    (define-key map "\t" 'completion-at-point)
    map)
  "Basic mode map for `run-sqlite3'")

;; (defvar sqlite3-prompt-regexp "^\\(?:\\[[^@]+@[^@]+\\]\\)"
;;   "Prompt for `run-sqlite3'.")

(defvar sqlite3-prompt-regexp "sqlite3>"
  "Prompt for `run-sqlite3'.")

(defun run-sqlite3 ()
  "Run an inferior instance of `sqlite3-cli' inside Emacs."
  (interactive)
  (let* ((sqlite3-program sqlite3-cli-file-path)
         (buffer (comint-check-proc "Sqlite3")))
    ;; pop to the "*Sqlite3*" buffer if the process is dead, the
    ;; buffer is missing or it's got the wrong mode.
    (pop-to-buffer-same-window
     (if (or buffer (not (derived-mode-p 'sqlite3-mode))
             (comint-check-proc (current-buffer)))
         (get-buffer-create (or buffer "*Sqlite3*"))
       (current-buffer)))
    ;; create the comint process if there is no buffer.
    (unless buffer
      (apply 'make-comint-in-buffer "Sqlite3" buffer
             sqlite3-program sqlite3-cli-arguments)
      (sqlite3-mode))))

(defun sqlite3--initialize ()
  "Helper function to initialize Sqlite3"
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp t))

(define-derived-mode sqlite3-mode comint-mode "Sqlite3"
  "Major mode for `run-sqlite3'.

\\<sqlite3-mode-map>"
  nil "Sqlite3"
  ;; this sets up the prompt so it matches things like: [foo@bar]
  (setq comint-prompt-regexp sqlite3-prompt-regexp)
  ;; this makes it read only; a contentious subject as some prefer the
  ;; buffer to be overwritable.
  (setq comint-prompt-read-only t)
  ;; this makes it so commands like M-{ and M-} work.
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'font-lock-defaults) '(sqlite3-font-lock-keywords t))
  (set (make-local-variable 'paragraph-start) sqlite3-prompt-regexp))


;; this has to be done in a hook. grumble grumble.
(add-hook 'sqlite3-mode-hook 'sqlite3--initialize)




(defconst sqlite3-keywords
  '("abort" "action" "add" "after" "all" "alter" "analyze"
    "and" "as"  "asc" "attach" "autoincrement" "before" "begin"
    "between" "by"  "cascade" "case" "cast" "check" "collate"
    "column" "commit"  "conflict" "constraint" "create" "cross"
    "current_date"  "current_time" "current_timestamp" "database"
    "default" "deferrable"  "deferred" "delete" "desc" "detach"
    "distinct" "drop" "each" "else"  "end" "escape" "except"
    "exclusive" "exists" "explain" "fail" "for"  "foreign" "from"
    "full" "glob" "group" "having" "if" "ignore"  "immediate"
    "in" "index" "indexed" "initially" "inner" "insert"  "instead"
    "intersect" "into" "is" "isnull" "join" "key" "left" "like"
 "limit" "match" "natural" "no" "not" "notnull" "null" "of" "offset"
 "on" "or" "order" "outer" "plan" "pragma" "primary" "query" "raise"
 "recursive" "references" "regexp" "reindex" "release" "rename"
 "replace" "restrict" "right" "rollback" "row" "savepoint" "select"
 "set" "table" "temp" "temporary" "then" "to" "transaction" "trigger"
 "union" "unique" "update" "using" "vacuum" "values" "view" "virtual"
 "when" "where" "with" "without"))


(defvar sqlite3-font-lock-keywords
  (list
   ;; highlight all the reserved commands.
   `(,(concat "\\_<" (regexp-opt sqlite3-keywords) "\\_>") . font-lock-keyword-face))
  "Additional expressions to highlight in `sqlite3-mode'.")
