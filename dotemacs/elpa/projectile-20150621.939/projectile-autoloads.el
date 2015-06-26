;;; projectile-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "projectile" "projectile.el" (21895 48439 573171
;;;;;;  710000))
;;; Generated autoloads from projectile.el

(autoload 'projectile-cache-current-file "projectile" "\
Add the currently visited file to the cache.

\(fn)" t nil)

(autoload 'projectile-switch-to-buffer "projectile" "\
Switch to a project buffer.

\(fn)" t nil)

(autoload 'projectile-switch-to-buffer-other-window "projectile" "\
Switch to a project buffer and show it in another window.

\(fn)" t nil)

(autoload 'projectile-display-buffer "projectile" "\
Display a project buffer in another window without selecting it.

\(fn)" t nil)

(autoload 'projectile-project-buffers-other-buffer "projectile" "\
Switch to the most recently selected buffer project buffer.
Only buffers not visible in windows are returned.

\(fn)" t nil)

(autoload 'projectile-multi-occur "projectile" "\
Do a `multi-occur' in the project's buffers.

\(fn)" t nil)

(autoload 'projectile-find-file-dwim "projectile" "\
Jump to a project's files using completion based on context.

With a prefix ARG invalidates the cache first.

If point is on a filename, Projectile first tries to search for that
file in project:

- If it finds just a file, it switches to that file instantly.  This works even
if the filename is incomplete, but there's only a single file in the current project
that matches the filename at point.  For example, if there's only a single file named
\"projectile/projectile.el\" but the current filename is \"projectile/proj\" (incomplete),
`projectile-find-file' still switches to \"projectile/projectile.el\" immediately
 because this is the only filename that matches.

- If it finds a list of files, the list is displayed for selecting.  A list of
files is displayed when a filename appears more than one in the project or the
filename at point is a prefix of more than two files in a project.  For example,
if `projectile-find-file' is executed on a filepath like \"projectile/\", it lists
the content of that directory.  If it is executed on a partial filename like
 \"projectile/a\", a list of files with character 'a' in that directory is presented.

- If it finds nothing, display a list of all files in project for selecting.

\(fn &optional ARG)" t nil)

(autoload 'projectile-find-file-dwim-other-window "projectile" "\
Jump to a project's files using completion based on context in other window.

With a prefix ARG invalidates the cache first.

If point is on a filename, Projectile first tries to search for that
file in project:

- If it finds just a file, it switches to that file instantly.  This works even
if the filename is incomplete, but there's only a single file in the current project
that matches the filename at point.  For example, if there's only a single file named
\"projectile/projectile.el\" but the current filename is \"projectile/proj\" (incomplete),
`projectile-find-file' still switches to \"projectile/projectile.el\"
immediately because this is the only filename that matches.

- If it finds a list of files, the list is displayed for selecting.  A list of
files is displayed when a filename appears more than one in the project or the
filename at point is a prefix of more than two files in a project.  For example,
if `projectile-find-file' is executed on a filepath like \"projectile/\", it lists
the content of that directory.  If it is executed on a partial filename
like \"projectile/a\", a list of files with character 'a' in that directory
is presented.

- If it finds nothing, display a list of all files in project for selecting.

\(fn &optional ARG)" t nil)

(autoload 'projectile-find-file "projectile" "\
Jump to a project's file using completion.
With a prefix ARG invalidates the cache first.

\(fn &optional ARG)" t nil)

(autoload 'projectile-find-file-other-window "projectile" "\
Jump to a project's file using completion and show it in another window.

With a prefix ARG invalidates the cache first.

\(fn &optional ARG)" t nil)

(autoload 'projectile-find-implementation-or-test-other-window "projectile" "\
Open matching implementation or test file in other window.

\(fn)" t nil)

(autoload 'projectile-toggle-between-implementation-and-test "projectile" "\
Toggle between an implementation file and its test file.

\(fn)" t nil)

(autoload 'projectile-regenerate-tags "projectile" "\
Regenerate the project's [e|g]tags.

\(fn)" t nil)

(autoload 'projectile-find-tag "projectile" "\
Find tag in project.

\(fn)" t nil)

(autoload 'projectile-run-command-in-root "projectile" "\
Invoke `execute-extended-command' in the project's root.

\(fn)" t nil)

(autoload 'projectile-run-shell-command-in-root "projectile" "\
Invoke `shell-command' in the project's root.

\(fn)" t nil)

(autoload 'projectile-run-async-shell-command-in-root "projectile" "\
Invoke `async-shell-command' in the project's root.

\(fn)" t nil)

(autoload 'projectile-kill-buffers "projectile" "\
Kill all project buffers.

\(fn)" t nil)

(autoload 'projectile-save-project-buffers "projectile" "\
Save all project buffers.

\(fn)" t nil)

(autoload 'projectile-dired "projectile" "\
Open `dired' at the root of the project.

\(fn)" t nil)

(autoload 'projectile-vc "projectile" "\
Open `vc-dir' at the root of the project.

For git projects `magit-status-internal' is used if available.

\(fn &optional PROJECT-ROOT)" t nil)

(autoload 'projectile-recentf "projectile" "\
Show a list of recently visited files in a project.

\(fn)" t nil)

(autoload 'projectile-find-file-in-known-projects "projectile" "\
Jump to a file in any of the known projects.

\(fn)" t nil)

(autoload 'projectile-cleanup-known-projects "projectile" "\
Remove known projects that don't exist anymore.

\(fn)" t nil)

(autoload 'projectile-clear-known-projects "projectile" "\
Clear both `projectile-known-projects' and `projectile-known-projects-file'.

\(fn)" t nil)

(autoload 'projectile-remove-current-project-from-known-projects "projectile" "\
Remove the current project from the list of known projects.

\(fn)" t nil)

(autoload 'projectile-commander "projectile" "\
Execute a Projectile command with a single letter.
The user is prompted for a single character indicating the action to invoke.
The `?' character describes then
available actions.

See `def-projectile-commander-method' for defining new methods.

\(fn)" t nil)

(defvar projectile-mode-line '(:eval (format " Projectile[%s]" (projectile-project-name))) "\
Mode line lighter for Projectile.

The value of this variable is a mode line template as in
`mode-line-format'.  See Info Node `(elisp)Mode Line Format' for
details about mode line templates.

Customize this variable to change how Projectile displays its
status in the mode line.  The default value displays the project
name.  Set this variable to nil to disable the mode line
entirely.")

(custom-autoload 'projectile-mode-line "projectile" t)

(autoload 'projectile-mode "projectile" "\
Minor mode to assist project management and navigation.

When called interactively, toggle `projectile-mode'.  With prefix
ARG, enable `projectile-mode' if ARG is positive, otherwise disable
it.

When called from Lisp, enable `projectile-mode' if ARG is omitted,
nil or positive.  If ARG is `toggle', toggle `projectile-mode'.
Otherwise behave as if called interactively.

\\{projectile-mode-map}

\(fn &optional ARG)" t nil)

(defvar projectile-global-mode nil "\
Non-nil if Projectile-Global mode is enabled.
See the command `projectile-global-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `projectile-global-mode'.")

(custom-autoload 'projectile-global-mode "projectile" nil)

(autoload 'projectile-global-mode "projectile" "\
Toggle Projectile mode in all buffers.
With prefix ARG, enable Projectile-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Projectile mode is enabled in all buffers where
`projectile-mode' would do it.
See `projectile-mode' for more information on Projectile mode.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; projectile-autoloads.el ends here
