  Basic steps to setup:

   (add-to-list 'load-path
                "~/path-to-yasnippet")
   (require 'yasnippet)
   (yas-global-mode 1)


  Interesting variables are:

      `yas-snippet-dirs'

          The directory where user-created snippets are to be
          stored.  Can also be a list of directories.  In that case,
          when used for bulk (re)loading of snippets (at startup or
          via `yas-reload-all'), directories appearing earlier in
          the list override other dir's snippets.  Also, the first
          directory is taken as the default for storing the user's
          new snippets.

          The deprecated `yas/root-directory' aliases this variable
          for backward-compatibility.


  Major commands are:

      M-x yas-expand

          Try to expand snippets before point.  In `yas-minor-mode',
          this is normally bound to TAB, but you can customize it in
          `yas-minor-mode-map'.

      M-x yas-load-directory

          Prompts you for a directory hierarchy of snippets to load.

      M-x yas-activate-extra-mode

          Prompts you for an extra mode to add snippets for in the
          current buffer.

      M-x yas-insert-snippet

          Prompts you for possible snippet expansion if that is
          possible according to buffer-local and snippet-local
          expansion conditions.  With prefix argument, ignore these
          conditions.

      M-x yas-visit-snippet-file

          Prompts you for possible snippet expansions like
          `yas-insert-snippet', but instead of expanding it, takes
          you directly to the snippet definition's file, if it
          exists.

      M-x yas-new-snippet

          Lets you create a new snippet file in the correct
          subdirectory of `yas-snippet-dirs', according to the
          active major mode.

      M-x yas-load-snippet-buffer

          When editing a snippet, this loads the snippet.  This is
          bound to "C-c C-c" while in the `snippet-mode' editing
          mode.

      M-x yas-tryout-snippet

          When editing a snippet, this opens a new empty buffer,
          sets it to the appropriate major mode and inserts the
          snippet there, so you can see what it looks like.  This is
          bound to "C-c C-t" while in `snippet-mode'.

      M-x yas-describe-tables

          Lists known snippets in a separate buffer.  User is
          prompted as to whether only the currently active tables
          are to be displayed, or all the tables for all major
          modes.

  If you have `dropdown-list' installed, you can optionally use it
  as the preferred "prompting method", putting in your .emacs file,
  for example:

      (require 'dropdown-list)
      (setq yas-prompt-functions '(yas-dropdown-prompt
                                   yas-ido-prompt
                                   yas-completing-prompt))

  Also check out the customization group

       M-x customize-group RET yasnippet RET

  If you use the customization group to set variables
  `yas-snippet-dirs' or `yas-global-mode', make sure the path to
  "yasnippet.el" is present in the `load-path' *before* the
  `custom-set-variables' is executed in your .emacs file.

  For more information and detailed usage, refer to the project page:
     http://github.com/capitaomorte/yasnippet
