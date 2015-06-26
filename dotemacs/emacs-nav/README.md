emacs-nav: simple file-system navigation
========================================

Nav is a lightweight solution for Emacs users who want something like
TextMate's file browser, or the Eclipse project view. Unlike these
two, Nav only shows the contents of a single directory at a time. Nav
can be run painlessly in terminals, where Speedbar either fails on its
attempt to make a new frame or is hidden. Nav's terminal-friendliness
comes from running in the frame where it was started, keeping window
management simple. The Nav key bindings are also simple -- each
key command is a single keystroke long.

# Install
Put something like this in your ~/.emacs:

(add-to-list 'load-path "/directory/containing/nav/")
(require 'nav)
(nav-disable-overeager-window-splitting)
;; Optional: set up a quick key to toggle nav
;; (global-set-key [f8] 'nav-toggle)

# Navigate!
Type M-x nav to start navigating.

In the nav window, hit ? to get help on keyboard shortcuts.

To set options for Nav, type M-x customize, then select Applications,
Nav.

