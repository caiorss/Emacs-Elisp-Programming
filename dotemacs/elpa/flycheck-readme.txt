On-the-fly syntax checking for GNU Emacs 24.

This package provides on-the-fly syntax checking for GNU Emacs 24.  It is a
replacement for the older Flymake extension which is part of GNU Emacs, with
many improvements and additional features.

Flycheck provides fully-automatic, fail-safe, on-the-fly background syntax
checking for over 30 programming and markup languages with more than 70
different tools.  It highlights errors and warnings inline in the buffer, and
provides an optional IDE-like error list.

It comes with a rich interface for custom syntax checkers and other
extensions, and has already many 3rd party extensions adding new features.

# Setup

Flycheck works best on Unix systems.  It does not officially support Windows,
but tries to maintain Windows compatibility and should generally work fine on
Windows, too.

To enable Flycheck add the following to your init file:

   (add-hook 'after-init-hook #'global-flycheck-mode)

Flycheck will then automatically check buffers in supported languages, as
long as all necessary tools are present.

# Documentation

Flycheck comes with a rich manual, which you can read in Emacs with `M-x
flycheck-info' after installing this package.  It is also available online at
URL `http://www.flycheck.org/manual/latest/index.html'.

The manual has a Quickstart section which gives you a short and comprehensive
introduction into Flycheck's features and usage, and a complete list of all
supported languages and tools.
