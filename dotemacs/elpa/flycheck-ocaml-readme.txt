This Flycheck extension provides a new `ocaml-merlin' syntax checker which
uses Merlin Mode (see URL `https://github.com/the-lambda-church/merlin') to
check OCaml buffers for errors.

# Setup

Add the following to your init file:

   (with-eval-after-load 'merlin
     ;; Disable Merlin's own error checking
     (setq merlin-error-after-save nil)

     ;; Enable Flycheck checker
     (flycheck-ocaml-setup))

   (add-hook 'tuareg-mode-hook #'merlin-mode)

# Usage

Just use Flycheck as usual in Tuareg Mode buffers.  Flycheck will
automatically use the new `ocaml-merlin` syntax checker if Merlin Mode is
enabled and Merlin's own error checking (`merlin-error-after-save`) is
disabled.

If you enable Merlin's error checking with `M-x merlin-toggle-view-errors`
Flycheck will not use the `ocaml-merlin` syntax checker anymore, to avoid
duplicate and redundant error reporting.
