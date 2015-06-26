Colors mismatched open parentheses with fl-mismatched-face, red by
default. Works reliably after Emacs 24.3, in which bug 16247 is
fixed.

Also colors open and close parentheses which are inconsistent with
the indentation of lines between them with fl-inconsistent-face,
orange by default. This is useful for the Lisp programmer who
infers a close paren's location from the open paren and
indentation. The coloring serves as a warning that the indentation
misleads about where the close paren is. It may also help to
localize the mistake, whether due to a misindented line or a
misplaced paren.

As an example, consider:

  (aaa (bbb "word-a
  word-b" (ccc 1
               2)
       fff))

(aaa ...) and (ccc ...) are consistent, so are not colored.
(bbb ...) is inconsistent because the indentation of fff is
inconsistent with the actual location of the close paren. The open
and close paren are thus colored with the fl-inconsistent-face.
This example also shows that multi line strings don't cause an
inconsistency.

Currently, the package only detects close parens that are after the
place indentation would predict. A planned feature is to also
indicate when the close paren is before.

Also planned is to color mismatched close parens.
