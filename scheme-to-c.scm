(load "scheme-to-c-lib.scm")
(main (map string->symbol (cdr (command-line))))
