#lang racket
(require racket/include)
(include "scheme-to-c-lib.scm")

(main (map string->symbol (vector->list (current-command-line-arguments))))