(ql:quickload :graylex)
(ql:quickload :yacc)
(defpackage #:modv-compiler
  (:use #:cl #:yacc #:graylex))
(in-package #:modv-compiler)

(load :lex)
(load :parse)

(handler-case (parse-with-lexer #'next-tok *parser*)
  (unmatched-lexing-sequence (e)
                             (inspect e)))

(print-vhdl)
