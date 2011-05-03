(ql:quickload :graylex)
(ql:quickload :yacc)
(defpackage #:modv-compiler
  (:use #:cl #:yacc #:graylex))
(in-package #:modv-compiler)

(load :lex)
(load :parse)

(parse-with-lexer #'next-tok *parser*)

(print-vhdl)
