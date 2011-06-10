(require :asdf)
(require :graylex)
(require :yacc)
(defpackage #:modv-compiler
  (:use #:cl #:yacc #:graylex))
(in-package #:modv-compiler)

(load "lex.lisp")
(load "parse.lisp")

(defun main (argv)
  (handler-case (parse-with-lexer #'next-tok *parser*)
    (unmatched-lexing-sequence (e)
                               (inspect e)))

  (print-vhdl))
