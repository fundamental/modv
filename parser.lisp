(ql:quickload :graylex)
(defpackage #:modv-compiler
  (:use #:cl #:graylex))
(in-package #:modv-compiler)

(defun handle-tok (token)
  (if (and (car token) (not (eq (car token) :whitespace)))
    (print token))
  (car token))

(defparameter *lexer* 
  (with-open-file (stream "test.mod")
    (make-instance 'lexer-input-stream
                   :stream stream
                   :rules '(("module"   . :module)
                            ("end"      . :end)
                            ("out"      . :out)
                            ("in"       . :in)
                            ("[a-z]*"   . :symbol)
                            (":" . #\:)
                            ("[ \\n]" . :whitespace)))))


(loop while (handle-tok (multiple-value-list (stream-read-token *lexer*))))
