(in-package #:modv-compiler)

(defun handle-tok (token)
  (if (and (car token) (not (eq (car token) :whitespace)))
    (print token))
  (car token))

(defparameter *lexer* 
  (let ((stream (open "test.mod")))
    (make-instance 'lexer-input-stream
                   :stream stream
                   :rules '(("module"   . module)
                            ("arch"     . arch)
                            ("signal"   . signal)
                            ("head"     . head)
                            ("end"      . end)
                            ("out"      . out)
                            ("in"       . in)
                            ("sync"     . synchronous)
                            ("="        . =)
                            (":"        . :)
                            ("[a-z]*"   . symbol)
                            ("\\)"        . close)
                            ("\\("        . open)
                            ("[ \\n]"   . :whitespace)))))

(defun next-tok ()
  (let ((value (multiple-value-list (stream-read-token *lexer*))))
    (if (eq :whitespace (car value))
      (next-tok)
      (apply #'values value))))
