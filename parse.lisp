(in-package #:modv-compiler)

(defvar *sync*           nil)
(defvar *relations*      nil)
(defvar *sync-relations* nil)
(defvar *mod-name*       nil)
(defvar *ports*          nil)
(defvar *signals*        nil)

(defun add-port (name dir type)
  (push (format nil "~%~A :~A ~A" name dir type) *ports*))

(defun add-sig (x name type)
  (push (format nil "signal ~a: ~a" name type) *signals*))

(defun add-relation (lvalue x rvalue)
  (let ((rel (format nil "~a <= ~a;" lvalue rvalue)))
    (if *sync*
      (push rel *sync-relations*)
      (push rel *relations*))))

(defun start-sync (x x name x)
  (push (format nil "process(all)~%begin~%if(rising_edge(~A)) then" name) *relations*))

(defun end-sync (&rest rest)
  (push (format nil "end if;~%end process;") *relations*))

(defun type-print (type x lim x)
  (format nil "~a(~a downto 0)" type (1- (parse-integer lim))))

(defun hex-to-bin (hex)
  (format nil "X\"~X\"" (parse-integer (subseq hex 2) :radix 16)))

(defun with-select (main selects x)
  (push (format nil "~a ~%~{~a~^,~%~};" main selects) *relations*))

(defun with-select-srt (result x x x input x x)
  (format nil "with ~A select ~A <=" input result))

(defun with-select-item (value x result)
  (list (format nil "~A when ~A" result value)))


(define-parser *parser*
  (:start-symbol module-file)
  (:terminals (module end out in : symbol end head arch signal = synchronous open close [ ] int hex lookup default))
  (module-file (module-declare head : ports end arch : statements end))
  (statements (statements statement)
              statement)
  (statement signal-declare
             sync-block
             lookup-block
             relation)
  (signal-declare (signal symbol type #'add-sig))
  (type (symbol [ int ] #'type-print)
        (symbol #'identity))
  (module-declare (module symbol #'(lambda (x name) (setf *mod-name* name) (format t "~%entity ~A" name))))
  (direction in out)

  ;synchronous statements
  (sync (synchronous open symbol close #'start-sync))
  (sync-block (sync relation #'end-sync))
  ;lookup statements
  (lookup-start (symbol = lookup open symbol close : #'with-select-srt))
  (lookup-block (lookup-start lookup-entries end #'with-select))
  (lookup-entry (rvalue : rvalue #'with-select-item)
                (default : rvalue))
  (lookup-entries lookup-entry
                  (lookup-entries lookup-entry #'append))
  (port (symbol direction type #'add-port))
  (relation (symbol = rvalue #'add-relation))
  (rvalue (hex #'hex-to-bin) symbol int)
  (ports (port) (ports port)))

(defun print-vhdl ()
  (format t
"
library ieee;
use ieee.stuff;
use work.all;

entity ~A is
port
(~{~a~^; ~}
);
end ~A;

architecture default of ~A
~{~a;~}
begin

~{~a~%~}
end architecture;" *mod-name* *ports* *mod-name* *mod-name* *signals* (reverse *relations*)))
