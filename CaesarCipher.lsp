#||
LISP Caesar Cipher
Teddy Williams
November 22, 2016
||#

#||
This function takes in the list
||#
(defun encode (expr)   ; Syntax: define function funcName (argument)
; Out case when the list is empty
(cond ((null expr) "Enter (exit) to leave clisp")   ; Syntax: conditional (test 1 action1 ) (test 2 action 2) ... (test n action n)
; Checking if the expression is an atom only then to go encryption

((atom expr)  (encRot expr))  ; test2 see if one or less atom
;   (cons (atom) (list)) Adding the encoded atom to the rest of list
;   (encode (first atom of list)) recursively called itself
(t(if (not nil) (append (encode(car expr)) (encode(cdr expr))))))) ; (cond (t1 a1) (t (...)) will run if all others fail


(defun encRot (expr)
; casts the object and then shifts the char by 5
; Printing result out as strings
(format t "~a "(string (int-char(enCheck (+ 5 (char-int(char (string expr) 0))))))))


(defun enCheck (x)
; Checking to see if the atom is a letter. Mods by 90 to shift index to prevent falling off the ascii table.
(cond (( > x 90) (+ 64 ( mod x 90 )))
(( < x 91) x)))

(defun decode (expr)
; Out case when the list is empty
(cond ((null expr) "Enter (exit) to leave clisp")
; Checking if the expression is an atom only then to go decryption
((atom expr)  (deRot expr))
;   (cons (atom) (list))) Adding the decoded atom to the rest of list
;   (decode (first atom of list)) recursively called itself
(t(append (decode (car expr)) (decode(cdr expr))))))

(defun deRot (expr)
; casts the object and then shifts the char back by 5
; Printing result out as strings
(format t "~a "(string(int-char(deCheck (+ -5 (char-int(char (string expr) 0))))))))

(defun deCheck (x)
; Checking to see if the atom is a letter.
(cond (( < x 65) (+ 26 x))
(( > x 64) x)))
