;;; -*- lexical-binding: t -*-

(require 'ert)
(require 'seq)
(require 'typ)

(typ--default-annotations-load)

(defmacro should-eq (x y)
  `(should (eq ,x ,y)))

(defmacro should-eq-all (&rest shoulds)
  (setq shoulds (mapcar (lambda (x)
                          (cons 'should-eq x))
                        (seq-partition shoulds 2)))
  `(progn ,@shoulds))

(defmacro should-all (&rest shoulds)
  (setq shoulds (mapcar (lambda (x)
                          `(should ,x))
                        shoulds))
  `(progn ,@shoulds))

(defmacro should-not-all (&rest shoulds)
  (setq shoulds (mapcar (lambda (x)
                          `(should-not ,x))
                        shoulds))
  `(progn ,@shoulds))

(defmacro typ--test/typ-infer (value &rest shoulds)
  (declare (indent defun))
  (setq shoulds (mapcar (lambda (x)
                          `(should-eq ,value (typ-infer ',x)))
                        shoulds))
  `(progn ,@shoulds))

(ert-deftest typ-infer-const ()
  (should-eq-all
   :integer (typ-infer 10)
   :float (typ-infer 10.0)
   :string (typ-infer "")
   :vector (typ-infer [])
   :boolean (typ-infer t)
   :symbol (typ-infer 'sym)
   :list (typ-infer nil)
   ))

(ert-deftest typ-noreturn ()
  (should-all
   (typ-noreturn? 'throw)
   (typ-noreturn? 'error)
   (typ-noreturn? 'signal)
   )
  (should-not-all
   (typ-noreturn? 'foo)
   ))

(ert-deftest typ-predicates ()
  (should-all
   (typ-integer? 10)
   (typ-float? 10.0)
   (typ-string? "")
   (typ-vector? [])
   (typ-boolean? t)
   (typ-symbol? 'sym)
   (typ-list? nil)
   ))

(ert-deftest typ-infer-integer ()
  (typ--test/typ-infer :integer
    (lsh 10 x)
    (lsh (lsh 3 x) y)
    (char-syntax ?c)
    (char-syntax (char-syntax ?c))
    (point)
    (length [1])
    )
  (typ--test/typ-infer :integer
    (+ 1 1 1)
    (* 2 (* 2 2))
    ))

(ert-deftest typ-infer-float ()
  (typ--test/typ-infer :float
    (float 1)
    )
  (typ--test/typ-infer :float
    (+ 1.0 1)
    (+ 1 1.0)
    (+ 1.0 1.0)
    (+ x 1.0)
    (+ 1.0 x)
    ))

(ert-deftest typ-infer-number ()
  (typ--test/typ-infer :number
    (string-to-number "1")
    )
  (typ--test/typ-infer :number
    (+ x 1)
    (+ 1 x)
    (+ x x)
    ))

(ert-deftest typ-infer-string ()
  (typ--test/typ-infer :string
    (int-to-string 1)
    (number-to-string 1)
    (char-to-string ?c)
    (concat "a" "b")
    (symbol-name 'sym)
    ))

(ert-deftest typ-infer-vector ()
  (typ--test/typ-infer :vector
    (vector 1 2 3)
    (vconcat [1] [2])
    ))

(ert-deftest typ-infer-list ()
  (typ--test/typ-infer :list
    (cons 1 2)
    (list 1 2 3)
    ))

(ert-deftest typ-infer-symbol ()
  (typ--test/typ-infer :symbol
    (intern "sym")
    ))

(ert-deftest typ-infer-boolean ()
  (typ--test/typ-infer :boolean
    (not t)
    (symbolp 'sym)
    (stringp "")
    (consp (cons 1 2))
    (listp (list 1 2))))
