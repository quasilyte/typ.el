;;; -*- lexical-binding: t -*-

(require 'ert)
(require 'seq)
(require 'typ)

(typ--default-annotations-load)

(ert-deftest typ-noreturn ()
  (dolist (f '(throw error signal user-error))
    (should (typ-noreturn? f))))

(ert-deftest typ-combine ()
  (pcase-dolist
      (`[,t1 ,t2 -> ,want]
       '(;;; Numeric types combinations.
         [:number :number -> :number]
         [:number :integer -> :number]
         [:number :float -> :number]
         [:number :nil -> nil]
         [:integer :integer -> :integer]
         [:integer :float -> :number]
         [:integer :nil -> nil]
         [:float :float -> :float]
         [:float :nil -> nil]
         ;;; Parametric types combinations.
         ;; Single-level combinations.
         [(:sequence . :integer) (:sequence . :float) -> (:sequence . :number)]
         [(:sequence . :integer) (:list . :float) -> (:sequence . :number)]
         [(:sequence . :integer) (:array . :float) -> (:sequence . :number)]
         [(:sequence . :boolean) :bool-vector -> (:sequence . :boolean)]
         [(:sequence . :integer) :string -> (:sequence . :integer)]
         [(:sequence . :integer) (:vector . :float) -> (:sequence . :number)]
         [(:list . :integer) (:list . :float) -> (:list . :number)]
         [(:list . :integer) (:array . :float) -> (:sequence . :number)]
         [(:list . :boolean) :bool-vector -> (:sequence . :boolean)]
         [(:list . :integer) :string -> (:sequence . :integer)]
         [(:list . :integer) (:vector . :float) -> (:sequence . :number)]
         [(:array . :integer) (:array . :float) -> (:array . :number)]
         [(:array . :boolean) :bool-vector -> (:array . :boolean)]
         [(:array . :integer) :string -> (:array . :integer)]
         [(:array . :integer) (:vector . :float) -> (:array . :number)]
         [(:vector . :integer) (:vector . :float) -> (:vector . :number)]
         ;; Multi-level combinations.
         [(:list :list . :integer) (:list :list . :float) -> (:list :list . :number)]
         [(:sequence :list . :nil) (:array :list . :nil) -> (:sequence :list . :nil)]
         [(:sequence :vector . :nil) (:list :list . :nil) -> (:sequence :sequence . :nil)]
         [(:list . :string) (:list :array . :integer) -> (:list :array . :integer)]
         [(:list :list . :string) (:vector :list :sequence . :integer) -> (:sequence :list :sequence . :integer)]
         [(:list :list . :bool-vector) (:vector :list :array . :boolean) -> (:sequence :list :array . :boolean)]
         [(:list :list . :string) (:vector :vector . :string) -> (:sequence :sequence . :string)]
         [(:vector :vector :vector . :float) (:array :array :array . :integer) -> (:array :array :array . :number)]
         ;; Special cases.
         [:string :bool-vector -> (:array . nil)]
         [(:vector . :float) :bool-vector -> (:array . nil)]
         [(:vector . :nil) :bool-vector -> (:array . nil)]
         [(:vector . :float) :string -> (:array . :number)]
         [(:vector . :string) :string -> (:array . nil)]
         ;; Nil propagation.
         [(:sequence . :integer) (:list . nil) -> (:sequence . nil)]
         [(:list . :integer) (:list . nil) -> (:list . nil)]
         [(:list :list . :integer) (:list :list . nil) -> (:list :list . nil)]
         ))
    ;; Perform merge in both directions.
    ;; `combine' operation must be commutative.
    (should (and (equal want
                        (typ-combine t1 t2))
                 (equal want
                        (typ-combine t2 t1))))))

(ert-deftest typ-infer-lit ()
  (pcase-dolist
      (`[,lit ,want]
       '([10 :integer]
         [10.0 :float]
         ["" :string]
         [t :boolean]
         [sym :symbol]
         [nil :nil]
         [() :nil]))
    (should (eq want
                (typ-infer lit)))))

(ert-deftest typ-simple-predicates ()
  (pcase-dolist
      (`[,pred ,arg ,want]
       '([typ-integer? 10 t]
         [typ-integer? nil nil]
         [typ-float? 10.0 t]
         [typ-float? nil nil]
         [typ-string? "" t]
         [typ-string? nil nil]
         [typ-hash-table? #s(hash-table size 1 data nil) t]
         [typ-hash-table? nil nil]
         [typ-boolean? t t]
         [typ-boolean? nil nil]
         [typ-symbol? sym t]
         [typ-symbol? nil nil]))
    (should (eq want
                (funcall pred arg t)))))

(ert-deftest typ-abstract-predicates ()
  (pcase-dolist
      (`[,pred ,arg ,want]
       '([typ-number? 1 t]
         [typ-number? 1.0 t]
         [typ-number? nil nil]
         [typ-sequence? "1" t]
         [typ-sequence? (1) t]
         [typ-sequence? [1] t]
         [typ-sequence? 1 nil]
         [typ-sequence? nil nil]
         [typ-array? "1" t]
         [typ-array? [1] t]
         [typ-array? (1) nil]
         [typ-array? 1 nil]
         [typ-array? nil nil]))
    (should (eq want
                (not (not (funcall pred arg t)))))))
