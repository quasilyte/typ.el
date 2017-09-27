;;; typ.el --- type inference framework for Emacs Lisp compilers -*- lexical-binding: t -*-

;; Author: Iskander Sharipov <quasilyte@gmail.com>
;; Keywords: lisp

;;; Commentary:

;; "typ.el" defines two types of functions:
;; (a) functions that record type information and
;; (b) functions that answer type information queries.
;;
;; The simplest use cases for (a):
;; - Optimizer/Compiler annotates some builtin functions.
;; - Programmer annotates his own functions for better documentation
;;   and optimization possibilities.
;;
;; Some use cases for (b):
;; - Optimizer gets some type information that may enable
;;   better optimizations.
;; - Linter can warn about invalid/unsafe functions usage.
;;
;; To get detailed documentation, visit dev repo:
;; https://github.com/Quasilyte/typ.el

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(defvar typ--funcs-db (make-hash-table :test 'eq)
  "Maps function name (symbol) to it's return value type.

There are two possible value kinds:
    - callable objects (lambdas, closures, ...)
    - keywords
If key contains callable value, it is called with
unevaluated function arguments.")

(defvar typ--noreturn-funcs '(throw
                              error
                              signal)
  "List of functions that never return normally.
This list may be extended by users, but great caution is required.")

(defsubst typ-noreturn? (fn)
  "Return non-nil if FN is marked with `noreturn'."
  (memq fn typ--noreturn-funcs))

(defsubst typ-any-numerical? (expr)
  "Return non-nol if (typ-infer EXPR) returned any of [:integer :float :number]."
  (memq (typ-infer expr) '(:float :number :integer)))
(defsubst typ-integer? (expr)
  "Return non-nil if (typ-infer EXPR) returned `:integer'."
  (eq :integer (typ-infer expr)))
(defsubst typ-float? (expr)
  "Return non-nil if (typ-infer EXPR) returned `:float'."
  (eq :float (typ-infer expr)))
(defsubst typ-number? (expr)
  "Return non-nil if (typ-infer EXPR) returned `:number'."
  (eq :number (typ-infer expr)))
(defsubst typ-string? (expr)
  "Return non-nil if (typ-infer EXPR) returned `:string'."
  (eq :string (typ-infer expr)))
(defsubst typ-vector? (expr)
  "Return non-nil if (typ-infer EXPR) returned `:vector'."
  (eq :vector (typ-infer expr)))
(defsubst typ-list? (expr)
  "Return non-nil if (typ-infer EXPR) returned `:list'."
  (eq :list (typ-infer expr)))
(defsubst typ-symbol? (expr)
  "Return non-nil if (typ-infer EXPR) returned `:symbol'."
  (eq :symbol (typ-infer expr)))
(defsubst typ-boolean? (expr)
  "Return non-nil if (typ-infer EXPR) returned `:boolean'."
  (eq :boolean (typ-infer expr)))

(defun typ-infer-call (call-expr)
  "Return result type of CALL-EXPR invocation or nil, if inference fail."
  (let* ((fn (car call-expr))
         (info (gethash fn typ--funcs-db)))
    (if (keywordp info)
        info
      (let ((args (cdr call-expr)))
        (funcall info args)))))

(defun typ-infer (expr)
  "Return result type of EXPR evaluation or nil, if inference fail."
  (cond ((listp expr)
         (typ-infer-call expr))
        ((numberp expr)
         (if (integerp expr)
             :integer
           :float))
        ((arrayp expr)
         (if (stringp expr)
             :string
           :vector))
        ((eq t expr) :boolean)
        ((symbolp expr) :symbol)
        (t nil)))

(defun typ--blame-typ (typ)
  (error "TYP must be a symbol, %S (%s) given" typ (type-of typ)))
(defun typ--blame-fn (fn)
  (error "FN must be a symbol, %S (%s) given" fn (type-of fn)))
(defun typ--blame-handler (handler)
  (error "HANDLER must be a function, %S (%s) given" handler (type-of handler)))

(defun typ-annotate (fn typ)
  "Set FN function return value type to TYP.
Only applicable for functions that return specific type
in all cases, with any arguments.
Used in `typ-infer-call' to discover FN return type."
  (unless (symbolp fn)
    (typ--blame-fn fn))
  (unless (symbolp typ)
    (typ--blame-typ typ))
  (puthash fn typ typ--funcs-db))

(defun typ-annotate-mixed (fn handler)
  "Register FN function type inference callback HANDLER.
When `typ-infer-call' is called for list that head is `eq'
to FN, HANDLER is called with that list `cdr'.
In other words, HANDLER is called with FN invocation arguments.
HANDLER must return a type based on the input arguments.

If it is impossible to give precise type information,
handler *must* return nil."
  (unless (symbolp fn)
    (typ--blame-fn fn))
  (unless (functionp handler)
    (typ--blame-handler handler))
  (puthash fn handler typ--funcs-db))

(defun typ-mark-noreturn (fn)
  "If you do not know what `noreturn' means, leave this function alone!

Mark function FN as non-returning.

If function actually does return, something really bad may happen.
If in doubdt, do not use this."
  (unless (symbolp fn)
    (typ--blame-fn fn))
  (unless (memq fn typ--noreturn-funcs)
    (push fn typ--noreturn-funcs)))

(defun typ-default-annotations-loaded-p ()
  "Return non-nil if default annotations are loaded."
  (not (not (gethash '+ typ--funcs-db))))

(defun typ-default-annotations-load ()
  "Ensure that type information for builtin functions is set.
When `typ-default-annotations-loaded-p' returns non-nil, does nothing."
  (unless (typ-default-annotations-loaded-p)
    (typ--default-annotations-load)))

(defun typ--default-annotations-load ()
  "Unconditionally load default annotations."
  ;; NOTE: if you add additional definitions here,
  ;;       please do not forget to add tests for it.
  ;;
  ;; Special case for nil.
  (typ-annotate nil :list)
  ;; Fill `integer' functions.
  (dolist (fn '(lsh
                char-syntax
                point
                length))
    (typ-annotate fn :integer))
  ;; Fill `float' functions.
  (dolist (fn '(float))
    (typ-annotate fn :float))
  ;; Fill `number' functions.
  (dolist (fn '(string-to-number))
    (typ-annotate fn :number))
  ;; Fill `string' functions.
  (dolist (fn '(int-to-string
                number-to-string
                char-to-string
                concat
                symbol-name))
    (typ-annotate fn :string))
  ;; Fill `vector' functions.
  (dolist (fn '(vector
                vconcat))
    (typ-annotate fn :vector))
  ;; Fill `list' functions.
  (dolist (fn '(cons
                list))
    (typ-annotate fn :list))
  ;; Fill `symbol' functions.
  (dolist (fn '(intern))
    (typ-annotate fn :symbol))
  ;; Fill `boolean' functions.
  (dolist (fn '(not
                symbolp
                stringp
                consp
                listp))
    (typ-annotate fn :boolean))
  ;; Fill mixed type functions.
  (cl-flet
      ;; Common function handler defined here.
      ((num-arith
        (args)
        ;; - `float' if at least 1 arg is float
        ;; - `integer' if all arguments are integers
        ;; - `number' otherwise
        (let ((found-float nil)
              (all-ints t))
          (while (and args
                      (not found-float))
            (let ((type (typ-infer (car args))))
              (if (eq :float type)
                  (setq found-float t)
                (unless (eq :integer type)
                  (setq all-ints nil))
                (setq args (cdr args)))))
          (cond
           (found-float :float)
           (all-ints :integer)
           (t :number))))
       )
    ;; `quote' returns `list' for quoted list
    ;; instead of going through `typ-infer' which will
    ;; lead to `typ-infer-call' and incorrect results.
    (typ-annotate-mixed
     #'quote
     (lambda (args)
       (let ((arg (car args)))
         (if (listp arg)
             :list
           (typ-infer arg)))))
    ;; Arith functions.
    (typ-annotate-mixed #'+ #'num-arith)
    (typ-annotate-mixed #'- #'num-arith)
    (typ-annotate-mixed #'* #'num-arith)
    (typ-annotate-mixed #'/ #'num-arith)))

(provide 'typ)

;;; typ.el ends here
