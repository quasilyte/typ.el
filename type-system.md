## Type system overview

This is a compact `typ.el` type system model overview.  
It is heavilly based on official ["Programming types"](https://www.gnu.org/software/emacs/manual/html_node/elisp/Programming-Types.html#Programming-Types) documentation.

### Kinds of types

**Simple type** - concrete value type (can also be viewed as "primitive type").  
Represented with `keyword`.

**Parametric type** - mostly related to sequence types.
Parametric types have primary type and a single parameter type.  
Represented with a `cons` with `car` set to primary type and `cdr`
set to parameter type.  
Example: `(list 1 2)` => `(cons :list :integer)`  
Example: `(list (list 1) (list 2))` => `(cons :list (cons :list :integer))`  

**Abstract type** - type that have no direct instances.  
Abstract type tells about object interface and never about it's concrete type.  
Represented with `keyword`.  
Example: `(string-to-number x)` => `:number`  
At the *run time*, type is either `:integer` or `:float`, but we can not
select appropriate type for that expression during *compile time*,
so `:number` is the closest one that can be safely selected.

### Type tree

```elisp
:number
   :integer
   :float
:sequence
   (:list . T)
   :array
      :string
      (:vector . T)
:hash-table
:symbol
:boolean
:nil
```

In the tree above, **simple** and **parametric** types are always the leaves.  
The **abstract** types are always a root of some other types.

There is no `:character` type because `:integer` is enough (characters are
represented as integers in Emacs Lisp).

### Special cases

1. Type `:nil` is a type of void-like functions (called "procedures" in some languages).  
   `nil` literal has `:nil` type.
   
2. If no type can be inferred, `nil` is reported.  
   There is no `:undefined` or `:any` type.

