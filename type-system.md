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

**Abstract type** - type that is an abstraction around several types.
Abstract type tells about interface, but never about the concrete type.  
Represented with `keyword`.

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

In the tree above, simple and parametric types are always the leaves.
The abstract is always a root of some types.

`:character` is not used because `:integer` is enough.
