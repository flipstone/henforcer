# modulo

`modulo` is a tool to help us keep dependencies amongs entire trees of Haskell
modules from becoming hopelessly engangled.

## Examples

Here are some examples of the sorts of module imports we would prefer not have.

### Example 1

* A.Module imports B.C.Module1
* B.C.Module2 imports A.Module

We want to prevent this because:

* Trees A and B.C depend each other in a cycle
* Trees A and B also depend on each other in a cycle

### Example 2

* A.Module imports B.Module1
* B.Module2 imports A.Module2

We want to prevent this because:

* Trees A and B depend on each other in a cycle

### Example 3

* A.B.Module imports A.C.Module1
* A.C.Module2 imports A.B.Module

We want to prevent this because:

* Trees A.B and A.C depend on each other in a cycle
* The A depends on itself, which is not a cycle

### Example 4

* A.Module1 imports A.B.Module1
* A.B.Module2 imports A.Module1

We want to prevent this because:

* Trees A and A.B depend on each other in a cycle

### Counterexample

* A.Module1 imports A.B.Module
* A.Module2 imports A.Module1

This is perfectly fine for these reasons:

*  Tree A depends on Tree A.B, but not in a cycle.
*  A.Module2 depends on sibling A.Module1, but not in a cycle
*  A.Module2 could also depend on A.B.Module directly without
*  creating any cycle

## Known Issues

Currently imports of modules that contain no imports themselves end up getting
ignored when we filter out non-local imports. Eventually we can improve this
logic.
