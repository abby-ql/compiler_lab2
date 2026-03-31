### Semantic Analysis for Cdim+2 written in Scala

This lab implements a semantic analyser for a small imperative language (*Cdim+2*), extending a parsed abstract syntax tree with type information and enforcing typing rules.
The analyser traverses an annotated AST and assigns types (`int`, `bool`, and multi-dimensional arrays) to expressions and variables, rejecting programs that violate typing or scoping rules.


* Maintain a symbol table (environment) mapping identifiers to types
* Traverse the AST to:
  * type-check expressions
  * validate assignments (LHS and RHS types match)
  * enforce boolean conditions in `if` and `while`
* Handle scoping and shadowing correctly using block structure
* Update `Variable` nodes from unknown (`Svoid`) to concrete types

For arrays:

* represent multi-dimensional types recursively
* ensure index expressions are integers
* ensure array accesses resolve to basic types (`int` or `bool`)



**Running**

```bash
scala cdim.Main test-good.cdim
```

Test files:

* `*-good.cdim` → valid programs
* `*-bad.cdim` → should be rejected by semantic analysis


**More Details**

* Full type checking for statements (`checkStmt`)
* Expression typing for variables and array elements
* Scope-aware symbol table with support for shadowing
* Validation of array accesses (dimensions and index types)
* Additional test cases beyond the provided ones

