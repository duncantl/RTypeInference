
# Pending:

* Tests for larger bodies of code (not just snippets).
* Support extraction functions `[[` and `$`.
* Support for multidimensional `[`.
* Detect variables used as indexes in subset operations.
* Use mixins or similar for semantic type system.
* Document exported functions.

# Deferred:

* Infer metadata from assignment functions such as `dim<-`
* Detect branching `return()` behavior and branch conditions; currently we just 
  assume user functions are type stable.
* Detect iterator patterns in while loops.
* Consolidate code for handling math operations with RLLVMCompile.
