Consider the example:

```r
x = 3L
y = 3.1
x + y
```

In this example, type inference needs to instantiate the `+` function as
$$
+: (Integer, Real) \to Real
$$
How we can describe the generic type of `+` in the type system? In a standard
Damas-Milner type system, we could set
$$
+: (a, b) \to b
$$
but this is not correct if we consider `y + x` instead of `x + y`.

What we really need is something like
$$
+: (a, b) \to \vee(a, b) \mathrm{where} a, b \leq Complex
$$
where $\vee()$ denotes the join (least upper bound) of its arguments. How can
this be integrated with the inference algorithm?

The `infer_dm.Call()` method calls `infer_dm.Symbol()` to get the type for `+`.
When control returns to `infer_dm.Call()`, the function's type is unified with
the call's (argument) types. Since unification is always applied to the
arguments first, it would be easy enough to have a separate unification
routine for `Join()` and some other type. This would just join the types before
performing standard unification.

In other words, rather than defining individual function handlers, we need to
define a flexible vocabulary for describing types. This strikes a balance
between allowing user customization and having a coherent type system. That is,
there are limits on how much flexibility we can support while still using
unification to infer types. For instance, it is well-known that subtyping is
not supported by unification and requires a cubic-time algorithm instead.

We also need to distinguish between a union, where the type could be one of
several but unknown, and a join, where the type is the least upper bound of
several known types (and thus known).

An alternative strategy is to use type classes, similar to what is done in
Haskell (cf. Wadler & Blott 1988). This amounts to keeping a dictionary of
overloaded definitions for each function. This strategy is conceptually
simpler although it still requires changes to the unification system. This
could also be a way to avoid dealing with subtyping.
