Sometimes it may be possible to infer types for the parameters of a function
based on how they are used inside the function. For example, we might know that
the function `f` has $$f: Integer \to Boolean$$. Then if we see a function
definition
```r
g = function(x, y) f(x)
```
we can infer that `x` must have $$x: Integer$$ and therefore $$g: (Integer, a)
\to Boolean$$ for an unknown type variable $$a$$. In other words, the function
`g` is (trivially) polymorphic in its second parameter `y`, but its first
parameter `x` must be an integer.

When we actually run inference on this example, the first step is to inform the
inference system what we know about `f`. We can do this by creating a new type
environment that contains the type information for `f`. Type environments are
analogous to R environments, but instead of holding (name, value) pairs they
hold (name, type) pairs. To do this we need the __typesys__ package.
```r
tenv = typesys::TypeEnvironment$new(
  "f" = Integer ~ Boolean
)
```
That package includes a formula notation shorthand for writing type signatures
for functions. The argument types appear to the left of `~` and the return type
appears to the right.

Now we can run type inference. In `infer_dm.Function()`, the function
parameters are assigned type variables since their types are unknown. That is,
$$x: t_1$$, $$y: t_2$$.

Inference proceeds to `infer_dm.Call()` for the call `f(x)`. The type of `f` is
looked up in `tenv` and unified with the type $$t_1 \to t_3$$ for new type
variable $$t_3$$. The resulting substitution includes $$t_1 \mapsto Integer$$.

When control returns to `infer_dm.Function()`, the substitution can be applied
to the function's type environment (a descendant of `tenv`). This sets $$x:
Integer$$. Finally, the function's type is assembled using information in its
type environment. The result is the type $$(Integer, t_2) \to Boolean$$

