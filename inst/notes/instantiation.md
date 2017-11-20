Some functions are polymorphic, which means that the types of their parameters
are flexible. An example of this is the identity function
```r
function(x) x
```
where the parameter `x` is not constrained to any specific type.

We can represent the type of the identity function by $$\forall a. a \to a$$,
where $$a$$ is a "type variable" that serves as a placeholder for a concrete
type. The quantifier $$\forall a$$ says that $$a$$ is a parameter, which means
$$a$$ can be replaced by a different type each time the function is called.
Consequently, this kind of polymorphism is called parametric polymorphism.

Type variables are also used when a type is unknown. For instance, in
```r
function(x) {
  y = f(x)
  g(y)
}
```
the type of `x` is initially unknown, so `x` is assigned a new type variable,
say $$t_1$$. If it is known that
$$
f: \forall a. a \to a
g: Integer \to Boolean
$$
then we can conclude that $$t_1 = Integer$$. This requires several steps.
First, the instance of `f` in the expression `f(x)` must have type $$t_1 \to
t_1$$ because $$x: t_1$$. In the implementation, this is computed in four
steps:

1. The function `f` is instantiated to type $$t_2 \to t_2$$ for a new type
   variable $$t_2$$. Note that $$t_2$$ is not quantified.
2. The call `f(x)` says that `f` must also have type $$t_1 \to t_3$$ for a new
   type variable $$t_3$$. In other words, `f` must be a function and must
   accept an argument with the same type as `x`; the return type is unknown.
3. The two types for `f` must agree, so they are unified. This produces a
   substitution, for example $$t_1 \mapsto t_2, t_3 \mapsto t_2$$.
4. The substitution is applied to the type environment, so $$x: t_2$$. This is
   important because the substitution may affect the original type variables
   for some of the arguments (as with $$t_1$$ here).

As a result, the type of `f(x)` is $$t_2$$ and therefore the type of `y` is
$$t_2$$. Since $$t_2$$ is already bound to `x` in the type environment, it
cannot be quantified.

Next, the same procedure is applied to the call `g(y)`. The function `g` is not
polymorphic, so the first step is simplified:

1. The function `g` has type $$Integer \to Boolean$$
2. The call `g(x)` says that `g` must also have type $$t_2 \to t_4$$ for a new
   type variable $$t_4$$.
3. The two types for `g` must agree, so they are unified. This produces the
   substitution $$t_2 \mapsto Integer, t_4 \mapsto Boolean$$.
4. The substitution is applied to the type environment, so $$x: Integer$$.
