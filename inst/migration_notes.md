# Notes for Migration to `infer_dm()`


Notes from Duncan that might still be relevant:
```r
# constrain.R ----------------------------------------
# FIXME: Nested calls might be a problem here. Need to generate a temporary
# value for each call.
# FIXME: Infer return type immediately if types are known for all arguments.
# ...
# XXX This is temporarily here.  We'll add customization handlers for this.


# constraint_set.R ----------------------------------------
# avoiding adding duplicate constraints. This messes solve() up. We may want to
# fix that
# but probably good to avoid redundant constraints.
#XXX not using identical as one might have a known value and the other unknown.
# (see assignSEXP2.R in R2llvm/tests)
# If this constraint has more information (e.g. a value), we may want to
# replace the existing "identical" one with this.


# constraint_handlers.R ----------------------------------------
# Need to match the argument by name for other functions, eg. runif(n, 1, 2)
# but  runif(1, 2, n = n1)


# solve.R ----------------------------------------
# Try inference if no handler is available.
#XXXX !!!
fun = get(call@func, globalenv()) # XXX from anywhere!


# infer_types.R ----------------------------------------
  #XXX This should be in the initialization method for set.
if(length(names(init)) == 0) 
    names(init) = names(code$params)[seq(along = init)]

mapply(function(type, name)
          set$append(name, type),  # Need to convert the types from
          # user-convenient types to those we expect (in typesys?) ```

# For infer_types.function(), infer_types.call(), `infer_types.{`():
# The default method may handle the following. If so, remove these.
# However the infer_types.call should probably grab the values of the arguments.

# For infer_types.call():
# This is probably not useful for calls such as x + 1 where we don't know x
# but we a) we can get the current type of x from the workspace, and
# b) calls such as foo(2) do make sense - we can get the return type.
