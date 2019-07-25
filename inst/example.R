# 2019-07-24

library(rstatic)
library(typesys)
library(RTypeInference)

# Convert some code to a CFG. Due to a bug in rstatic, the code must be wrapped
# in a function.
#
node = to_blocks(function() {
  x = 31
  if (x > 10)
    z = 10
  else
    y = 10
                                       
  x = x^2
})


# Generate type constraints from the code.
#
# The result is a list with $contraints, the list of constraints, and $helper,
# an InferHelper object. The InferHelper object records the correspondence
# between program variables and type variables.
#
result = constrain(node)

# Solve the constraints. The return value is a Substitution of type variables
# that satistfies the constraints.
#
# Passing in the counter is necessary because the solver may generate some new
# type variables (for calls to polymorphic functions).
#
sub = solve(result$constraints, result$helper@counter)


# Call the Substitution on the InferHelper object to see which types were
# found for each program variable.
#
sub(result$helper)

# -----

# Note that we can also set types for program variables beforehand, by setting
# up an InferHelper before constraint generation:

helper = InferHelper()
# Function (a, a) -> a
type = formula_to_type(c(a, a) ~ a)
# Add type to helper as the type for the `^` function.
helper = RTypeInference:::add_def(helper, "^", type)

# Generate constraints.
result = constrain(node, helper)

# Now proceed as in the previous example.
# ...
