# Description:
#   Functions to generate type constraint systems for a control-flow graph.


#' Infer Types for a Function
#'
#' @export

infer_types = function(code, init = list(), scalar = FALSE, ...) {
  UseMethod("infer_types")
}


#' @export
infer_types.ControlFlowGraph =
function(code, init = list(), scalar = FALSE, set = ConstraintSet$new(),
          ConstrainHandlers = getConstrainHandlers(),  ...)
{
  if(length(init) && !is(init, "ConstraintSet")) {
       #XXX This should be in the initialization method for set.
     if(length(names(init)) == 0) 
         names(init) = names(cfg$params)[seq(along = init)]
     
     mapply(function(type, name)
             set$append(name, type),  # Need to convert the types from user-convenient types to those we expect (in typesys?)
            init, paste0(names(init), "_1"))
  }
  
  constraints = constrain(code, set = set, scalar = scalar)
  
  types = solve(constraints)

  return (types)
}

#' @export
infer_types.default = function(code, init = list(), ...) {
  cfg = rstatic::to_cfg(rstatic::to_ast(code), in_place = TRUE)
  infer_types(cfg, init, ...)
}


# The default method may handle the following. If so, remove these.
# However the infer_types.call should probably grab the values of the arguments.

infer_types.function =
function(code, init = list(), scalar = FALSE, set = ConstraintSet$new(),
          ConstrainHandlers = getConstrainHandlers(),  ...)
{
   infer_types(rstatic::to_cfg(code), init, scalar, set, ConstrainHandlers, ...)
}

# This is probably not useful for calls such as x + 1 where we don't know x
# but we a) we can get the current type of x from the workspace, and
# b) calls such as foo(2) do make sense - we can get the return type.
infer_types.call =
function(code, init = list(), scalar = FALSE, set = ConstraintSet$new(),
          ConstrainHandlers = getConstrainHandlers(),  ...)
{
   f = function() x
   body(f) = code
   infer_types(f, init, scalar, set, ConstrainHandlers, ...)
   # probably want the return type.
}

`infer_types.{` =
function(code, init = list(), scalar = FALSE, set = ConstraintSet$new(),
          ConstrainHandlers = getConstrainHandlers(),  ...)
{
   f = function() {}
   body(f) = code
   infer_types(f, init, scalar, set, ConstrainHandlers, ...)
}
