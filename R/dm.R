

inferDM = function(node, env, counter) {
  UseMethod("inferDM")
}

# FIXME: !!! Now the top level in rstatic is just a Function.
inferDM.Function = function(node,
  env = typesys::TypeEnvironment(),
  counter = rstatic::Counter$new()
) {
  # FIXME:
  # Only use env if this function is called from the top level. Otherwise, a
  # new scope needs to be created.

  # NOTE: This creates a new scope. So we need to be careful about handling the
  # assumption set. How can we keep the function's assumption set without
  # breaking the outer assumption set?
  fn_env = typesys::TypeEnvironment()

  # Assign new type variables to the parameters.
  for (i in seq_along(node$params)) {
    param = node$params[[i]]
    var_name = sprintf("%s.%i", counter$increment(param$name))
    fn_env[[param$name]] = typesys::TypeVar(name)
  }
  param_env = fn_env

  # Compute the return type.
  # NOTE: Should this use a new counter?
  # FIXME: Traverse the graph in the right order.
  for (i in seq_along(node$cfg)) {
    block = node$cfg[[i]]

    for (j in seq_along(block$body)) {
      result = inferDM(block$body[[j]], fn_env, counter)
      fn_env = result$env
    }
  }

  # Make this a function type.
  #innerScope = typesys::applySubstitution(innerScope, result$sub)
  result$type = typesys::FunctionType(param_env@env, result$type)

  # FIXME: What's the correct scope here? Outer?
  result
}


inferDM.Symbol = function(node,
  env = typesys::TypeEnvironment(),
  counter = rstatic::Counter$new()
) {
  # Check Symbol is in assumptions.
  if ( !(node$name %in% names(env)) )
    stop(sprintf("`%s` is used before it is defined.", node$name))
  
  # Replace quantified type vars with new unquantified type vars.
  type = instantiate(env[[node$name]], counter)
  
  list(type = type, sub = typesys::Substitution(), env = env)
}

inferDM.Call = function(node,
  env = typesys::TypeEnvironment(),
  counter = rstatic::Counter$new()
) {
  result = inferDM(node$fn, env, counter)
  sub = result[["sub"]]
  fn_type = result[["type"]]

  # Compute type for each argument.
  arg_types = list()
  for (i in seq_along(node$args)) {
    # FIXME: The arguments could modify the type environment, if we allow for
    # things like
    #
    #     sum(x <- 3, 4)
    #
    # Temporarily apply the substitution to the type environment.
    temp_env = typesys::applySubstitution(env, sub)
    result = inferDM(node$args[[i]], temp_env)

    arg_types[[length(arg_types) + 1]] = result$type
    sub = typesys::compose(sub, result$sub)
  }

  # Unify the function type with the argument types.
  var_name = sprintf("fn.%i", counter$increment("fn"))
  ret_type = typesys::TypeVar(var_name)
  other_fn_type = typesys::FunctionType(arg_types, ret_type)
  unifier = typesys::unify(fn_type, other_fn_type)

  ret_type = typesys::applySubstitution(ret_type, unifier)
  sub = typesys::compose(sub, unifier)

  list(type = ret_type, sub = sub, env = env)
}

# Basically a let-expression
inferDM.Assign = function(node,
  env = typesys::TypeEnvironment(),
  counter = rstatic::Counter$new()
) {
  # Compute type for RHS.
  result = inferDM(node$read, env, counter)

  # FIXME: Quantify type variables that aren't in a.
  # result$type = quantify(result$type, result$a)
  
  # Static single assignments are like let-expressions that hold for the
  # remainder of the scope, so just modify the assumption set and let the top
  # level inference handle the rest.
  name = node$write$name
  result$env[[name]] = result$type

  result
}


# Literals ----------------------------------------

# inferDM.Null

inferDM.Logical = function(node,
  env = typesys::TypeEnvironment(),
  counter = rstatic::Counter$new()
) {
  list(type = typesys::BooleanType(), sub = typesys::Substitution(), env = env)
}


inferDM.Integer = function(node,
  env = typesys::TypeEnvironment(),
  counter = rstatic::Counter$new()
) {
  list(type = typesys::IntegerType(), sub = typesys::Substitution(), env = env)
}


inferDM.Numeric = function(node,
  env = typesys::TypeEnvironment(),
  counter = rstatic::Counter$new()
) {
  list(type = typesys::RealType(), sub = typesys::Substitution(), env = env)
}

inferDM.Character = function(node,
  env = typesys::TypeEnvironment(),
  counter = rstatic::Counter$new()
) {
  list(type = typesys::StringType(), sub = typesys::Substitution(), env = env)
}
