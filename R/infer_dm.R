infer_dm = function(node, env, counter, top) {
  UseMethod("infer_dm")
}

# FIXME:
infer_dm.Function = function(node,
  env = typesys::TypeEnvironment$new(),
  counter = rstatic::Counter$new(),
  top
) {
  old_env = env
  env = typesys::TypeEnvironment$new(parent = old_env)

  # Assign new type variables to the parameters.
  params = vapply(node$params, function(param) {
    name = param$name
    var_name = sprintf("t%i", counter$increment("t"))
    type = typesys::TypeVar(var_name)

    env[[name]] = type

    name
  }, NA_character_)

  # Compute the return type.
  # NOTE: Assume that the blocks are already sorted.
  for (block in node$cfg$blocks)
    result = inferBlock(block, env, counter)

  # Make this a function type.
  env = typesys::do_substitution(env, result$sub)
  result$type = typesys::FunctionType(env[params], result$type)

  # Return outer scope.
  # NOTE: Using `top` to flag the top level is hacky. We could instead provide
  # a way to see children of a type environment or change how type environments
  # are passed around.
  if (!top)
    result$env = old_env

  result
}

# This function traverses the control flow graph.
# FIXME: This should be `infer_dm.Brace()`
inferBlock = function(block, env, counter) {
  lapply(block$phi, infer_dm, env, counter, top = FALSE)

  result = lapply(block$body, infer_dm, env, counter, top = FALSE)

  # Return result of last block.
  result[[length(result)]]
}


infer_dm.Phi = function(node,
  env = typesys::TypeEnvironment$new(),
  counter = rstatic::Counter$new(),
  top
) {
  # NOTE: We could potentially treat Phi like any other call.
  sub = typesys::Substitution()

  # Get the types for the incoming values.
  arg_types = lapply(node$read, function(arg) {
    result = infer_dm(arg, env, counter, top)
    sub <<- typesys::compose(sub, result$sub)

    result$type
  })

  # Unify the types.
  # FIXME: Right now this is just a check that the types are equal, but we may
  # want to do something other than emit an error if they are not.
  Reduce(typesys::unify, arg_types)

  # Set the type of the write variable to the unified type.
  # This part is the same as `infer_dm.Assign()`
  result = list(type = NULL, sub = sub, env = env)
  result$type = typesys::quantify(arg_types[[1]], env)
  name = node$write$name
  result$env[[name]] = result$type

  result
}


infer_dm.Symbol = function(node,
  env = typesys::TypeEnvironment$new(),
  counter = rstatic::Counter$new(),
  top
) {
  # Walk up the type environments looking for the symbol.
  # FIXME: Need to use the active name in the environment.
  e = env

  while ( !(node$name %in% names(e)) ) {
    if (is.null(e$parent))
      stop(sprintf("`%s` is used before it is defined.", node$name))

    e = e$parent
  }

  # Replace quantified type vars with new unquantified type vars.
  type = e[[node$name]]
  if (length(type@quantified) > 0) {
    type = instantiate(type, counter)
  }
  
  list(type = type, sub = typesys::Substitution(), env = env)
}

infer_dm.Call = function(node,
  env = typesys::TypeEnvironment$new(),
  counter = rstatic::Counter$new(),
  top
) {
  # Get the function's type at definition from the environment.
  result = infer_dm(node$fn, env, counter, top)
  sub = result[["sub"]]
  def_type = result[["type"]]

  # Construct the function's type at this call.
  arg_types = lapply(node$args, function(arg) {
    result = infer_dm(arg, env, counter, top)
    sub <<- typesys::compose(sub, result$sub)

    result$type
  })
  var_name = sprintf("fn%i", counter$increment("fn"))
  ret_type = typesys::TypeVar(var_name)
  call_type = typesys::FunctionType(arg_types, ret_type)

  # Make sure all args have current type variables.
  call_type = typesys::do_substitution(call_type, sub)

  # Unify the function type with the argument types.
  unifier = typesys::unify(def_type, call_type)

  ret_type = typesys::do_substitution(ret_type, unifier)
  env = typesys::do_substitution(env, unifier)
  sub = typesys::compose(sub, unifier)

  list(type = ret_type, sub = sub, env = env)
}

# Basically a let-expression
infer_dm.Assign = function(node,
  env = typesys::TypeEnvironment$new(),
  counter = rstatic::Counter$new(),
  top
) {
  # Compute type for RHS.
  result = infer_dm(node$read, env, counter, top)

  result$type = typesys::quantify(result$type, env)
  
  # Static single assignments are like let-expressions that hold for the
  # remainder of the scope, so just modify the assumption set and let the top
  # level inference handle the rest.
  name = node$write$name
  result$env[[name]] = result$type

  result
}

# Control flow ----------------------------------------
infer_dm.If = function(node,
  env = typesys::TypeEnvironment$new(),
  counter = rstatic::Counter$new(),
  top
) {
  list(type = NULL, sub = typesys::Substitution(), env = env)
}

infer_dm.For = infer_dm.If
infer_dm.While = infer_dm.If


# Literals ----------------------------------------

# infer_dm.Null

infer_dm.Logical = function(node,
  env = typesys::TypeEnvironment$new(),
  counter = rstatic::Counter$new(),
  top
) {
  list(type = typesys::BooleanType(), sub = typesys::Substitution(), env = env)
}


infer_dm.Integer = function(node,
  env = typesys::TypeEnvironment(),
  counter = rstatic::Counter$new(),
  top
) {
  list(type = typesys::IntegerType(), sub = typesys::Substitution(), env = env)
}


infer_dm.Numeric = function(node,
  env = typesys::TypeEnvironment(),
  counter = rstatic::Counter$new(),
  top
) {
  list(type = typesys::RealType(), sub = typesys::Substitution(), env = env)
}

infer_dm.Character = function(node,
  env = typesys::TypeEnvironment(),
  counter = rstatic::Counter$new(),
  top
) {
  list(type = typesys::StringType(), sub = typesys::Substitution(), env = env)
}
