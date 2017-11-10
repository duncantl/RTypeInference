infer_dm = function(node, env, counter, active, top) {
  UseMethod("infer_dm")
}

# FIXME:
infer_dm.Function = function(node,
  env = typesys::TypeEnvironment$new(),
  counter = rstatic::Counter$new(),
  top
) {
  # FIXME: This is really hacky. We need a better way to mark the top level
  # globals as being active.
  if (top) {
    env$active = names(env$env)
    names(env$active) = names(env$env)
  }
  # NOTE: Alternatively, we could provide a way to see children of a type
  # environment or change how type environments are passed around. As it is,
  # a separate parameter for the top level feels hacky.
  #
  # Top level, so use this TypeEnvironment. Otherwise, create a new
  # TypeEnvironment for the definition.

  # NOTE: This would be good place to install defaults for the global
  # environment.

  # NOTE: How can we keep the function's TypeEnvironment without breaking the
  # outer TypeEnvironment?
  old_env = env
  env = typesys::TypeEnvironment$new(parent = env)

  # Assign new type variables to the parameters.
  params = vapply(node$params, function(param) {
    name = param$name
    var_name = sprintf("t%i", counter$increment("t"))
    type = typesys::TypeVar(var_name)

    env[[name]] = type
    env$active[[param$basename]] = name

    name
  }, NA_character_)

  # Compute the return type.
  dom_t = rstatic::dominator_tree(node$cfg)
  result = inferBlock(node$cfg$entry, node$cfg, dom_t, env, counter)

  # Make this a function type.
  env = typesys::applySubstitution(env, result$sub)
  result$type = typesys::FunctionType(env[params], result$type)

  # Return outer scope.
  if (!top)
    result$env = old_env

  result
}

# This function traverses the control flow graph.
inferBlock = function(b, cfg, dom_t, env, counter) {
  block = cfg[[b]]

  result = lapply(block$body, infer_dm, env, counter, top = FALSE)

  # Now visit descendants.
  # Reset active variables for each child by copying the current active
  # variables.
  locals = env$active

  children = setdiff(which(dom_t == b), b)
  if (length(children) > 0)
    result = lapply(children, function(child) {
      env$active = locals
      inferBlock(child, cfg, dom_t, env, counter)
    })

  # Return result of last block.
  result[[length(result)]]
}


infer_dm.Symbol = function(node,
  env = typesys::TypeEnvironment$new(),
  counter = rstatic::Counter$new(),
  top
) {
  # Walk up the type environments looking for the symbol.
  # FIXME: Need to use the active name in the environment.
  e = env
  while ( !(node$basename %in% names(e$active)) ) {
    if (is.null(e$parent)) {
      stop(sprintf("`%s` is used before it is defined.", node$name))
    }

    e = e$parent
  }

  # Replace quantified type vars with new unquantified type vars.
  active = e$active[[node$basename]]
  type = e[[active]]
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
  result = infer_dm(node$fn, env, counter, top)
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
    temp_env = typesys::applySubstitution(env$clone(), sub)
    result = infer_dm(node$args[[i]], temp_env, top)

    arg_types[[length(arg_types) + 1]] = result$type
    sub = typesys::compose(sub, result$sub)
  }

  # Unify the function type with the argument types.
  var_name = sprintf("fn%i", counter$increment("fn"))
  ret_type = typesys::TypeVar(var_name)
  other_fn_type = typesys::FunctionType(arg_types, ret_type)
  unifier = typesys::unify(fn_type, other_fn_type)

  ret_type = typesys::applySubstitution(ret_type, unifier)
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

  result$type = quantify(result$type, env)
  
  # Static single assignments are like let-expressions that hold for the
  # remainder of the scope, so just modify the assumption set and let the top
  # level inference handle the rest.
  name = node$write$name
  result$env[[name]] = result$type

  # Set this variable as active.
  result$env$active[[node$write$basename]] = name

  result
}


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
