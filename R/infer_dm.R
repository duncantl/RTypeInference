#' Infer Types for a Function
#'
#' This function infers the types for a function.
#'
#' @param node (Function) The function to infer types for.
#' @param tenv (TypeEnvironment) The global type environment.
#' @param counter (Counter) A counter for naming type variables.
#'
#' @export
infer_dm = function(node, env, counter) {
  UseMethod("infer_dm")
}


#' @export
infer_dm.Function = function(node,
  env = typesys::TypeEnvironment$new(),
  counter = rstatic::Counter$new()
) {
  # Create a new type environment for this function.
  env = typesys::TypeEnvironment$new(parent = env)

  # Assign new type variables to the parameters.
  params = vapply(node$params, function(param) {
    var_name = sprintf("t%i", counter$increment("t"))
    type = typesys::TypeVar(var_name)

    name = param$name
    env[[name]] = type

    name
  }, NA_character_)

  # Visit the blocks (under the assumption that they are already sorted).
  for (block in node$cfg$blocks) {
    lapply(block$phi, infer_dm, env, counter)

    line_types = lapply(block$body, infer_dm, env, counter)

    # Return result of last block.
    ret_type = line_types[[length(line_types)]]
  }

  # Make this a function type and attach the environment.
  type = typesys::FunctionType(env[params], ret_type)
  type@type_environment = env

  type
}


#' @export
infer_dm.Call = function(node,
  env = typesys::TypeEnvironment$new(),
  counter = rstatic::Counter$new()
) {
  # Get the function's type at definition from the environment.
  def_type = infer_dm(node$fn, env, counter)

  # Construct the function's type at this call.
  arg_types = lapply(node$args, infer_dm, env, counter)

  var_name = sprintf("fn%i", counter$increment("fn"))
  ret_type = typesys::TypeVar(var_name)
  call_type = typesys::FunctionType(arg_types, ret_type)

  # Unify the function type with the argument types.
  unifier = typesys::unify(def_type, call_type)
  typesys::do_substitution(env, unifier)

  typesys::do_substitution(ret_type, unifier)
}


#' @export
infer_dm.Symbol = function(node,
  env = typesys::TypeEnvironment$new(),
  counter = rstatic::Counter$new()
) {
  # Walk up the type environments looking for the symbol.
  while ( !(node$name %in% names(env)) ) {
    if (is.null(env$parent))
      stop(sprintf("`%s` is used before it is defined.", node$name))

    env = env$parent
  }

  # Replace quantified type vars with new unquantified type vars.
  type = env[[node$name]]
  if (length(type@quantified) > 0)
    type = instantiate(type, counter)
  
  type
}


#' @export
infer_dm.Assign = function(node,
  env = typesys::TypeEnvironment$new(),
  counter = rstatic::Counter$new()
) {
  type = infer_dm(node$read, env, counter)
  type = typesys::quantify(type, env)
  
  # Static single assignments are like let-expressions that hold for the
  # remainder of the scope. So add this to the type environment.
  env[[node$write$name]] = type
}


#' @export
infer_dm.Phi = function(node,
  env = typesys::TypeEnvironment$new(),
  counter = rstatic::Counter$new()
) {
  # Unify the incoming types.
  #
  # FIXME: Right now this is just a check that the types are equal, but we may
  # want to do something other than emit an error if they are not.
  arg_types = lapply(node$read, infer_dm, env, counter)
  Reduce(typesys::unify, arg_types)
  type = typesys::quantify(arg_types[[1]], env)

  env[[node$write$name]] = type
}


# Control flow ----------------------------------------

#' @export
infer_dm.If = function(node,
  env = typesys::TypeEnvironment$new(),
  counter = rstatic::Counter$new()
) {
  NULL
}

#' @export
infer_dm.For = infer_dm.If

#' @export
infer_dm.While = infer_dm.If


# Literals ----------------------------------------

# infer_dm.Null

#' @export
infer_dm.Logical = function(node,
  env = typesys::TypeEnvironment$new(),
  counter = rstatic::Counter$new()
) {
  typesys::BooleanType()
}


#' @export
infer_dm.Integer = function(node,
  env = typesys::TypeEnvironment(),
  counter = rstatic::Counter$new()
) {
  typesys::IntegerType()
}


#' @export
infer_dm.Numeric = function(node,
  env = typesys::TypeEnvironment(),
  counter = rstatic::Counter$new()
) {
  typesys::RealType()
}

#' @export
infer_dm.Character = function(node,
  env = typesys::TypeEnvironment(),
  counter = rstatic::Counter$new()
) {
  typesys::StringType()
}
