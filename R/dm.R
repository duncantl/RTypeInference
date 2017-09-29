

inferDM = function(node, a, counter) {
  UseMethod("inferDM")
}

inferDM.ControlFlowGraph = function(node, a, counter = rstatic::Counter$new()) {
  # FIXME: Traverse the graph in the right order.
  for (i in seq_along(node)) {
    a = inferDM(node[[i]], a, counter)
  }

  a
}

inferDM.BasicBlock = function(node, a, counter) {
  for (i in seq_along(node$body)) {
    result = inferDM(node$body[[i]], a, counter)
    a = result$a
  }

  a
}

inferDM.Symbol = function(node, a, counter) {
  # Check Symbol is in assumptions.
  if ( !(node$name %in% names(a)) )
    stop(sprintf("`%s` is used before it is defined.", node$name))
  
  # Replace quantified type vars with new unquantified type vars.
  type = instantiate(a[[node$name]], counter)
  
  list(type = type, sub = typesys::Substitution(), a = a)
}

inferDM.Call = function(node, a, counter) {
  result = inferDM(node$fn, a, counter)
  sub = result[["sub"]]
  fn_type = result[["type"]]

  # Compute type for each argument.
  arg_types = list()
  for (i in seq_along(node$args)) {
    # Temporarily apply the substitution to the assumptions.
    result = inferDM(node$args[[i]], typesys::applySubstitution(a, sub))

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

  list(type = ret_type, sub = sub, a = a)
}

inferDM.Function = function(node, a, counter) {
  # FIXME:

  # NOTE: This creates a new scope. So we need to be careful about handling the
  # assumption set. How can we keep the function's assumption set without
  # breaking the outer assumption set?
  innerScope = typesys::TypeEnvironment()

  # Assign new type variables to the parameters.
  for (i in seq_along(node$params)) {
    param = node$params[[i]]
    var_name = sprintf("%s.%i", counter$increment(param$name))
    innerScope[[param$name]] = typesys::TypeVar(name)
  }

  # Compute the return type.
  # FIXME: Need to actually convert the body to a control flow graph.
  result = inferDM(node$body, innerScope)

  # Make this a function type.
  #innerScope = typesys::applySubstitution(innerScope, result$sub)
  result$type = typesys::FunctionType(innerScope$values, result$type)

  result
}

# Basically a let-expression
inferDM.Assign = function(node, a, counter) {
  # Compute type for RHS.
  result = inferDM(node$read, a, counter)

  # FIXME: Quantify type variables that aren't in a.
  # result$type = quantify(result$type, result$a)
  
  # Static single assignments are like let-expressions that hold for the
  # remainder of the scope, so just modify the assumption set and let the top
  # level inference handle the rest.
  name = node$write$name
  result$a[[name]] = result$type

  result
}


# Literals ----------------------------------------

# inferDM.Null

inferDM.Logical = function(node, a, counter) {
  list(type = typesys::BooleanType(), sub = typesys::Substitution(), a = a)
}


inferDM.Integer = function(node, a, counter) {
  list(type = typesys::IntegerType(), sub = typesys::Substitution(), a = a)
}


inferDM.Numeric = function(node, a, counter) {
  list(type = typesys::RealType(), sub = typesys::Substitution(), a = a)
}

inferDM.Character = function(node, a, counter) {
  list(type = typesys::StringType(), sub = typesys::Substitution(), a = a)
}
