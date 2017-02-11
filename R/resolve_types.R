

#' Type Resolution
#'
#' @param state (InferState)
#'
#' @export
resolve_types = function(state) {
  # Apply unification algorithm to the constraint set.
  set = state$constraints
  soln_set = list()

  while (length(set) > 0) {
    constraint = set[[1]]
    set = set[-1]

    # Compute solution for a constraint.
    soln = unify(constraint[[1]], constrant[[2]], soln_set)

    # Apply solution set to remaining constraints.
    cons = lapply(set, lapply, subs_soln, soln)

    # Apply solution to solution set.
    soln_set = lapply(soln_set, subs_soln, soln)

    # Union solution with solution set.
    soln_set = modifyList(soln_set, soln)
  }

  return (soln_set)
}


# TODO: Use dispatch.
unify = function(x, y, soln_set) {
  # Unify if both constraints are non-variables; otherwise assign to variable.

  if (is(x, "character") && is(y, "AtomicType")) {
    return (structure(list(y), names = x))
  }

  #if (is(y, "ASTNode")) {
  #  # Look up the call in the call handler table.
  #  idx = match(y$name, names(call_handlers))
  #  if (!is.na(idx)) {
  #    type = call_handlers[[idx]](y, soln_set)

  #    if (!is.null(type))
  #      return (structure(list(type), names = x))
  #  }

  #  # TODO: Try type inference.
  #  stop("Cross-call type inference not yet supported.")

  #} else if (is(y, "CompositeType")) {
  #  # TODO: composite types

  #} else if (is(y, "character") || is(y, "AtomicType")) {
  #  return (structure(list(y), names = x))
  #}
}


# Apply a solution to a type expression.
subs_soln = function(x, soln) {
  # For the moment, only work with variables.
  if (!is(x, "character"))
    return (x)

  idx = match(x, names(soln))
  if (is.na(idx))
    return (x)

  return (soln[[idx]])
}
