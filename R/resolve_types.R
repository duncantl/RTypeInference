

#' Type Resolution
#'
#' @param state (InferState)
#'
#' @export
resolve_types = function(set) {
  # Apply unification algorithm to the constraint set.
  constraints = set$constraints
  solutions = list()

  while (length(constraints) > 0) {
    cons = constraints[[1]]
    constraints = constraints[-1]

    # Unify cons; cons is returned when unification fails.
    soln = unify(cons[[1]], cons[[2]])
    if (inherits(soln, "Constraint")) {
      # FIXME: Keep track of which constraints couldn't be unified.
      next
    }

    # Substitute soln into the constraints and solutions.
    constraints = apply_solution(constraints, soln)
    solutions = apply_solution(solutions, soln)

    # Add the constraint to the solution set.
    solutions = modifyList(solutions, soln)
  }

  list (constraints = set, solutions = solutions)
}


unify = function(x, y) {
  UseMethod("unify")
}

unify.character = function(x, y) {
  if (is(y, "AtomicType")) {
    return (structure(list(y), names = x))
  } else if (is(y, "Union")) {
    all_same = all(vapply(y@types[-1], identical, logical(1), y@types[[1]]))
    if (all_same) {
      return (structure(list(y@types[[1]]), names = x))
    } else {
      # FIXME: some elements might be non-types
      stop("Could not resolve types!")
    }
  }
}

#unify = function(x, y, soln_set) {
#  # Unify if both constraints are non-variables; otherwise assign to variable.
#
#  if (is(x, "character") && is(y, "AtomicType")) {
#    return (structure(list(y), names = x))
#  }
#
#  #if (is(y, "ASTNode")) {
#  #  # Look up the call in the call handler table.
#  #  idx = match(y$name, names(call_handlers))
#  #  if (!is.na(idx)) {
#  #    type = call_handlers[[idx]](y, soln_set)
#
#  #    if (!is.null(type))
#  #      return (structure(list(type), names = x))
#  #  }
#
#  #  # TODO: Try type inference.
#  #  stop("Cross-call type inference not yet supported.")
#
#  #} else if (is(y, "CompositeType")) {
#  #  # TODO: composite types
#
#  #} else if (is(y, "character") || is(y, "AtomicType")) {
#  #  return (structure(list(y), names = x))
#  #}
#}


apply_solution = function(constraints, soln) {
  UseMethod("apply_solution")
}


apply_solution.list = function(constraints, soln) {
  lapply(constraints, apply_solution, soln)
}


apply_solution.character = function(x, soln) {
  idx = match(x, names(soln))
  if (is.na(idx))
    return (x)

  return (soln[[idx]])
}

apply_solution.Union = function(x, soln) {
  x@types = apply_solution(x@types, soln)
  return (x)
}

apply_solution.Type = function(x, soln) {
  return (x)
}
