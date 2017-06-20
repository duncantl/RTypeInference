# Description:
#

#' Solve a Set of Type Constraints
#'
#' @param a (ConstraintSet) A set of type constraints.
#'
#' @export
solve.ConstraintSet = function(a, b, ...) {
  # Apply unification algorithm to the constraint set.
  constraints = a$constraints
  solutions = SolutionSet()

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

  return (solutions)
}


unify = function(x, y) {
  UseMethod("unify")
}

#' @export
unify.character = function(x, y) {
  if (is(y, "typesys::Type")) {
    solution = SolutionSet(x, y)

  } else if (is(y, "typesys::Call")) {
    type = infer_call(y)
    solution = SolutionSet(x, type)

  } else {
    # FIXME: some elements might be non-types
    msg = sprintf("Could not resolve %s <=> %s.", format(x), format(y))
    stop(msg)
  }

  return (solution)
}


# Infer Types for a Call
#
infer_call = function(call) {
  # Look up call handler.
  idx = match(call@func, names(CALL_HANDLERS))
  if (is.na(idx)) {
    # FIXME: Try inference if no handler is available.
    stop(sprintf("No handler defined for function '%s'.", call@func))

  } else {
    handler = CALL_HANDLERS[[idx]]

    # Resolve any inner calls.
    args = lapply(call@args, function(arg) {
      if (is(arg, "typesys::Call"))
        infer_call(arg)
      else
        arg
    })

    # Take the Cartesian product of all Unions in the arguments.
    args_list = expand_args(args)
    if (length(args_list) == 0) {
      msg = sprintf("Missing argument types for `%s`.", format(call))
      stop(msg)
    }

    # Call the handler with the args.
    u = lapply(args_list, handler)
    type = do.call(typesys::Union, u)
  }

  return (type)
}


# Expand an Argument Type List
#
# When given a list of argument types that contains Unions, this function
# produces a list of all possible combinations of argument types.
expand_args = function(args) {
  # Set up vector of lengths.
  seqs = lapply(args, function(arg) {
    if (is(arg, "typesys::Union"))
      seq_len(length(arg))
    else
      1L
  })
  combos = t(do.call(expand.grid, seqs))

  results = apply(combos, 2, function(combo) {
    Map(function(arg, i) {
      if (is(arg, "typesys::Union"))
        arg[[i]]
      else
        arg
    }, args, combo)
  })

  results = Filter(function(result) {
    !any(vapply(result, is.character, logical(1)))
  }, results)

  return (results)
}


#' Apply a Solution
#'
#' This function substitutes a solution into a constraint set or solution set.
#'
#' @param x The constraint set or solution set.
#' @param soln The solution to apply.
apply_solution = function(x, soln) {
  UseMethod("apply_solution")
}

#' @export
apply_solution.Constraint = function(x, soln) {
  as_constraint(NextMethod())
}

apply_solution.SolutionSet = function(x, soln) {
  as_solution_set(NextMethod())
}

#' @export
apply_solution.list = function(x, soln) {
  lapply(x, apply_solution, soln)
}

#' @export
apply_solution.character = function(x, soln) {
  idx = match(x, names(soln))
  if (is.na(idx))
    return (x)

  type = soln[[idx]]
  type@value = typesys::SymbolValue(x)

  return (type)
}

#' @export
`apply_solution.typesys::Call` = function(x, soln) {
  # FIXME: Move to class methods.
  x@args = apply_solution(x@args, soln)
  return (x)
}

#' @export
`apply_solution.typesys::Union` = function(x, soln) {
  # FIXME: Move to class methods.
  x@types = apply_solution(x@types, soln)
  x = typesys::simplify(x)

  return (x)
}

#' @export
`apply_solution.typesys::Type` = function(x, soln) {
  return (x)
}
