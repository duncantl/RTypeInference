quantify = function(type, env) {
  # Quantify type variables that aren't in the type environment.
  candidates = typesys::collect_type_vars(type)
  type@quantified = setdiff(candidates, env$bound_type_vars)

  type
}


instantiate = function(x, counter) {
  new_vars = lapply(x@quantified, function(q) {
    name = sprintf("t%i", counter$increment("t"))
    typesys::TypeVar(name)
  })
  names(new_vars) = x@quantified

  x@quantified = character(0)

  # Now just substitute.
  sub = typesys::Substitution(new_vars)
  typesys::applySubstitution(x, sub)
}

