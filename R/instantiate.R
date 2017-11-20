instantiate = function(x, counter) {
  new_vars = lapply(x@quantified, function(q) {
    name = sprintf("t%i", counter$increment("t"))
    typesys::TypeVariable(name)
  })
  names(new_vars) = x@quantified

  x@quantified = character(0)

  # Now just substitute.
  sub = typesys::Substitution(new_vars)
  typesys::do_substitution(x, sub)
}

