# Assumptions need to keep track of which type variables have been added.


Assumption = function(name, value) {
  a = structure(list(
      vars = character(0),
      values = list()
    ), class = "Assumption")

  if (!missing(name) && !missing(value)) {
    a[[name]] = value
  }

  a
}

applySubstitution.Assumption =
function(x, sub) {
  x$values = lapply(x$values, typesys::applySubstitution, sub)
  x
}


`[[<-.Assumption` = function(x, i, value) {
  #vars = collectVars(value)
  #x$vars = union(x$vars, vars)
  x$values[[i]] = value
  x
}

`[[.Assumption` = function(x, i) {
  x$values[[i]]
}

`length.Assumption` = function(x) {
  length(x$values)
}

`names.Assumption` = function(x) {
  names(x$values)
}
