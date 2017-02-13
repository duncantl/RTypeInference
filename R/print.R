
.print = function(x, ...) cat(format(x, ...), "\n")

.format_tag = function(x) sprintf("<%s>", class(x)[1])

#' @export
format.ConstraintSet = function(x, ...) {
  tag = .format_tag(x)

  constraints = vapply(x$constraints, format, character(1), indent = 2)
  str = paste(constraints, collapse = "\n")

  sprintf("%s\n%s\n", tag, str)
}

#' @export
print.ConstraintSet = .print


#' @export
format.Constraint = function(x, indent = 0, ...) {
  str = sprintf("%s <=> %s", format(x[[1]]), format(x[[2]]))
  strwrap(str, indent = indent)
}

#' @export
print.Constraint = .print


#' @export
format.SolutionSet = function(x, ...) {
  tag = .format_tag(x)

  values = vapply(x, format, character(1))
  solutions = sprintf("  %s => %s", names(x), values)
  str = paste(solutions, collapse = "\n")

  sprintf("%s\n%s\n", tag, str)
}

#' @export
print.SolutionSet = .print
