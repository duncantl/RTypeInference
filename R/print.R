
.print = function(x, ...) cat(format(x, ...), "\n\n")

.format_tag = function(x) sprintf("<%s>", class(x)[1])

#' @export
format.ConstraintSet = function(x, ...) {
  tag = .format_tag(x)

  cons = vapply(x$constraints, function(con) {
    lhs = format(con[[1]])
    rhs = format(con[[2]])
    sprintf("  %s <=> %s", lhs, rhs)
  }, character(1))
  cons = paste(cons, collapse = "\n")

  sprintf("%s\n%s", tag, cons)
}

#' @export
print.ConstraintSet = .print
