
.print = function(x, ...) cat(format(x, ...), "\n")

.show = function(object) cat(format(object, indent = 0), "\n")

.format_tag = function(x) sprintf("<%s>", class(x)[1])

#' @export
setMethod("show", signature("RTypeInference::InferHelper"), .show)

#' @export
setMethod("format", signature("RTypeInference::InferHelper"),
function(x, ...)
{
  tag = paste0(.format_tag(x), "\n")

  if (length(x) == 0)
    return (tag)

  usedef = vapply(x@.Data, function(elt) {
    if (is.null(elt$defined_as))
      defn_as = "no definition\n  "
    else
      defn_as = paste0("defined as ", format(elt$defined_as), "\n  ")

    if (length(elt$used_as) == 0)
      return (paste0(defn_as, "no uses"))

    used_as = vapply(elt$used_as, format, NA_character_)
    used_as = paste(used_as, collapse = "; ")
    paste0(defn_as, "used as ", used_as)
  }, NA_character_)

  # `NAME` defined as X
  #   used as Y
  msg = paste0("`", names(x), "` ", usedef, collapse = "\n\n")

  paste0(tag, "\n", msg, "\n")
})

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
