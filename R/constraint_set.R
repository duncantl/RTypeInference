
#' @export
ConstraintSet = R6::R6Class("ConstraintSet",
  public = list(
    constraints = NULL,

    initialize = function() {
      self$constraints = list()
    },

    append = function(left, right) {
      len = length(self$constraints)
      self$constraints[[len + 1]] = list(left, right)
      invisible (self)
    }

    #assign_type = function(name) {
    #  type = self$namegen$get(name)
    #  self$live[[name]] = type
    #  return (type)
    #},

    #get_type = function(name) {
    #  # Look up the variable name.
    #  type = self$live[[name]]
    #  if (is.null(type))
    #    return ("???")
    #  else
    #    return (type)
    #},

    #add_constraint = function(left, right) {
    #  n = length(self$constraints)
    #  self$constraints[[n + 1]] = list(left, right)
    #},

    #print_constraints = function(indent = 0) {
    #  lapply(self$constraints, function(ct) {
    #    left = char_or_class(ct[[1]])
    #    right = char_or_class(ct[[2]])

    #    msg = sprintf("%*s%s <=> %s\n", indent, "", left, right)
    #    cat(msg)

    #    invisible (msg)
    #  })
    #}
  )
)

#' @export
#print.InferState = function(x, indent = 2, ...) {
#  # Print header.
#  cat(sprintf("<%s>\n", class(x)[1]))
#  
#  # Print fields.
#  fields = ls(x)
#  fields = fields[!fields %in% c("clone", "initialize", "constraints")]
#  fields = paste("$", fields, sep = "", collapse = " ")
#  fields = strwrap(fields, indent = indent, exdent = indent)
#  cat(fields, sep = "\n")
#
#  # Print constraints.
#  cat(sprintf("\n%*s$constraints\n", indent, ""))
#  x$print_constraints(2 * indent)
#
#  invisible (NULL)
#}


#' @export
Unresolved = function(node) {
  structure(
    list(
      node = node
    ),
    class = "Unresolved"
  )
}


#' @export
print.Unresolved = function(x, ...) {
  cat("<Unresolved>\n")
  args = paste(sapply(x$args, char_or_class), collapse = ", ")
  msg = sprintf("%s(%s)\n", x$name, args)
  cat(strwrap(msg, exdent = 2))

  invisible (NULL)
}


char_or_class = function(x) if (is.character(x)) x else class(x)

get_arg_types = function(node, soln) {
  # TODO: Keep argument names.
  lapply(node$args, function(arg) {
    # FIXME: Calls and literals do not have a "type" field.
    type = arg$type
    if (!is(type, "character"))
      return (type)

    idx = match(type, names(soln))
    if (is.na(idx))
      return (type)

    return (soln[[type]])
  })
}
