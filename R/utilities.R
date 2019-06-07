# Description:
#   Utility functions.

#' Get Return Type from Solution Set
#'
#' This function gets the return type from the solved types for a function.
#'
#' @param (SolutionSet) Complete type information for a function.
#'
#' @export
return_type = function(solutions) {
  is_return = startsWith(names(solutions), "._return_")
  type = do.call(typesys::Union, solutions[is_return])

  return (type)
}


#' Type Annotation
#'
#' This is a no-op function for annotating functions with types.
#'
#' @rdname typeInfo_
#' @export
.typeInfo =
function(...) {}


evalTypeInfo =
function(annotation)
  # Evaluate the contents of a .typeInfo annotation.
{
  lapply(as.list(annotation)[-1], eval)
}


makeVector =
function(atom, dimension) 
  # Make a vector unless length is 1.
{
  length = prod(dimension)
  if(!is.na(length) && length == 1)
    atom
  else
    ArrayType(atom, dimension)
}


any_is =
function(objects, class_name)
  # Test inheritance for multiple objects.
{
  any(vapply(objects, is, TRUE, class_name))
}


upcast =
function(types)
  # Upcast to the most general type.
{
  # character > complex > numeric > integer > logical
  # TODO:
  #   * Merge with math operator upcasting?
  #   * Test on semantic types

  # FIXME: This breaks for things that make lists.

  types = lapply(types, element_type)

  if (any_is(types, "CharacterType"))
    CharacterType()
  else if (any_is(types, "ComplexType"))
    ComplexType()
  else if (any_is(types, "NumericType"))
    NumericType()
  else if (any_is(types, "IntegerType"))
    IntegerType()
  else if (any_is(types, "LogicalType"))
    LogicalType()
  else
    # FIXME:
    stop("Upcast fell through!")
}

`:=` =
function(x, y)
{
  x = substitute(x)
  if (length(x) < 2 || x[[1]] != "c")
    stop("left-hand side of unpack must be c(...).")
  x = x[-1]

  is_name = vapply(x, is.name, TRUE)
  if (!all(is_name))
    stop("cannot unpack into non-variables.")
  x = as.character(x)

  len_x = length(x)
  len_y = length(y)
  if (len_x != len_y)
    stop(sprintf("cannot unpack %i elements into %i variables.", len_y, len_x))

  keep = x != ""
  mapply(assign, x[keep], y[keep], MoreArgs = list(envir = parent.frame()),
    SIMPLIFY = FALSE, USE.NAMES = FALSE)

  invisible(NULL)
}
