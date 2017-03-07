# Description:
#   Functions for handling operators.

#MATH_OPS = c("+", "-", "*", "/", "^")
#LOGIC_OPS = c("<", ">", "<=", ">=", "==", "!=", "|", "||", "&", "&&")

#' Upcast Types for Math Operator
#'
#' This function determines the output type of a unary or binary math operator
#' when given the input types.
#'
#' @param types (list) The input types.
#' @param op (character) The name of the math operator.
#'
#' @export
upcast_math = function(types, op) {
  # complex > numeric > integer > logical
  # Operators `/` and `^` always produce numeric or complex.
  if (length(types) == 1) {
    if (op %in% c("+", "-"))
      return (types[[1]])
    else
      stop(sprintf("Operator '%s' is not unary.", op))
  }

  # TODO: This is not correct for vectors.
  type =
    if (any_is(types, "typesys::ComplexType"))
      typesys::ComplexType()
    else if (any_is(types, "typesys::RealType") || op %in% c("/", "^"))
      typesys::RealType()
    else if (any_is(types, "typesys::IntegerType"))
      typesys::IntegerType()
    else if (any_is(types, "typesys::BooleanType"))
      typesys::IntegerType()
    else
      stop(sprintf("Invalid types for operator '%s'.", op))

  return (type)
}


inferLogicOpType =
function(x, tc, ...)
{
  # TODO: What if a generic logical op is used and return type isn't
  # logical?
  op_name = as.character(x[[1]])

  types = lapply(x[-1], .infer_types, tc, ...)

  length = max(vapply(types, length, 0L))
  if (op_name %in% c("||", "&&"))
    length = 1L

  makeVector(BooleanType(), length)
}


inferSubsetType =
    # to handle var[i - 1]  and var[i, j] and var[ f(a, b, c) ]
function(x, tc, ...)
{
  # FIXME: Empty arguments are not handled correctly.
  # FIXME: Lists are not handled correctly.
  # TODO: For now, all we handle is single-dimension, constant subsets.
  types = lapply(x[-1], .infer_types, tc, ...)
  atom = element_type(types[[1]])
  arg_types = types[-1]

  if (length(arg_types) == 1) {
    # Length comes from arguments, atomic type comes from variable.
    arg_type = arg_types[[1]]

    length = 
      if (is(element_type(arg_type), "BooleanType")) {
        # For logical argument, length is number of TRUEs.
        sum(value(arg_type, NA))
      } else {
        length(arg_type)
      }

    type = makeVector(atom, length)

  } else
    stop("Multi-dimensional subsetting is not yet supported!")

  return(type)
}
