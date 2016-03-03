# Description:
#   Functions for handling operators.

MATH_OPS = c("+", "-", "*", "/", "^")
LOGIC_OPS = c("<", ">", "<=", ">=", "==", "!=", "|", "||", "&", "&&")


inferMathOpType =
function(x, typeCollector, ...)
{
  # complex > numeric > integer > logical
  # Division and exponentiation alway produce numeric or complex.
  # Get operator name and argument types.
  op_name = as.character(x[[1]])

  args = lapply(x[-1], inferTypes, typeCollector, ...)

  length = max(vapply(args, length, 0L))

  args = lapply(args, element_type)

  atom =
  if (any_is(args, "ComplexType"))
    ComplexType()
  else if (any_is(args, "RealType") || op_name %in% c("/", "^"))
    RealType()
  else if(any_is(args, "IntegerType"))
    IntegerType()
  else if(any_is(args, "BooleanType"))
    # Note: logicals are implicitly casted to integers.
    IntegerType()
  else
    stop(sprintf("Invalid type passed to operation `%s`.", op_name))

  makeVector(atom, length)
}


inferLogicOpType =
function(x, typeCollector, ...)
{
  # TODO: What if a generic logical op is used and return type isn't
  # logical?
  op_name = as.character(x[[1]])

  types = lapply(x[-1], inferTypes, typeCollector, ...)

  length = max(vapply(types, length, 0L))
  if (op_name %in% c("||", "&&"))
    length = 1L

  makeVector(BooleanType(), length)
}


inferSubsetType =
    # to handle var[i - 1]  and var[i, j] and var[ f(a, b, c) ]
function(x, typeCollector, ...)
{
  # FIXME: Empty arguments are not handled correctly.
  # FIXME: Lists are not handled correctly.
  # TODO: For now, all we handle is single-dimension, constant subsets.
  types = lapply(x[-1], inferTypes, typeCollector, ...)
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
