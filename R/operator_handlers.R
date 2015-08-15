# Description:
#   Functions for handling operators.

MATH_OPS = c("+", "-", "*", "/", "^")
LOGIC_OPS = c("<", ">", "<=", ">=", "==", "!=", "|", "||", "&", "&&")


inferMathOpType =
function(x, typeCollector, ...)
{
  # complex > numeric > integer > logical
  # Division and exponentiation alway produce numeric or complex.
  op_name = as.character(x[[1]])

  types = lapply(x[-1], inferTypes, typeCollector, ...)

  length = max(vapply(types, length, 0L))

  types = lapply(types, atomicType)
  atom =
  if (any_is(types, "ComplexType"))
    ComplexType()
  else if (any_is(types, "NumericType") || op_name %in% c("/", "^"))
    NumericType()
  else if(any_is(types, "IntegerType"))
    IntegerType()
  else if(any_is(types, "LogicalType"))
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

  makeVector(LogicalType(), length)
}


inferSubsetType =
    # to handle var[i - 1]  and var[i, j] and var[ f(a, b, c) ]
function(x, typeCollector, ...)
{
  # FIXME: Empty arguments are not handled correctly.
  # TODO: For now, all we handle is single-dimension, constant subsets.
  types = lapply(x[-1], inferTypes, typeCollector, ...)
  atom = atomicType(types[[1]])
  arg_types = types[-1]

  if (length(arg_types) == 1) {
    # Length comes from arguments, atomic type comes from variable.
    arg_type = arg_types[[1]]

    length = 
      if (is(atomicType(arg_type), "LogicalType")) {
        # For logical argument, length is number of TRUEs.
        sum(value(arg_type, NA))
      } else {
        length(arg_type)
      }

    type = makeVector(atom, length)

  } else
    stop("Multi-dimensional subsetting is not yet supported!")

  return(type)
  #varname = getVarName(x)
  #ty = typeCollector$getType(varname)
  #if(is(ty, "UpdateType"))
  #   ty = ty@type
  #
  #if(isScalar(ty)) {
  #  ty
  #} else {
  #   if(isScalarSubset(x, type, varname, typeCollector, ...))
  #       mapTypeToScalar(ty)
  #   else
  #     ty
  #}
}


isScalarSubset =
function(call, type, varname, typeCollector, ...)    
{
# have to handle matrices.
#    
  isScalar( inferTypes(call[[3]], typeCollector, ...) )
}
