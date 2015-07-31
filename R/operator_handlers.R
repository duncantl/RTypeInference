# Description:
#   Functions for handling operators.

MATH_OPS = c("+", "-", "*", "/", "^")
LOGIC_OPS = c("<", ">", "<=", ">=", "==", "!=", "|", "||", "&", "&&")


inferMathOpType =
function(x, typeCollector, ...)
{
  # logical -> integer -> numeric -> complex
  # Division and exponentiation alway produce numeric or complex.
  op_name = as.character(x[[1]])

  types = lapply(x[-1], inferTypes, typeCollector, ...)

  # Check if any operands are vectors.
  vector_types = types[vapply(types, is, TRUE, "VectorType")]

  if (length(vector_types) == 0) {
    # TODO: A supertype calculation function would be useful here.
    if (any_is(types, "ComplexType"))
      new("ComplexType")
    else if (any_is(types, "NumericType") || op_name %in% c("/", "^"))
      new("NumericType")
    else if(any_is(types, "IntegerType"))
      new("IntegerType")
    else if(any_is(types, "LogicalType"))
      new("IntegerType")
    else
      NA

  } else {
    # Get the length of the longest vector.
    length = max(sapply(vector_types, slot, "length"))

    if (any_is(types, "ComplexVectorType"))
      new("ComplexVectorType", length = length)
    else if (any_is(types, "NumericVectorType") || op_name == "/")
      new("NumericVectorType", length = length)
    else if(any_is(types, "IntegerVectorType"))
      new("IntegerVectorType", length = length)
    else if(any_is(types, "LogicalVectorType"))
      new("IntegerVectorType", length = length)
    else
      NA
  }
}


inferLogicOpType =
function(x, typeCollector, ...)
{
  # TODO: What if a generic logical op is used and return type isn't
  # logical?
  op_name = as.character(x[[1]])

  types = lapply(x[-1], inferTypes, typeCollector, ...)

  # Check if any operands are vectors.
  vector_types = types[vapply(types, is, TRUE, "VectorType")]

  if (length(vector_types) == 0 || op_name %in% c("||", "&&")) {
    new("LogicalType")

  } else {
    length = max(sapply(vector_types, slot, "length"))
    new("LogicalVectorType", length = length)
  }
}


inferSubsetType =
    # to handle var[i - 1]  and var[i, j] and var[ f(a, b, c) ]
function(x, typeCollector, ...)
{
browser()    
  varname = getVarName(x)
  ty = typeCollector$getType(varname)
  if(is(ty, "UpdateType"))
     ty = ty@type
  
  if(isScalar(ty)) {
    ty
  } else {
     if(isScalarSubset(x, type, varname, typeCollector, ...))
         mapTypeToScalar(ty)
     else
       ty
  }
}


isScalarSubset =
function(call, type, varname, typeCollector, ...)    
{
# have to handle matrices.
#    
  isScalar( inferTypes(call[[3]], typeCollector, ...) )
}