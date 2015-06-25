# Description:
#   Functions for handling operators.


inferMathOpType =
function(x, typeCollector, ...)
{
  # logical -> integer -> numeric -> complex
  # Division always produces numeric or complex.
  op_name = as.character(x[[1]])

  types = lapply(x[-1], inferTypes, typeCollector, ...)

  # Check if any operands are vectors.
  vector_types = types[vapply(types, is, TRUE, "VectorType")]

  if (length(vector_types) == 0) {
    # TODO: A supertype calculation function would be useful here.
    if (any_is(types, "ComplexType"))
      new("ComplexType")
    else if (any_is(types, "NumericType") || op_name == "/")
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
  types = lapply(x[-1], inferTypes, typeCollector, ...)
  if(all(types %in% c("boolean", "int", "double")))
      "boolean"
  else
      "logical"
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


isScalar =
function(type)
{
  all(type %in% c("boolean", "int", "double", "string")) # ADD    
#  type %in% c("logical", "integer", "numeric", "character") # ADD
}

RToScalarMap = c(logical = "boolean", integer = "int", numeric = "double", character = "string")

BasicRVectorTypes = names(RToScalarMap)

mapTypeToScalar =
function(type)
{
   RToScalarMap[type]
}

isScalarSubset =
function(call, type, varname, typeCollector, ...)    
{
# have to handle matrices.
#    
  isScalar( inferTypes(call[[3]], typeCollector, ...) )
}
