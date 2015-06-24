inferMathOpType =
function(x, typeCollector, ...)
{
  types = lapply(x[-1], inferTypes, typeCollector, ...)
#browser()  
  if(any(types == "numeric"))
      "numeric"
  else if(any(types == "double"))
      "double"
  else if(any(types == "integer"))
      "integer"
  else
      "int"
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
