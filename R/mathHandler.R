inferMathOpType =
function(x, typeCollector, ...)
{
  types = lapply(x[-1], inferType, typeCollector, ...)
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
  types = lapply(x[-1], inferType, typeCollector, ...)
  if(all(types %in% c("boolean", "int", "double")))
      "boolean"
  else
      "logical"
}
