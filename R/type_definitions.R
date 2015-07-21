# Description:
#   S4 classes for types.

setClass("Type")

# Special Types --------------------------------------------------

NullType =
setClass("NullType", contains = "Type")


setClass("IndexType", contains = "Type",
  slots = list(
    type = "Type"
  )
)

setClassUnion("TypeOrCall", c("Type", "call"))
setGeneric("infer", function(expr, envir) {
  standardGeneric("infer")
})

setClass("ConditionalType", contains = "Type",
  slots = list(
    conditions = "list",
    default = "TypeOrCall"
  )
)

ConditionalType = function(conditions, default)
{
  new("ConditionalType", conditions = conditions, default = default)
}

addCondition = function(self, condition, type)
{
  rule = Condition(condition = condition, type = type)
  self@conditions = append(self@conditions, rule)

  self
}

setMethod("infer",
  list(expr = "ConditionalType"),
  function(expr, envir)
  {
    # TODO:
    # What if multiple conditions are true? We could just stop at the first
    # true condition.
    true_conditions = sapply(expr@conditions, infer, envir)
    selection = match(TRUE, true_conditions)

    type =
      if (is.na(selection))
        expr@default
      else
        expr@conditions[[selection]]@type

    eval(type, envir)
  }
)

Condition = 
setClass("Condition",
  slots = list(
    condition = "language",
    type = "TypeOrCall"
  )
)

Condition = function(condition, type)
{
  new("Condition", condition = condition, type = type)
}

Condition_q = function(condition, type)
{
  new("Condition", condition = substitute(condition), type = substitute(type))
}

setMethod("infer", 
  c(expr = "Condition"),
  function(expr, envir)
  {
    eval(expr@condition, envir = envir)
  }
)

# Scalar Types --------------------------------------------------
setClass("ScalarType", contains = "Type")

setClass("LogicalType", contains = "ScalarType")
setClass("IntegerType", contains = "ScalarType")
setClass("NumericType", contains = "ScalarType")
setClass("ComplexType", contains = "ScalarType")
setClass("CharacterType", contains = "ScalarType")
setClass("ListType", contains = "ScalarType")

# Vector Types --------------------------------------------------
setClass("VectorType", contains = "Type",
  slots = list(
    length = "integer"
  )
) 

setClass("LogicalVectorType", contains = "VectorType")
LogicalVectorType = function(length)
  new("LogicalVectorType", length = as.integer(length))

setClass("IntegerVectorType", contains = "VectorType")
IntegerVectorType = function(length) 
  new("IntegerVectorType", length = as.integer(length))

setClass("NumericVectorType", contains = "VectorType")
NumericVectorType = function(length)
  new("NumericVectorType", length = as.integer(length))

setClass("ComplexVectorType", contains = "VectorType")
ComplexVectorType = function(length)
  new("ComplexVectorType", length = as.integer(length))

setClass("CharacterVectorType", contains = "VectorType")
CharacterVectorType = function(length) 
  new("CharacterVectorType", length = as.integer(length))

setClass("ListVectorType", contains = "VectorType")

# Utilities --------------------------------------------------
vectorTypeToScalarType =
function(type)
{
  if (is(type, "ScalarType"))
    return(type)

  switch(class(type)[[1]],
    "LogicalVectorType" = new("LogicalType"),
    "IntegerVectorType" = new("IntegerType"),
    "NumericVectorType" = new("NumericType"),
    "ComplexVectorType" = new("ComplexType"),
    "CharacterVectorType" = new("CharacterType"),
    "ListVectorType" = new("ListType"),
    NA
  )
}

