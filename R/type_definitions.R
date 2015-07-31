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

setGeneric("infer", function(self, args) {
  standardGeneric("infer")
})

setClass("ConditionalType", contains = "Type",
  slots = list(
    handler = "function"
  )
)

ConditionalType = function(handler = function(args) {})
  # Construct a ConditionalType.
{
  # TODO: S4 validation.

  # Check that handler includes a condition list.
  if ( !("args" %in% names(formals(handler))) )
    stop("Handler must have parameter 'args'.")

  if ( body(handler)[[1]] != "{" )
    body(handler) = call("{", body(handler))

  new("ConditionalType", handler = handler)
}

pushCondition_q = function(self, condition, if_type, else_type)
  # Push a new condition onto the ConditionalType.
{
  body = body(self@handler)

  # Conditions are stored in the final if-else statement.
  length = length(body)
  if_statement = body[[length]]

  if (class(if_statement) == "if") {
    # Push branch onto existing if-else statement.
    if_statement = call("if", condition, if_type, if_statement)
    body[length] = list(if_statement)

  } else if (!missing(else_type)) {
    # No if-else statement, so create one.
    if_statement = call("if", condition, if_type, else_type)
    body[length + 1] = list(if_statement)

  } else {
    stop("There was no existing else_type, so one must be supplied.")
  }

  body(self@handler) = body
  return(self)
}

pushCondition = function(self, condition, if_type, else_type)
{
  pushCondition_q(self, substitute(condition), substitute(if_type),
    substitute(else_type))
}

getBranchTypes = function(self)
  # Get the type on each branch.
{
  body = body(self@handler)

  length = length(body)
  if_statement = body[[length]]

  if_apply(if_statement, class)
}

if_apply = function(statement, f, simplify = TRUE)
  # Map over the branches of an if-else statement.
{
  # if <condition> <body> <else>
  answer = list( f(statement[[3]]) )

  if (length(statement) > 3) {
    answer = append(answer,
      if (class(statement[[4]]) == "if")
        if_apply(statement[[4]], f)
      else
        f(statement[[4]])
    )
  }

  if (simplify)
    simplify2array(answer)
  else answer
}

setMethod("infer",
  list(self = "ConditionalType"),
  function(self, args)
  {
    self@handler(args)
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

