# Description:
#   S4 classes for types.

# Basic unknown value class. Later this could be expanded to a reference
# system.
UnknownValue =
  setClass("UnknownValue", contains = "NULL")

# Virtual Types

setClass("Type", contains = "VIRTUAL")

setClass("AtomicType", contains = c("Type", "VIRTUAL"),
  # TODO: Still not clear this is the right class to place the value slot on.
  slots = list(
    value = "ANY"
  ),
  prototype = list(
    value = UnknownValue()
  )
)

setClass("SemanticType", contains = c("Type", "VIRTUAL"),
  slots = list(
    atom = "AtomicType"
  )
)

# Methods

setGeneric("identicalType",
  # Check whether two types are identical.
  #
  # This generic checks whether two types are identical up to value.
  function(x, y) standardGeneric("identicalType"),
  valueClass = "logical")

setMethod("identicalType",
  signature(x = "ANY"),
  function(x, y) identical(x, y)
)

setMethod("identicalType",
  signature(x = "AtomicType"),
  function(x, y) is(x, class(y))
)

setMethod("identicalType",
  signature(x = "SemanticType"),
  function(x, y) {
    is_identical = is(x, class(y))

    is_identical = 
      is_identical &
      sapply(slotNames(x),
        function(name) {
          identicalType(slot(x, name), slot(y, name))
        }
      )

    all(is_identical)
  }
)

# TODO: Check if value is a vector.
setGeneric("value",
  function(self, unknown) standardGeneric("value")
)

setMethod("value",
  signature(self = "AtomicType", unknown = "missing"),
  function(self, unknown) self@value
)

setMethod("value",
  signature(self = "AtomicType"),
  function(self, unknown) {
    if (is(self@value, "UnknownValue"))
      unknown
    else
      self@value
  }
)

setMethod("value",
  signature(self = "SemanticType"),
  function(self, unknown) {
    self = atomicType(self)
    callGeneric()
  }
)

setGeneric("value<-", function(self, value) standardGeneric("value<-"))

setMethod("value<-",
  signature(self = "AtomicType"),
  function(self, value) {
    self@value = value
    return(self)
  }
)

setMethod("value<-",
  signature(self = "SemanticType"),
  function(self, value) {
    value(atomicType(self)) = value
    return(self)
  }
)

setGeneric("atomicType",
  function(self) standardGeneric("atomicType"),
  valueClass = "AtomicType")

setMethod("atomicType",
  signature(self = "AtomicType"),
  function(self) self
)

setMethod("atomicType",
  signature(self = "SemanticType"),
  function(self) self@atom
)

setGeneric("atomicType<-",
  function(self, value) standardGeneric("atomicType<-"),
  valueClass = "Type")

setMethod("atomicType<-",
  signature(self = "SemanticType"),
  function(self, value) {
    self@atom = value
    return(self)
  }
)

setMethod("length",
  signature(x = "Type"),
  function(x) 1L
)

# Atomic Types

UnknownType =
  setClass("UnknownType", contains = "AtomicType")

setMethod("length",
  signature(x = "UnknownType"),
  function(x) NA_integer_
)

NullType =
  setClass("NullType", contains = "AtomicType")

LogicalType =
  setClass("LogicalType", contains = "AtomicType")
IntegerType =
  setClass("IntegerType", contains = "AtomicType")
NumericType =
  setClass("NumericType", contains = "AtomicType")
ComplexType =
  setClass("ComplexType", contains = "AtomicType")
CharacterType =
  setClass("CharacterType", contains = "AtomicType")
#setClass("StringType", contains = "AtomicType")

# Semantic Types --------------------------------------------------

VectorType =
  function(atom, length) {
    .VectorType(atom = atom, length = as.integer(length))
  }

.VectorType =
  setClass("VectorType", contains = "SemanticType",
    slots = list(
      length = "integer"
    )
  )

setMethod("length",
  signature(x = "VectorType"),
  function(x) x@length
)

setMethod("length<-",
  signature(x = "VectorType"),
  function(x, value) {
    x@length = as.integer(value)
    return(x)
  }
)

IteratorType =
  setClass("IteratorType", contains = "SemanticType")
#setClass("ListType", contains = "ScalarType")

# Branch Type
# FIXME: Set this up for the composable type system!

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
