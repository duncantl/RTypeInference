
#' @include NameGenerator.R
NULL


#' Type Detection
#'
#' This function detects and marks the types of nodes in a tree of ASTNodes
#' in-place. The return value includes a list of constraint tuples meant to be
#' solved via unification as part of the type inference process.
#'
#' @param node (ASTNode) An abstract syntax tree.
#' @export
infer = function(node) {
  state = InferState$new()
  .infer(node, state)
  return (state)
}


# Each `.infer` method should do one of two things:
#
# 1. Detect the type, assign the type to the node, and return the type.
# 2. Call `.infer` to descend in the tree.
#
.infer = function(node, state) {
  UseMethod(".infer")
}


#' @export
.infer.If = function(node, state) {
  .infer(node$predicate, state)
  # FIXME: Return an IfType
  .infer(node$true, state)
  .infer(node$false, state)
}


#' @export
.infer.For = function(node, state) {
  # Infer type of iterator.
  type = .infer(node$iter, state)

  # FIXME: What if the iterator is a heterogeneous list?
  ivar = node$ivar
  ivar$type = state$assign_type(ivar$name)
  state$add_constraint(ivar$type, type)

  .infer(node$body, state)
}


#' @export
.infer.While = function(node, state) {
  # FIXME: Consider a loop where type changes on each iteration.
  .infer(node$predicate, state)
  .infer(node$body, state)
}


#' @export
.infer.Assign = function(node, state) {
  # NOTE: RHS must come first!
  type = .infer(node$right, state)

  left = node$left
  left$type = state$assign_type(left$name)
  
  state$add_constraint(left$type, type)
  return (type)
}


#' @export
.infer.Call = function(node, state) {
  # FIXME: Lookup the type or type handler for the call.
  # This is one case where lack of information will be problematic.
  lapply(node$args, .infer, state)

  type = "???"
  return (type)
}


#' @export
.infer.Return = function(node, state) {
  type = .infer(node$args[[0]], state)
  
  state$add_constraint("$return", type)
  return (type)
}


#' @export
.infer.Symbol = function(node, state) {
  node$type = state$get_type(node$name)
}


# TODO: Check for scalar vs array.
#' @export
.infer.Null      = function(node, state) node$type = typesys::NullType()

#' @export
.infer.Logical   = function(node, state) {
  type = typesys::BooleanType()
  len = length(node$value)
  if (len != 1)
    type = typesys::ArrayType(type, len)

  node$type = type
}

#' @export
.infer.Integer   = function(node, state) {
  type = typesys::IntegerType()
  len = length(node$value)
  if (len != 1)
    type = typesys::ArrayType(type, len)

  node$type = type
}

#' @export
.infer.Numeric   = function(node, state) {
  type = typesys::RealType()
  len = length(node$value)
  if (len != 1)
    type = typesys::ArrayType(type, len)

  node$type = type
}

#' @export
.infer.Complex   = function(node, state) {
  type = typesys::ComplexType()
  len = length(node$value)
  if (len != 1)
    type = typesys::ArrayType(type, len)

  node$type = type
}

#' @export
.infer.Character = function(node, state) {
  type = typesys::CharacterType()
  len = length(node$value)
  if (len != 1)
    type = typesys::ArrayType(type, len)

  node$type = type
}


#' @export
.infer.Bracket = function(node, state) {
  len = length(node$body)
  if (len == 0)
    return (typesys::NullType())

  types = lapply(node$body, .infer, state)
  return (types[[len]])
}

#' @export
.infer.default = function(node, state) {
  msg = sprintf("Cannot infer type for node class '%s'.", class(node)[1])
  stop(msg)
}
