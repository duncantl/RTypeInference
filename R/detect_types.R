
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
detect_types = function(node) {
  state = InferState$new()
  .detect(node, state)
  return (state)
}


# Each `.detect` method should do one of two things:
#
# 1. Detect the type, assign the type to the node, and return the type.
# 2. Call `.detect` to descend in the tree.
#
.detect = function(node, state) {
  UseMethod(".detect")
}


#' @export
.detect.If = function(node, state) {
  .detect(node$predicate, state)
  # FIXME: Return an IfType, should be similar to existing inference code.
  .detect(node$true, state)
  .detect(node$false, state)

  # Start by just returning a RuntimeType.
}


#' @export
.detect.For = function(node, state) {
  # detect type of iterator.
  type = .detect(node$iter, state)

  # FIXME: What if the iterator is a heterogeneous list?
  ivar = node$ivar
  ivar$type = state$assign_type(ivar$name)
  state$add_constraint(ivar$type, type)

  .detect(node$body, state)
}


#' @export
.detect.While = function(node, state) {
  # FIXME: Consider a loop where type changes on each iteration.
  .detect(node$predicate, state)
  .detect(node$body, state)
}


#' @export
.detect.Assign = function(node, state) {
  # NOTE: RHS must come first!
  type = .detect(node$read, state)

  write = node$write
  write$type = state$assign_type(write$name)
  
  state$add_constraint(write$type, type)
  return (type)
}


#' @export
.detect.Call = function(node, state) {
  lapply(node$args, .detect, state)

  # Immediately infer types for simple calls with literal arguments.
  if ( all(sapply(node$args, is, "Literal")) ) {
    R_TYPES = c("character", "complex", "numeric", "integer", "logical")
    if (node$name %in% R_TYPES) {
      atom = switch(node$name,
        "character" = typesys::CharacterType()
        , "complex" = typesys::ComplexType()
        , "numeric" = typesys::RealType()
        , "integer" = typesys::IntegerType()
        , "logical" = typesys::BooleanType()
      )
    
      return (typesys::ArrayType(atom, args[[1]]))
    }
  }

  # Otherwise, defer inference to the resolution step.
  return (node)
}


#' @export
.detect.Return = function(node, state) {
  # FIXME: How should this work with IfType?
  type = .detect(node$args[[0]], state)
  
  state$add_constraint("$return", type)
  return (type)
}


#' @export
.detect.Bracket = function(node, state) {
  len = length(node$body)
  if (len == 0)
    return (typesys::NullType())

  types = lapply(node$body, .detect, state)
  return (types[[len]])
}

# Finished
# --------

#' @export
.detect.Symbol = function(node, state) {
  node$type = state$get_type(node$name)
}


# Literals
# --------
#' @export
.detect.Null      = function(node, state) node$type = typesys::NullType()

#' @export
.detect.Logical   = function(node, state) {
  type = typesys::BooleanType()
  len = length(node$value)
  if (len != 1)
    type = typesys::ArrayType(type, len)

  return (type)
}

#' @export
.detect.Integer   = function(node, state) {
  type = typesys::IntegerType()
  len = length(node$value)
  if (len != 1)
    type = typesys::ArrayType(type, len)

  return (type)
}

#' @export
.detect.Numeric   = function(node, state) {
  type = typesys::RealType()
  len = length(node$value)
  if (len != 1)
    type = typesys::ArrayType(type, len)

  return (type)
}

#' @export
.detect.Complex   = function(node, state) {
  type = typesys::ComplexType()
  len = length(node$value)
  if (len != 1)
    type = typesys::ArrayType(type, len)

  return (type)
}

#' @export
.detect.Character = function(node, state) {
  type = typesys::CharacterType()
  len = length(node$value)
  if (len != 1)
    type = typesys::ArrayType(type, len)

  return (type)
}

#' @export
.detect.default = function(node, state) {
  msg = sprintf("Cannot detect type for node class '%s'.", class(node)[1])
  stop(msg)
}
