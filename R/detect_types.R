

# FIXME:
#' Type Detection
#'
#' This function detects and marks the types of nodes in a tree of ASTNodes
#' in-place. The return value includes a list of constraint tuples meant to be
#' solved via unification as part of the type inference process.
#'
#' @param node (ASTNode) An abstract syntax tree.
#' @export
detect_types = function(node, set = ConstraintSet$new()) {
  .detect_types(node, set)
  return (set)
}


#' @export
.detect_types = function(node, set) {
  UseMethod(".detect_types")
}

#' @export
.detect_types.Assign = function(node, set) {
  type = .detect_types(node$read)

  set$append(node$write$name, type)

  return (type)
}

#' @export
.detect_types.Phi = function(node, set) {
  rhs = do.call(typesys::Union, as.list(node$read))
  set$append(node$write, rhs)
}

#' @export
.detect_types.Call = function(node, set) {
  args = lapply(node$args, .detect_types, set)
  # FIXME: Nested calls might be a problem here. Need to generate a temporary
  # value for each call.

  # FIXME: Infer return type immediately if types are known for all arguments.
  # ...

  # FIXME: anonymous functions
  # Defer inference to the resolution step.
  do.call(typesys::Call, append(node$func, args))
}

#' @export
.detect_types.Symbol = function(node, set) {
  node$name
}

#' @export
.detect_types.Null = function(node, set) {
  typesys::NullType()
}

#' @export
.detect_types.Logical = function(node, set) {
  type = typesys::BooleanType()
  # FIXME:
  len = length(node$value)
  if (len != 1)
    type = typesys::ArrayType(type, len)

  return (type)
}

#' @export
.detect_types.Integer = function(node, set) {
  type = typesys::IntegerType()
  len = length(node$value)
  if (len != 1)
    type = typesys::ArrayType(type, len)

  return (type)
}

#' @export
.detect_types.Numeric = function(node, set) {
  type = typesys::RealType()
  len = length(node$value)
  if (len != 1)
    type = typesys::ArrayType(type, len)

  return (type)
}

#' @export
.detect_types.Complex = function(node, set) {
  type = typesys::ComplexType()
  len = length(node$value)
  if (len != 1)
    type = typesys::ArrayType(type, len)

  return (type)
}

#' @export
.detect_types.Character = function(node, set) {
  type = typesys::CharacterType()
  len = length(node$value)
  if (len != 1)
    type = typesys::ArrayType(type, len)

  return (type)
}

#' @export
.detect_types.default = function(node, set) {
  msg = sprintf("Cannot detect type for node class '%s'.", class(node)[1])
  stop(msg)
}
