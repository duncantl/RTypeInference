# Description:
#   Functions to generate type constraints from a control-flow graph.

#' Compute Type Constraints from a Control-flow Graph
#'
#'
#' @export
constrain = function(cfg
  , id = 1L
  , dom_t = dom_tree(cfg)
  , set = ConstraintSet$new()
) {
  # Iterate over Phi nodes.
  lapply(cfg[[id]]$phi, constrain_ast, set)

  # Iterate over body, generating type constraints.
  lapply(cfg[[id]]$body, constrain_ast, set)

  # Descend to next blocks.
  children = setdiff(which(dom_t == id), id)
  lapply(children, function(i) constrain(cfg, i, dom_t, set))

  return (set)
}


#' @export
constrain_ast = function(node, set) {
  UseMethod("constrain_ast")
}

#' @export
constrain_ast.Assign = function(node, set) {
  type = constrain_ast(node$read)

  set$append(node$write$name, type)

  return (type)
}

#' @export
constrain_ast.Phi = function(node, set) {
  rhs = do.call(typesys::Union, as.list(node$read))
  set$append(node$write, rhs)
}

#' @export
constrain_ast.Call = function(node, set) {
  args = lapply(node$args, constrain_ast, set)
  # FIXME: Nested calls might be a problem here. Need to generate a temporary
  # value for each call.

  # FIXME: Infer return type immediately if types are known for all arguments.
  # ...

  # FIXME: anonymous functions
  # Defer inference to the resolution step.
  do.call(typesys::Call, append(node$func, args))
}

#' @export
constrain_ast.Replacement = function(node, set) {
  warning("Constraints are not generated for replacements.")
  #browser()
}

#' @export
constrain_ast.Symbol = function(node, set) {
  node$name
}

#' @export
constrain_ast.Null = function(node, set) {
  typesys::NullType()
}

#' @export
constrain_ast.Logical = function(node, set) {
  type = typesys::BooleanType()
  # FIXME:
  len = length(node$value)
  if (len != 1)
    type = typesys::ArrayType(type, len)

  return (type)
}

#' @export
constrain_ast.Integer = function(node, set) {
  type = typesys::IntegerType()
  len = length(node$value)
  if (len != 1)
    type = typesys::ArrayType(type, len)

  return (type)
}

#' @export
constrain_ast.Numeric = function(node, set) {
  type = typesys::RealType()
  len = length(node$value)
  if (len != 1)
    type = typesys::ArrayType(type, len)

  return (type)
}

#' @export
constrain_ast.Complex = function(node, set) {
  type = typesys::ComplexType()
  len = length(node$value)
  if (len != 1)
    type = typesys::ArrayType(type, len)

  return (type)
}

#' @export
constrain_ast.Character = function(node, set) {
  type = typesys::CharacterType()
  len = length(node$value)
  if (len != 1)
    type = typesys::ArrayType(type, len)

  return (type)
}

#' @export
constrain_ast.default = function(node, set) {
  msg = sprintf("No type constraint defined for '%s'.", class(node)[1])
  stop(msg)
}
