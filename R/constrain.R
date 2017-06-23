# Description:
#   Functions to generate type constraints from a control-flow graph.

#' Compute Type Constraints from a Control-flow Graph
#'
#'
#' @export
constrain = function(cfg
  , b
  , dom_t = rstatic::dom_tree(cfg)
  , set = ConstraintSet$new()
) {

  if (missing(b)) {
    b = cfg$get_index(cfg$entry)

    # TODO: Generate constraints for parameters in caller, before recursion.
    if (!is.null(cfg$params)) {
      given = (names(cfg$params) %in% sapply(set$constraints, `[[`, 1))
      lapply(cfg$params[!given], constrain_ast, set)
    }
  }

  block = cfg[[b]]

  # Iterate over Phi nodes.
  lapply(block$phi, constrain_ast, set)

  # Iterate over body, generating type constraints.
  lapply(block$body, constrain_ast, set)

  # Descend to next blocks.
  children = setdiff(which(dom_t == b), b)
  lapply(children, function(i) constrain(cfg, i, dom_t, set))

  return (set)
}


#' @export
constrain_ast = function(node, set) {
  UseMethod("constrain_ast")
}

#' @export
constrain_ast.Assign = function(node, set) {
  type = constrain_ast(node$read, set)

  set$append(node$write$name, type)

  return (type)
}

#' @export
constrain_ast.Parameter = function(node, set) {
  if (is.null(node$default))
    return (NULL)

  type = constrain_ast(node$default, set)

  set$append(node$name, type)
  
  return(type)
}

#' @export
constrain_ast.Phi = function(node, set) {
  # FIXME: Check type of the read; it could be a non-Symbol.
  reads = lapply(node$read, function(read) read$name)
  rhs = do.call(typesys::Union, reads)
  set$append(node$write$name, rhs)
}

#' @export
constrain_ast.Call = function(node, set) {
  args = lapply(node$args, constrain_ast, set)
  # FIXME: Nested calls might be a problem here. Need to generate a temporary
  # value for each call.

  # FIXME: Infer return type immediately if types are known for all arguments.
  # ...

    # XXX This is temporarily here.  We'll add customization handlers for this.
  fn = node$fn$name
  if((fn %in% c("numeric", "integer", "logical", "character")) &&
       is(node$args[[1]], "Symbol") ) {
      set$append(node$args[[1]]$name, typesys::IntegerType())
  }  
  
  # FIXME: anonymous functions
  # Defer inference to the resolution step.
  do.call(typesys::Call, append(node$fn$name, args))
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

  type@value = node

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
  type = typesys::StringType()
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
