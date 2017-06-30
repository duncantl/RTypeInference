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
  , scalar = FALSE
) {

  if (missing(b)) {
    b = cfg$get_index(cfg$entry)

    # TODO: Generate constraints for parameters in caller, before recursion.
    if (length(cfg$params) > 0) {
                                        # "_1"
      given = (paste0(names(cfg$params), "") %in% sapply(set$constraints, `[[`, 1))
      lapply(cfg$params[!given], constrain_ast, set, scalar = scalar)
    }
  }

  block = cfg[[b]]

  # Iterate over Phi nodes.
  # Duncan - what are the Phi nodes here?
  lapply(block$phi, constrain_ast, set, scalar = scalar)

  # Iterate over body, generating type constraints.
  lapply(block$body, constrain_ast, set, scalar = scalar)

  # Descend to next blocks.
  children = setdiff(which(dom_t == b), b)
  lapply(children, function(i) constrain(cfg, i, dom_t, set, scalar = scalar))

  return (set)
}


#' @export
constrain_ast = function(node, set, scalar = FALSE, ...) {
  UseMethod("constrain_ast")
}

#' @export
constrain_ast.Assign = function(node, set, scalar = FALSE, ...) {
  type = constrain_ast(node$read, set)

  set$append(node$write$name, type)

  return (type)
}

#' @export
constrain_ast.Parameter = function(node, set, scalar = FALSE, ...) {
  if (is.null(node$default))
    return (NULL)

  type = constrain_ast(node$default, set)

  set$append(node$name, type)
  
  return(type)
}

#' @export
constrain_ast.Phi = function(node, set, scalar = FALSE, ...) {
  # FIXME: Check type of the read; it could be a non-Symbol.
  reads = lapply(node$read, function(read) read$name)
  rhs = do.call(typesys::Union, reads)
  set$append(node$write$name, rhs)
}

#' @export
constrain_ast.Call = function(node, set, scalar = FALSE, ...) {
  args = lapply(node$args, constrain_ast, set)
  # FIXME: Nested calls might be a problem here. Need to generate a temporary
  # value for each call.

  # FIXME: Infer return type immediately if types are known for all arguments.
  # ...

    # XXX This is temporarily here.  We'll add customization handlers for this.
  fn = node$fn$name
                               #XXX remove "g" - just testing.
  if((fn %in% c("numeric", "integer", "logical", "character", "runif", "rexp", "g")) &&
       is(node$args[[1]], "Symbol") ) {
       # Need to match the argument by name for other functions, eg. runif(n, 1, 2)
       # but  runif(1, 2, n = n1)
      set$append(node$args[[1]]$name, typesys::IntegerType())
  } else if(fn %in% c(":")) {
      sapply(node$args[1:2],
              function(x)
               if(is(x,"Symbol"))
                 set$append(x$name, typesys::IntegerType()))
  }
  
  # FIXME: anonymous functions
  # Defer inference to the resolution step.
  do.call(typesys::Call, append(node$fn$name, args))
}

#' @export
constrain_ast.Replacement = function(node, set, scalar = FALSE, ...) {
  warning("Constraints are not generated for replacements.")
  #browser()
}

#' @export
constrain_ast.Symbol = function(node, set, scalar = FALSE, ...) {
  node$name
}

#' @export
constrain_ast.Null = function(node, set, scalar = FALSE, ...) {
  typesys::NullType()
}

#' @export
constrain_ast.Logical = function(node, set, scalar = FALSE, ...) {
  type = typesys::BooleanType()
  # FIXME:
  len = length(node$value)
  if (len != 1)
    type = typesys::ArrayType(type, len)

  return (type)
}

#' @export
constrain_ast.Integer = function(node, set, scalar = FALSE, ...) {
  type = typesys::IntegerType()
  len = length(node$value)
  if (len != 1)
    type = typesys::ArrayType(type, len)

  type@value = node

  return (type)
}

#' @export
constrain_ast.Numeric = function(node, set, scalar = FALSE, ...) {
  type = typesys::RealType()
  len = length(node$value)
  if (len != 1)
    type = typesys::ArrayType(type, len)

  return (type)
}

#' @export
constrain_ast.Complex = function(node, set, scalar = FALSE, ...) {
  type = typesys::ComplexType()
  len = length(node$value)
  if (len != 1)
    type = typesys::ArrayType(type, len)

  return (type)
}

#' @export
constrain_ast.Character = function(node, set, scalar = FALSE, ...) {
  type = typesys::StringType()
  len = length(node$value)
  if (len != 1)
    type = typesys::ArrayType(type, len)

  return (type)
}

constrain_ast.Brace =
function(node, set, scalar = FALSE, ...) {
    #XXX - not just first element - or will there only ever be one
  if(length(node$body) > 1)
     warning("constrain_ast.Brace - ignoring other elements of body")    
  constrain_ast(node$body[[1]], set, scalar, ...)
}
    
#' @export
constrain_ast.default = function(node, set, scalar = FALSE, ...) {
  msg = sprintf("No type constraint defined for '%s'.", class(node)[1])
  stop(msg)
}
