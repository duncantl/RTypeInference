#' Generate Type Constraint Set from Expression
#'
#' This function generates a set of type constraints from the given expression,
#' which can then be solved to infer types for the expression.
#' 
#' @param node (ASTNode) An expression from which to generate type constraints.
#' @param map (SymbolMap) A mapping between program variables and type
#' variables.
#'
#' @return An S4 `Result` object, with slots `contraints` and `map`. The former
#' is the list of constraints, and the latter is the mapping between program
#' variables and type variables.
#'
#' @export
constrain =
function(node, map = SymbolMap()) 
{
  c(type, constraints, map) := .constrain(node, list(), map)

  Result(constraints, map)
}


.constrain =
function(node, constraints, map)
{
  UseMethod(".constrain")
}


.constrain.BlockList =
function(node, constraints, map)
{
  # Let's try the naive thing and just call constrain on each block.

  # Process everything except the exit block.
  # TODO: Find a more elegant way to split off the exit block.
  blocks = node$contents[-node$exit_index]
  for (block in blocks)
    c(, constraints, map) := .constrain(block, constraints, map)

  # Process the exit block.
  exit = rstatic::exit_block(node)
  c(type, constraints, map) := .constrain(exit, constraints, map)

  list(type = type, constraints = constraints, map = map)
}


.constrain.Block =
function(node, constraints, map)
{
  for (phi in node$phi)
    c(, constraints, map) := .constrain(phi, constraints, map)

  for(exp in node$contents)
    c(type, constraints, map) := .constrain(exp, constraints, map)

  list(type = type, constraints = constraints, map = map)
}


.constrain.If =
function(node, constraints, map)
{
  c(type, constraints, map) :=
    .constrain(node$condition, constraints, map)

  # Constrain condition to be a logical value.
  con = typesys::Equivalence(type, typesys::RLogical)
  constraints = append(constraints, con)

  # TODO: An if-expression returns the union of the types at the end of each
  # branch. An if-statement returns no type (instead the types are captured in
  # phi-expressions).
  list(type = typesys::RNull, constraints = constraints, map = map)
}


.constrain.For =
function(node, constraints, map)
{
  # Generate constraints for the iterator.
  c(tdef, constraints, map) := .constrain(node$iterator, constraints,
    map)

  # Record generic type for the iteration variable.
  #
  # FIXME: The type should be an element of tdef, not tdef itself!
  map = set_defined_as(map, node$variable$ssa_name, tdef)

  list(type = tdef, constraints = constraints, map = map)
}


.constrain.Phi =
function(node, constraints, map)
{
  # Record use for each of the inputs.
  types = vector("list", length(node$contents))
  for (i in seq_along(node$contents)) {
    elt = node$contents[[i]]
    c(type, constraints, map) := .constrain(elt, constraints, map)
    types[[i]] = type
  }

  # Create a union from the input types.
  type = typesys::OneOf(types)

  # Now proceed like this is an Assign node.
  map = set_defined_as(map, node$write$ssa_name, type)

  list(type = type, constraints = constraints, map = map)
}


.constrain.Branch =
function(node, constraints, map)
{
  # Do nothing.
  list(type = typesys::RNull, constraints = constraints, map = map)
}


.constrain.Brace =
function(node, constraints, map)
{
  # Call .constrain on each element.
  for (exp in node$contents)
    c(type, constraints, map) := .constrain(exp, constraints, map)

  list(type = type, constraints = constraints, map = map)
}

.constrain.Call = function(node, constraints, map) {
  # Infer type for called function.
  # FIXME: What if called function is anonymous?
  c(t1, constraints, map) := .constrain(node$fn, constraints, map)

  # Infer types for arguments.
  targs = lapply(node$args$contents, function(exp) {
    c(targ, constraints, map) := .constrain(exp, constraints, map)
    constraints <<- constraints
    map <<- map
    targ
  })

  # Create new type variable for return.
  tvar = new_variable(map)

  # Add constraint t1 ~ (targs -> tvar)
  tfun = typesys::RFunction(targs, tvar)
  con = typesys::Equivalence(t1, tfun)
  constraints = append(constraints, con)

  list(type = tvar, constraints = constraints, map = map)
}


.constrain.Symbol =
function(node, constraints, map)
{
  # Create new type variable.
  tvar = new_variable(map)

  # Add to uses set.
  map = add_use(map, node$ssa_name, tvar)

  is_parameter = get_is_parameter(map, node$ssa_name)
  if (is.na(is_parameter)) {
    # Symbol does not correspond to a definition.
    # Do nothing.

  } else {
    tdef = get_defined_as(map, node$ssa_name)
    if (is_parameter) {
      # Symbol corresponds to a parameter, so add equivalence constraint.
      con = typesys::Equivalence(tvar, tdef)

    } else {
      # Symbol corresponds to a variable, so add instance constraint.
      if (length(typesys::vars(tdef)) == 0L) {
        # No variables in the RHS so just make an equality constraint.
        # We could check this case in the solver instead, but it's currently not
        # clear what the benefit would be.
        con = typesys::Equivalence(tvar, tdef)

      } else {
        # FIXME: Consider scopes when determining active parameters.
        # TODO: Make a separate function to get active parameters:
        is_parameter = vapply(map, `[[`, NA, "is_parameter")
        # Symbols with no known definition don't count as parameters (so
        # globals can be polymorphic).
        is_parameter = is_parameter & !is.na(is_parameter)
        m = lapply(map[is_parameter], `[[`, "def")

        con = typesys::ImplicitInstance(tvar, tdef, m)
      }
    }

    constraints = append(constraints, con)
  }

  list(type = tvar, constraints = constraints, map = map)
}


.constrain.Assign =
function(node, constraints, map)
{
  # Generate constraints for the definition.
  c(tdef, constraints, map) := .constrain(node$read, constraints, map)

  # Record generic type for variable.
  map = set_defined_as(map, node$write$ssa_name, tdef)

  list(type = tdef, constraints = constraints, map = map)
}


.constrain.Return = .constrain.Assign


.constrain.Function =
function(node, constraints, map)
{
  # Create a type variable for each parameter.
  tparams = lapply(node$params$contents, function(param) {
    tvar = new_variable(map)

    # FIXME: Helper might be tainted with outer scope variables. Need to keep
    # track of scopes somehow.
    map <<- set_defined_as(map, param$ssa_name, tvar,
      is_parameter = TRUE)

    tvar
  })

  # Infer types for the body.
  c(type, constraints, map) := .constrain(node$body, constraints, map)

  # TODO: Record parameter names.
  tfun = typesys::RFunction(tparams, type)

  # FIXME: Remove parameters from the active set.
  for (p in node$params$contents) {
    map = remove_entry(map, p$ssa_name)
  }

  list(type = tfun, constraints = constraints, map = map)
}


# Literals ----------------------------------------

.constrain.Character =
function(node, constraints, map)
{
  list(type = typesys::RString, constraints = constraints, map = map)
}


.constrain.Numeric =
function(node, constraints, map)
{
  list(type = typesys::RNumeric, constraints = constraints, map = map)
}


.constrain.Integer =
function(node, constraints, map)
{
  list(type = typesys::RInteger, constraints = constraints, map = map)
}


.constrain.Logical =
function(node, constraints, map)
{
  list(type = typesys::RLogical, constraints = constraints, map = map)
}
