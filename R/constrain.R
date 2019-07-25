# Implementation of Heeren et al. variant of Damas-Milner.
#
# Uses typesys 0.3 and latest version of rstatic.


#' Generate Type Constraint Set from Expression
#'
#' This function generates a set of type constraints from the given expression,
#' which can then be solved to infer types for the expression.
#' 
#' @param node ASTNode. Expression from which to generate type constraints.
#' @param helper InferHelper. Helper object for constraint generation.
#'
#' @return A list with two elements. The first is the list of constraints, and
#' the second is the final state of the helper object.
#'
#' @export
constrain =
function(node
  , helper = InferHelper()
  ) 
{
  c(type, constraints, helper) := .constrain(node, list(), helper)

  list(constraints = constraints, helper = helper)
}


.constrain =
function(node, constraints, helper)
{
  UseMethod(".constrain")
}


.constrain.BlockList =
function(node, constraints, helper)
{
  # Let's try the naive thing and just call constrain on each block.

  # Process everything except the exit block.
  # TODO: Find a more elegant way to split off the exit block.
  blocks = node$contents[-node$exit_index]
  for (block in blocks)
    c(, constraints, helper) := .constrain(block, constraints, helper)

  # Process the exit block.
  exit = rstatic::exit_block(node)
  c(type, constraints, helper) := .constrain(exit, constraints, helper)

  list(type = type, constraints = constraints, helper = helper)
}


.constrain.Block =
function(node, constraints, helper)
{
  for (phi in node$phi)
    c(, constraints, helper) := .constrain(phi, constraints, helper)

  for(exp in node$contents)
    c(type, constraints, helper) := .constrain(exp, constraints, helper)

  list(type = type, constraints = constraints, helper = helper)
}


.constrain.If =
function(node, constraints, helper)
{
  c(type, constraints, helper) :=
    .constrain(node$condition, constraints, helper)

  # Constrain condition to be a logical value.
  con = typesys::Equivalence(type, typesys::RLogical)
  constraints = append(constraints, con)

  # TODO: An if-expression returns the union of the types at the end of each
  # branch. An if-statement returns no type (instead the types are captured in
  # phi-expressions).
  list(type = typesys::RNull, constraints = constraints, helper = helper)
}


.constrain.For =
function(node, constraints, helper)
{
  # Generate constraints for the iterator.
  c(tdef, constraints, helper) := .constrain(node$iterator, constraints,
    helper)

  # Record generic type for the iteration variable.
  #
  # FIXME: The type should be an element of tdef, not tdef itself!
  helper = add_def(helper, node$variable$ssa_name, tdef)

  list(type = tdef, constraints = constraints, helper = helper)
}


.constrain.Phi =
function(node, constraints, helper)
{
  # Record use for each of the inputs.
  types = vector("list", length(node$contents))
  for (i in seq_along(node$contents)) {
    elt = node$contents[[i]]
    c(type, constraints, helper) := .constrain(elt, constraints, helper)
    types[[i]] = type
  }

  # Create a union from the input types.
  type = typesys::OneOf(types)

  # Now proceed like this is an Assign node.
  helper = add_def(helper, node$write$ssa_name, type)

  list(type = type, constraints = constraints, helper = helper)
}


.constrain.Branch =
function(node, constraints, helper)
{
  # Do nothing.
  list(type = typesys::RNull, constraints = constraints, helper = helper)
}


.constrain.Brace = function(node, constraints, helper) {
  # Call .constrain on each element.
  for (exp in node$contents)
    c(type, constraints, helper) := .constrain(exp, constraints, helper)

  list(type = type, constraints = constraints, helper = helper)
}

.constrain.Call = function(node, constraints, helper) {
  # Infer type for called function.
  # FIXME: What if called function is anonymous?
  c(t1, constraints, helper) := .constrain(node$fn, constraints, helper)

  # Infer types for arguments.
  targs = lapply(node$args$contents, function(exp) {
    c(targ, constraints, helper) := .constrain(exp, constraints, helper)
    constraints <<- constraints
    helper <<- helper
    targ
  })

  # Create new type variable for return.
  tvar = new_variable(helper)

  # Add constraint t1 ~ (targs -> tvar)
  tfun = typesys::RFunction(targs, tvar)
  con = typesys::Equivalence(t1, tfun)
  constraints = append(constraints, con)

  list(type = tvar, constraints = constraints, helper = helper)
}

.constrain.Symbol = function(node, constraints, helper) {
  # Create new type variable.
  tvar = new_variable(helper)

  # Add to uses set.
  helper = add_use(helper, node$ssa_name, tvar)

  is_parameter = get_is_parameter(helper, node$ssa_name)
  if (is.na(is_parameter)) {
    # Symbol does not correspond to a definition.
    # Do nothing.

  } else {
    if (is_parameter) {
      # Symbol corresponds to a parameter, so add equivalence constraint.
      tdef = get_def(helper, node$ssa_name)
      con = typesys::Equivalence(tvar, tdef)

    } else {
      # Symbol corresponds to a variable, so add instance constraint.
      tdef = get_def(helper, node$ssa_name)
      if (length(typesys::vars(tdef)) == 0L) {
        # No variables in the RHS so just make an equality constraint.
        # We could check this case in the solver instead, but it's currently not
        # clear what the benefit would be.
        con = typesys::Equivalence(tvar, tdef)

      } else {
        # FIXME: Consider scopes when determining active parameters.
        # TODO: Make a separate function to get active parameters:
        is_parameter = vapply(helper, `[[`, NA, "is_parameter")
        # Symbols with no known definition don't count as parameters (so
        # globals can be polymorphic).
        is_parameter = is_parameter & !is.na(is_parameter)
        m = lapply(helper[is_parameter], `[[`, "def")

        con = typesys::ImplicitInstance(tvar, tdef, m)
      }
    }

    constraints = append(constraints, con)
  }

  list(type = tvar, constraints = constraints, helper = helper)
}


.constrain.Assign = function(node, constraints, helper) {
  # Generate constraints for the definition.
  c(tdef, constraints, helper) := .constrain(node$read, constraints, helper)

  # Record generic type for variable.
  helper = add_def(helper, node$write$ssa_name, tdef)

  list(type = tdef, constraints = constraints, helper = helper)
}


.constrain.Return = .constrain.Assign


.constrain.Function = function(node, constraints, helper) {
  # Create a type variable for each parameter.
  tparams = lapply(node$params$contents, function(param) {
    tvar = new_variable(helper)

    # FIXME: Helper might be tainted with outer scope variables. Need to keep
    # track of scopes somehow.
    helper <<- add_def(helper, param$ssa_name, tvar, is_parameter = TRUE)

    tvar
  })

  # Infer types for the body.
  c(type, constraints, helper) := .constrain(node$body, constraints, helper)

  # TODO: Record parameter names.
  tfun = typesys::RFunction(tparams, type)

  # FIXME: Remove parameters from the active set.
  for (p in node$params$contents) {
    helper = rm_def(helper, p$ssa_name)
  }

  list(type = tfun, constraints = constraints, helper = helper)
}


# Literals ----------------------------------------

.constrain.Character = function(node, constraints, helper) {
  list(type = typesys::RString, constraints = constraints, helper = helper)
}

.constrain.Numeric = function(node, constraints, helper) {
  list(type = typesys::RNumeric, constraints = constraints, helper = helper)
}

.constrain.Integer = function(node, constraints, helper) {
  list(type = typesys::RInteger, constraints = constraints, helper = helper)
}

.constrain.Logical = function(node, constraints, helper) {
  list(type = typesys::RLogical, constraints = constraints, helper = helper)
}
