
setOldClass("Counter")

setClass("RTypeInference::SymbolMap", contains = "list",
  slots = list(
    counter = "Counter"
  ))

#' @export
SymbolMap = function(counter = rstatic::Counter$new()) {
  new("RTypeInference::SymbolMap", counter = counter)
}


#' Get All Parameters in a SymbolMap
#'
#' This function gets all entries in a `SymbolMap` that correspond to function
#' parameters.
#'
#' @param map (SymbolMap) The map to search for parameter entries.
#' @param include_na (logical) Should entries with `is_parameter = NA` be
#' included in the result?
#'
#' @return A list of entries.
#'
#' @export
get_parameters =
function(map, include_na = FALSE)
{
  is_parameter = vapply(map, `[[`, NA, "is_parameter")

  if (include_na)
    is_parameter = is_parameter | is.na(is_parameter)
  else
    is_parameter = is_parameter & !is.na(is_parameter)

  map[is_parameter]
}


#' Set Type a Symbol Is Defined As in a SymbolMap
#'
#' This function sets the `defined_as` field in a `SymbolMap` entry.
#'
#' @param map (SymbolMap) A map to modify.
#' @param name (character) The symbol whose entry should be modified.
#' @param type (Term | formula) The new 'defined as' type. Formulas are
#' automatically converted to types with [typesys::formula_to_type()].
#' @param is_parameter (logical) Is this symbol a parameter?
#'
#' @return The updated `SymbolMap`.
#' @examples
#' map = SymbolMap()
#'
#' # Set 'x' defined as an Integer.
#' map = set_defined_as(map, "x", RInteger)
#'
#' # Set 'length' defined as a function from ANY to Numeric.
#' #
#' # Type variables in the type indicate polymorphism. In this example, 'b' can
#' # be replaced by any type at each call site for 'length'.
#' map = set_defined_as(map, "length", b ~ RNumeric)
#' @export
set_defined_as =
function(map, name, type, is_parameter = FALSE)
{
  if (inherits(type, "formula"))
    type = typesys::formula_to_type(type)

  # Check if name already has an entry.
  idx = match(name, names(map), 0L)
  if (idx == 0L) {
    map[[name]] = SymbolMapEntry(defined_as = type
      , is_parameter = is_parameter)

  } else {
    entry = map[[idx]]
    if (!is.null(entry[["defined_as"]]))
      warning(sprintf("defined_as already set for symbol '%s'.", name))

    entry[["defined_as"]] = type
    entry[["is_parameter"]] = is_parameter
    map[[idx]] = entry
  }

  map
}


#' @export
get_defined_as =
function(map, name)
{
  map[[name]][["defined_as"]]
}


#' @export
remove_entry =
function(map, name)
{
  idx = match(name, names(map), 0L)
  if (idx == 0L)
    return (map)

  new_names = names(map)[-idx]
  map@.Data = map@.Data[-idx]
  names(map) = new_names

  map
}




get_is_parameter = function(map, name) {
  is_parameter = map[[name]][["is_parameter"]]

  if (is.null(is_parameter)) NA else is_parameter
}


add_use = function(map, name, tvar) {
  idx = match(name, names(map), 0L)
  if (idx == 0L) {
    # Assume this is not a parameter, since parameters typically get added
    # first.
    map[[name]] = SymbolMapEntry(used_as = list(tvar))

  } else {
    entry = map[[idx]]
    entry[["used_as"]] = append(entry[["used_as"]], tvar)
    map[[idx]] = entry
  }

  map
}

get_uses = function(map, name) {
  # FIXME: Return list() when no entries for name?
  map[[name]][["used_as"]]
}


new_variable = function(map, prefix = "t", ...) {
  name = rstatic::next_name(map@counter, name = prefix, ...)
  typesys::Variable(name)
}


#' @export
SymbolMapEntry =
function(
  defined_as = NULL
  , used_as = list()
  , is_parameter = NA)
{
  list(defined_as = defined_as, used_as = used_as, is_parameter = is_parameter)
}
