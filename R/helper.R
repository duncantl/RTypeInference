
setOldClass("Counter")

setClass("RTypeInference::InferHelper", contains = "list",
  slots = list(
    counter = "Counter"
  ))

#' @export
InferHelper = function(counter = rstatic::Counter$new()) {
  new("RTypeInference::InferHelper", counter = counter)
}

add_def = function(helper, name, type, is_parameter = FALSE) {
  # Check if name already has an entry.
  idx = match(name, names(helper), 0L)
  if (idx == 0L) {
    helper[[name]] = helper_record(defined_as = type
      , is_parameter = is_parameter)

  } else {
    record = helper[[idx]]
    if (!is.null(record[["defined_as"]]))
      stop(sprintf("definition type already set for variable '%s'.", name))

    record[["defined_as"]] = type
    record[["is_parameter"]] = is_parameter
    helper[[idx]] = record
  }

  helper
}

rm_def = function(helper, name) {
  idx = match(name, names(helper), 0L)
  if (idx == 0L)
    return (helper)

  new_names = names(helper)[-idx]
  helper@.Data = helper@.Data[-idx]
  names(helper) = new_names

  helper
}

get_def = function(helper, name) {
  helper[[name]][["defined_as"]]
}

get_is_parameter = function(helper, name) {
  is_parameter = helper[[name]][["is_parameter"]]

  if (is.null(is_parameter)) NA else is_parameter
}

add_use = function(helper, name, tvar) {
  idx = match(name, names(helper), 0L)
  if (idx == 0L) {
    # Assume this is not a parameter, since parameters typically get added
    # first.
    helper[[name]] = helper_record(used_as = list(tvar))

  } else {
    record = helper[[idx]]
    record[["used_as"]] = append(record[["used_as"]], tvar)
    helper[[idx]] = record
  }

  helper
}

get_uses = function(helper, name) {
  # FIXME: Return list() when no entries for name?
  helper[[name]][["used_as"]]
}


new_variable = function(helper, prefix = "t", ...) {
  name = rstatic::next_name(helper@counter, name = prefix, ...)
  typesys::Variable(name)
}


helper_record =
function(
  defined_as = NULL
  , used_as = list()
  , is_parameter = NA)
{
  list(defined_as = defined_as, used_as = used_as, is_parameter = is_parameter)
}
