
setOldClass("Counter")

setClass("RTypeInference::InferHelper", contains = "list",
  slots = list(
    counter = "Counter"
  ))

#' @export
InferHelper = function(counter = rstatic::Counter$new()) {
  new("RTypeInference::InferHelper", counter = counter)
}

#' @export
add_def = function(helper, name, type, is_parameter = FALSE) {
  # Check if name already has an entry.
  idx = match(name, names(helper), 0L)
  if (idx == 0L) {
    helper[[name]] = helper_record(def = type, is_parameter = is_parameter)

  } else {
    record = helper[[idx]]
    if (!is.null(record[["def"]]))
      stop(sprintf("definition type already set for variable '%s'.", name))

    record[["def"]] = type
    record[["is_parameter"]] = is_parameter
    helper[[idx]] = record
  }

  helper
}

#' @export
get_def = function(helper, name) {
  helper[[name]][["def"]]
}

#' @export
get_is_parameter = function(helper, name) {
  is_parameter = helper[[name]][["is_parameter"]]

  if (is.null(is_parameter)) NA else is_parameter
}

#' @export
add_use = function(helper, name, tvar) {
  idx = match(name, names(helper), 0L)
  if (idx == 0L) {
    helper[[name]] = helper_record(uses = list(tvar))

  } else {
    record = helper[[idx]]
    record[["uses"]] = append(record[["uses"]], tvar)
    helper[[idx]] = record
  }

  helper
}

#' @export
get_uses = function(helper, name) {
  # FIXME: Return list() when no entries for name?
  helper[[name]][["uses"]]
}


# Private Functions ----------------------------------------
new_variable = function(helper, prefix = "t", ...) {
  name = rstatic::next_name(helper@counter, name = prefix, ...)
  typesys::Variable(name)
}


helper_record = function(def = NULL, uses = list(), is_parameter = NA) {
  list(def = def, uses = uses, is_parameter = is_parameter)
}
