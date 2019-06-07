
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
add_def = function(helper, name, type) {
  # Check if name already has an entry.
  idx = match(name, names(helper), 0L)
  if (idx == 0L) {
    helper[[name]] = list(def = type, uses = list())

  } else {
    record = helper[[idx]]
    if (!is.null(record[["def"]]))
      stop(sprintf("definition type already set for variable '%s'.", name))

    record[["def"]] = type
    helper[[idx]] = record
  }

  helper
}

#' @export
get_def = function(helper, name) {
  helper[[name]][["def"]]
}

#' @export
add_use = function(helper, name, tvar) {
  idx = match(name, names(helper), 0L)
  if (idx == 0L) {
    helper[[name]] = list(def = NULL, uses = list(tvar))

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

#' @export
new_name = function(helper, prefix = "t") {
  paste0(prefix, helper@counter$increment(prefix))
}
