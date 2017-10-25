
instantiate = function(x, counter) {
  UseMethod("instantiate")
}

setGeneric("instantiate", valueClass = "typesys::Type")


`instantiate.typesys::TypeVar` = function(x, counter) {
  if (x@quantified)
    TypeVar(sprintf("%s.%i", x@name, counter$increment(x@name)))
  else
    x
}

setMethod("instantiate", "typesys::TypeVar", `instantiate.typesys::TypeVar`)


`instantiate.typesys::FunctionType` = function(x, counter) {
  x@args = lapply(x@args, instantiate)
  x@return_type = instantiate(x@return_type)

  x
}

setMethod("instantiate", "typesys::FunctionType",
  `instantiate.typesys::FunctionType`)


`instantiate.typesys::AtomicType` = function(x, counter) x

setMethod("instantiate", "typesys::AtomicType",
  `instantiate.typesys::AtomicType`)
