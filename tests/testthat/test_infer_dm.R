context("infer_dm")


test_that("Variable assigned literal", {
  node = rstatic::quote_cfg(x <- 5L)

  result = infer_dm(node)

  # -----
  expect_is(result, "typesys::FunctionType")
  tenv = result@type_environment
  expect_equal(length(tenv), 2)
  expect_is(tenv[["x_1"]], "typesys::IntegerType")
})


test_that("Variable assigned variable", {
  node = rstatic::quote_cfg({
    x = 5L
    y = x
  })

  result = infer_dm(node)

  # -----
  tenv = result@type_environment
  expect_equal(length(tenv), 3)
  expect_is(tenv[["x_1"]], "typesys::IntegerType")
  expect_is(tenv[["y_1"]], "typesys::IntegerType")
})


test_that("Variable assigned call", {
  node = rstatic::quote_cfg({
    x = 5L
    y = x + 1L
  })

  global_tenv = typesys::TypeEnvironment$new(
    "+" = c(a, b) ~ Join(a, b, Integer),
    quantify = TRUE
  )

  result = infer_dm(node, global_tenv)

  # -----
  tenv = result@type_environment
  expect_equal(length(tenv), 3)
  expect_is(tenv[["x_1"]], "typesys::IntegerType")
  expect_is(tenv[["y_1"]], "typesys::IntegerType")
})


test_that("Variable assigned monomorphic function", {
  node = rstatic::quote_cfg({
    f = function() 42L
    f
  })
  
  result = infer_dm(node)

  # -----
  tenv = result@type_environment

  expect_equal(length(tenv), 2)
  f_type = tenv[["f_1"]]
  expect_is(f_type, "typesys::FunctionType")
  # NOTE: Should the args be an empty list or an empty RecordType?
  expect_is(f_type@args, "list")
  expect_equal(length(f_type@args), 0)
  expect_is(f_type@return_type, "typesys::IntegerType")
})


test_that("Variable assigned polymorphic function", {
  node = rstatic::quote_cfg({
    f = function(x) x
    f
  })

  result = infer_dm(node)

  # -----
  tenv = result@type_environment
  expect_equal(length(tenv), 2)
  f_type = tenv[["f_1"]]
  expect_is(f_type, "typesys::FunctionType")
  expect_equal(length(f_type@args), 1)
  # TODO: Equality for types.
  # expect_equal(f_type@args[[1]], f_type@return_type)
  expect_is(f_type@return_type, "typesys::TypeVar")
})


test_that("Polymorphic function can be instantiated", {
  node = rstatic::quote_cfg({
    f = function(x) x
    x = f(3L)
    y = f(3.1)
  })

  result = infer_dm(node)

  # -----
  tenv = result@type_environment
  expect_equal(length(tenv), 4)

  f_type = tenv[["f_1"]]
  expect_is(f_type, "typesys::FunctionType")
  expect_equal(length(f_type@args), 1)
  # TODO: Equality for types.
  # expect_equal(f_type@args[[1]], f_type@return_type)
  expect_is(f_type@return_type, "typesys::TypeVar")

  expect_is(tenv[["x_1"]], "typesys::IntegerType")
  expect_is(tenv[["y_1"]], "typesys::RealType")
})


test_that("Parameter inference", {
  node = rstatic::quote_cfg(
    function(x, y) f(x)
  )

  global_tenv = typesys::TypeEnvironment$new(
    "f" = Integer ~ Boolean
  )

  result = infer_dm(node, global_tenv)

  # -----
  expect_is(result@args[["x_1"]], "typesys::IntegerType")
  expect_is(result@args[["y_1"]], "typesys::TypeVar")
  expect_is(result@return_type, "typesys::BooleanType")
})


test_that("Complicated parameter inference", {
  node = rstatic::quote_cfg(
    function(x) {
      y = f(x)
      g(y)
    }
  )

  global_tenv = typesys::TypeEnvironment$new(
    "f" = a ~ a,
    "g" = Integer ~ Boolean,
    quantify = TRUE
  )

  result = infer_dm(node, global_tenv)

  # -----
  expect_is(result@args[["x_1"]], "typesys::IntegerType")
  expect_is(result@return_type, "typesys::BooleanType")
})


test_that("Branch assignment with equal types", {
  node = rstatic::quote_cfg(
    function() {
      x = "hi"
      if (TRUE)
        x = 4
      else
        x = 5
      y = x
    }
  )

  result = infer_dm(node)

  # -----
  expect_is(result@return_type, "typesys::RealType")
})


test_that("Branch assignment with different types", {
  node = rstatic::quote_cfg(
    function() {
      x = "hi"
      if (TRUE)
        x = 4
      else
        x = "hi"
      y = x
    }
  )

  # -----
  expect_error(infer_dm(node), "Cannot unify[.]")
})


#test_that("optional arguments", {
#  node = rstatic::quote_cfg({
#    # f: (x: a, y: b) ~ a
#    f = function(x, y = 1) x
#    # returns 1
#    f(1)
#  })
#
#  result = infer_dm(node, top = T)
#}
