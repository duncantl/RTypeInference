
context("infer_dm")

test_that("Variable assigned literal", {
  node = rstatic::quote_cfg(x <- 5L)

  result = infer_dm(node, top = TRUE)

  # -----
  expect_equal(length(result$env), 2)
  expect_is(result$env[["x_1"]], "typesys::IntegerType")
})

test_that("Variable assigned variable", {
  node = rstatic::quote_cfg({
    x = 5L
    y = x
  })

  result = infer_dm(node, top = TRUE)

  # -----
  expect_equal(length(result$env), 3)
  expect_is(result$env[["x_1"]], "typesys::IntegerType")
  expect_is(result$env[["y_1"]], "typesys::IntegerType")
})


test_that("Variable assigned call", {
  node = rstatic::quote_cfg({
    x = 5L
    y = x + 1L
  })

  tenv = typesys::TypeEnvironment$new(
    "+" = c(a, b) ~ Join(a, b, Integer),
    quantify = TRUE
  )

  result = infer_dm(node, tenv, top = TRUE)

  # -----
  expect_equal(length(result$env), 3)
  expect_is(result$env[["x_1"]], "typesys::IntegerType")
  expect_is(result$env[["y_1"]], "typesys::IntegerType")
})


test_that("Variable assigned monomorphic function", {
  node = rstatic::quote_cfg({
    f = function() 42L
    f
  })
  
  result = infer_dm(node, top = TRUE)

  # -----
  expect_equal(length(result$env), 2)
  f_type = result$env[["f_1"]]
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

  result = infer_dm(node, top = TRUE)

  # -----
  expect_equal(length(result$env), 2)
  f_type = result$env[["f_1"]]
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

  result = infer_dm(node, top = TRUE)

  # -----
  expect_equal(length(result$env), 4)

  f_type = result$env[["f_1"]]
  expect_is(f_type, "typesys::FunctionType")
  expect_equal(length(f_type@args), 1)
  # TODO: Equality for types.
  # expect_equal(f_type@args[[1]], f_type@return_type)
  expect_is(f_type@return_type, "typesys::TypeVar")

  expect_is(result$env[["x_1"]], "typesys::IntegerType")
  expect_is(result$env[["y_1"]], "typesys::RealType")
})


test_that("Parameter inference", {
  node = rstatic::quote_cfg(
    function(x, y) f(x)
  )

  tenv = typesys::TypeEnvironment$new(
    "f" = Integer ~ Boolean
  )

  result = infer_dm(node, tenv, top = TRUE)

  # -----
  type = result$type
  expect_is(type@args[["x_1"]], "typesys::IntegerType")
  expect_is(type@args[["y_1"]], "typesys::TypeVar")
  expect_is(type@return_type, "typesys::BooleanType")
})


test_that("Complicated parameter inference", {
  node = rstatic::quote_cfg(
    function(x) {
      y = f(x)
      g(y)
    }
  )

  tenv = typesys::TypeEnvironment$new(
    "f" = a ~ a,
    "g" = Integer ~ Boolean,
    quantify = TRUE
  )

  result = infer_dm(node, tenv, top = TRUE)

  # -----
  type = result$type
  expect_is(type@args[["x_1"]], "typesys::IntegerType")
  expect_is(type@return_type, "typesys::BooleanType")
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
