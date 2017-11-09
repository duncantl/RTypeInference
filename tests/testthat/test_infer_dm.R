
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

  env = typesys::TypeEnvironment$new(
    "+" = typesys::FunctionType(
      list(typesys::TypeVar("a"), typesys::TypeVar("b")),
      typesys::TypeVar("a"))
    )

  result = infer_dm(node, env, top = TRUE)

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

#test_that("polymorphism", {
#  node = rstatic::quote_cfg({
#    # identity: a ~ a
#    f = function(x) x
#    # identity: Integer ~ Integer
#    f(1L)
#    # returns Integer
#  })
#
#  infer_dm(node, top = TRUE)
#})
#
#test_that("polymorphism2", {
#  node = rstatic::quote_cfg({
#    # identity: a ~ a
#    f = function(x, y) x
#    # identity: Integer ~ Integer
#    f(1L, 2L)
#    # returns Integer
#  })
#
#  infer_dm(node, top = TRUE)
#})
#
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
