# Description:
#   Tests of inference for rnorm (and other type-unstable functions).

context("conditional types")

test_that("rnorm typed as scalar when n = 1", {
  expr = call("rnorm", 1)

  type = infer_types(expr)$type

  expect_is(type, "RealType")
})

test_that("rnorm typed as vector when n > 1", {
  len = 10
  expr = call("rnorm", len)

  type = infer_types(expr)$type

  expect_is(type, "ArrayType")
  expect_is(element_type(type), "RealType")
  expect_equal(length(type), len)
})

test_that("rnorm typed as vector with missing length when n unknown", {
  expr = call("rnorm", as.name("x"))

  type = infer_types(expr)$type

  expect_is(type, "ArrayType")
  expect_is(element_type(type), "RealType")
  expect_equal(length(type), NA_integer_)
})
