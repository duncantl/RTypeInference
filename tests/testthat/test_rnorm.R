# Description:
#   Tests of inference for rnorm (and other type-unstable functions).

test_that("rnorm typed as scalar when n = 1", {
  expr = call("rnorm", 1)

  type = inferTypes(expr)
  expect_is(type, "NumericType")
})

test_that("rnorm typed as vector when n > 1", {
  len = 10
  expr = call("rnorm", len)

  type = inferTypes(expr)
  expect_is(type, "VectorType")
  expect_is(atomicType(type), "NumericType")
  expect_equal(length(type), len)
})

test_that("rnorm typed as vector with missing length when n unknown", {
  expression = call("rnorm", as.name("x"))

  result = inferTypes(expression)
  expect_is(result, "VectorType")
  expect_is(atomicType(result), "NumericType")
  expect_equal(length(result), NA_integer_)
})
