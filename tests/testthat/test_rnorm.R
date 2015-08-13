# Description:
#   Tests of inference for rnorm (and other type-unstable functions).

test_that("rnorm returns scalar works", {
  expr = call("rnorm", 1)

  type = inferTypes(expr)
  expect_is(type, "NumericType")
})

test_that("rnorm returns vector works", {
  len = 10
  expr = call("rnorm", len)

  type = inferTypes(expr)
  expect_is(type, "VectorType")
  expect_is(atomicType(type), "NumericType")
  expect_equal(length(type), len)
})

test_that("rnorm gives ConditionalType on unknown arguments", {
  expr = call("rnorm", as.name("x"))

  type = inferTypes(expr)
  expect_is(type, "ConditionalType")
})
