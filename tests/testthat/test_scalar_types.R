# Description:
#   Tests of inference for scalar types.

test_that("logical types work", {
  type = inferTypes(TRUE)
  expect_is(type, "LogicalType")
})

test_that("integer types work", {
  type = inferTypes(-3L)
  expect_is(type, "IntegerType")
})

test_that("numeric types work", {
  type = inferTypes(3.17)
  expect_is(type, "NumericType")
})

test_that("complex types work", {
  type = inferTypes(1-12i)
  expect_is(type, "ComplexType")

  type = inferTypes(-3i)
  expect_is(type, "ComplexType")
})

test_that("character types work", {
  type = inferTypes("hi")
  expect_is(type, "CharacterType")
})
