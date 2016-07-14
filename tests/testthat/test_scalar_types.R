# Description:
#   Tests of inference for scalar types.

context("scalars")


test_that("logical types work", {
  type = infer_types(TRUE)$type
  expect_is(type, "BooleanType")
})


test_that("integer types work", {
  type = infer_types(-3L)$type
  expect_is(type, "IntegerType")
})


test_that("numeric types work", {
  type = infer_types(3.17)$type
  expect_is(type, "RealType")
})


test_that("complex types work", {
  type = infer_types(1-12i)$type
  expect_is(type, "ComplexType")

  type = infer_types(-3i)$type
  expect_is(type, "ComplexType")
})


test_that("character types work", {
  type = infer_types("hi")$type
  expect_is(type, "CharacterType")
})
