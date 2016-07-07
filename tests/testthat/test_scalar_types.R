# Description:
#   Tests of inference for scalar types.

context("scalars")


test_that("logical types work", {
  type = .infer_types(TRUE)
  expect_is(type, "BooleanType")
})


test_that("integer types work", {
  type = .infer_types(-3L)
  expect_is(type, "IntegerType")
})


test_that("numeric types work", {
  type = .infer_types(3.17)
  expect_is(type, "RealType")
})


test_that("complex types work", {
  type = .infer_types(1-12i)
  expect_is(type, "ComplexType")

  type = .infer_types(-3i)
  expect_is(type, "ComplexType")
})


test_that("character types work", {
  type = .infer_types("hi")
  expect_is(type, "CharacterType")
})
