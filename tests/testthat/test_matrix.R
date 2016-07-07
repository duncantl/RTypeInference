# Description:
#   Tests of inference for matrices.

context("matrix")


test_that("matrix inferred when only nrow supplied", {
  expression = call("matrix", "yo", 3)

  result = .infer_types(expression)
  expect_is(result, "ArrayType")
  expect_is(element_type(result), "CharacterType")
  expect_equal(dim(result), c(3, 1))
})


test_that("matrix type inferred", {
  expression = call("matrix", 3.1, 5, 7)

  result = .infer_types(expression)
  expect_is(result, "ArrayType")
  expect_is(element_type(result), "RealType")
  expect_equal(dim(result), c(5, 7))
})

