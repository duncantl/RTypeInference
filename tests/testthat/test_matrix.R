# Description:
#   Tests of inference for matrices.

context("matrix")


test_that("matrix inferred when only nrow supplied", {
  expr = call("matrix", "yo", 3)

  type = infer_types(expr)$type

  expect_is(type, "ArrayType")
  expect_is(element_type(type), "CharacterType")
  expect_equal(dim(type), c(3, 1))
})


test_that("matrix type inferred", {
  expr = call("matrix", 3.1, 5, 7)

  type = infer_types(expr)$type

  expect_is(type, "ArrayType")
  expect_is(element_type(type), "RealType")
  expect_equal(dim(type), c(5, 7))
})

