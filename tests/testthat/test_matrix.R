# Description:
#   Tests of inference for matrices.

test_that("matrix inferred when only nrow supplied", {
  expression = call("matrix", "yo", 3)

  result = inferTypes(expression)
  expect_is(result, "VectorType")
  expect_is(atomicType(result), "CharacterType")
  expect_equal(dim(result), c(3, 1))
})

test_that("matrix type inferred", {
  expression = call("matrix", 3.1, 5, 7)

  result = inferTypes(expression)
  expect_is(result, "VectorType")
  expect_is(atomicType(result), "NumericType")
  expect_equal(dim(result), c(5, 7))
})

