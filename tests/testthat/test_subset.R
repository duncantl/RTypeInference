# Description:
#   Tests of inference for subsetting.

context("subsets")


test_that("type inferred for 1d literal index subsetting", {
  idx = 1:5
  expression = call("[", quote(x), idx)
  collector = TypeCollector()

  result = .infer_types(expression, collector)
  expect_is(result, "ArrayType")
  expect_is(element_type(result), "UnknownType")
  expect_equal(length(result), length(idx))
})


test_that("type inferred for 1d literal logical subsetting", {
  subs = c(FALSE, TRUE, TRUE)
  expression = call("[", quote(x), subs)
  collector = TypeCollector()

  result = .infer_types(expression, collector)
  expect_is(result, "ArrayType")
  expect_is(element_type(result), "UnknownType")
  expect_equal(length(result), sum(subs))
})


test_that("type inferred for 1d variable index subsetting", {
  expression = call("[", quote(x), quote(idx))
  collector = TypeCollector()

  len = 5L
  collector$setVariableType("x", ArrayType(ComplexType(), 10L))
  collector$setVariableType("idx", ArrayType(IntegerType(), len))

  result = .infer_types(expression, collector)
  expect_is(result, "ArrayType")
  expect_is(element_type(result), "ComplexType")
  expect_equal(length(result), len)
})

