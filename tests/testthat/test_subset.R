# Description:
#   Tests of inference for subsetting.

test_that("type inferred for 1d literal index subsetting", {
  idx = 1:5
  expression = call("[", quote(x), idx)
  collector = TypeCollector()

  result = inferTypes(expression, collector)
  expect_is(result, "VectorType")
  expect_is(atomicType(result), "UnknownType")
  expect_equal(length(result), length(idx))
})

test_that("type inferred for 1d literal logical subsetting", {
  subs = c(FALSE, TRUE, TRUE)
  expression = call("[", quote(x), subs)
  collector = TypeCollector()

  result = inferTypes(expression, collector)
  expect_is(result, "VectorType")
  expect_is(atomicType(result), "UnknownType")
  expect_equal(length(result), sum(subs))
})

test_that("type inferred for 1d variable index subsetting", {
  expression = call("[", quote(x), quote(idx))
  collector = TypeCollector()

  len = 5L
  collector$setVariableType("x", VectorType(ComplexType(), 10L))
  collector$setVariableType("idx", VectorType(IntegerType(), len))

  result = inferTypes(expression, collector)
  expect_is(result, "VectorType")
  expect_is(atomicType(result), "ComplexType")
  expect_equal(length(result), len)
})

