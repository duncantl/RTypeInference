# Description:
#   Tests of inference on example functions (case studies).

test_that("types are inferred for isum()", {
  isum =
  function(x, n)
  {
    total = 0
    for(i in 1:n)
        total = total + x[i]
    total
  }
  attr(isum, ".typeInfo") =
    list(x = VectorType(NumericType(), NA), n = IntegerType())

  collector = TypeCollector()
  result = inferTypes(isum, collector)

  expect_is(collector$getType("i"), "IteratorType")
  expect_is(collector$getType("total"), "NumericType")
  expect_is(result, "NumericType")
})
