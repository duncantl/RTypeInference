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
    list(x = ArrayType(RealType(), NA), n = IntegerType())

  collector = TypeCollector()
  result = inferTypes(isum, collector)

  expect_true(has_context(collector$getVariableType("i"), "iterator"))
  expect_is(collector$getVariableType("i"), "IntegerType")
  expect_is(collector$getVariableType("total"), "RealType")
  expect_is(result, "RealType")
})
