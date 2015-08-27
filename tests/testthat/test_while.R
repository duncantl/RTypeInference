# Description:
#   Tests of inference for while loops.

test_that("types are collected for simple while loops", {
  expression = quote(
    while (i < 3)
      i = i + 1
  )
  collector = TypeCollector()
  collector$setVariableType("i", IntegerType())

  result = inferTypes(expression, collector)
  type = collector$getVariableType("i")
  expect_is(result, "IntegerType")
  expect_is(type, "IntegerType")
})

test_that("types are collected for while loops", {
  expression = quote(
    while (j < 10) {
      x = j
      y = x / 1i
      j = j + 1
    }
  )
  collector = TypeCollector()
  collector$setVariableType("j", IntegerType())

  result = inferTypes(expression, collector)
  expect_is(result, "IntegerType")
  expect_is(collector$getVariableType("x"), "IntegerType")
  expect_is(collector$getVariableType("y"), "ComplexType")
  expect_is(collector$getVariableType("j"), "IntegerType")
})
