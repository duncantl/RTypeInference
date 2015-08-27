# Description:
#   Tests of inference of { expressions.

test_that("type is inferred for empty {", {
  expression = quote({})

  result = inferTypes(expression)
  expect_is(result, "NullType")
})

test_that("type is inferred for {", {
  expression = quote(
    {
      x = 1.1
      y = "hi"
    }
  )

  result = inferTypes(expression)
  expect_is(result, "CharacterType")
})

test_that("type is collected for {", {
  expression = quote(
    {
      a = 15L
      b = 0+1i
      c = a
    }
  )
  collector = TypeCollector()

  inferTypes(expression, collector)
  expect_is(collector$getVariableType("a"), "IntegerType")
  expect_is(collector$getVariableType("b"), "ComplexType")
  expect_is(collector$getVariableType("c"), "IntegerType")
})
