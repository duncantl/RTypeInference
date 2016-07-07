# Description:
#   Tests of inference of { expressions.

context("syntax")


test_that("type is inferred for empty {", {
  expression = quote({})

  result = .infer_types(expression)
  expect_is(result, "NullType")
})


test_that("type is inferred for {", {
  expression = quote(
    {
      x = 1.1
      y = "hi"
    }
  )

  result = .infer_types(expression)
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

  .infer_types(expression, collector)
  expect_is(collector$getVariableType("a"), "IntegerType")
  expect_is(collector$getVariableType("b"), "ComplexType")
  expect_is(collector$getVariableType("c"), "IntegerType")
})
