# Description:
#   Tests of inference for for loops.

context("for loops")


test_that("iterator type is inferred for simple for loops", {
  expression = substitute(
    for (i in x) print(i),
    list(x = 1:3)
  )
  collector = TypeCollector()

  result = .infer_types(expression, collector)
  type = collector$getVariableType("i")
  expect_true(has_context(type, "iterator"))
  expect_is(type, "IntegerType")
})


test_that("types are collected for for loops", {
  expression = substitute(
    for (i in vec) {
      x = i
      y = 42L
    }, list(vec = c("a", "b", "c"))
  )
  collector = TypeCollector()

  result = .infer_types(expression, collector)
  expect_is(collector$getVariableType("x"), "CharacterType")
  expect_is(collector$getVariableType("y"), "IntegerType")
})
