# Description:
#   Tests of inference for for loops.

context("for loops")


test_that("iterator type is inferred for simple for loops", {
  expr = substitute(
    for (i in x) print(i),
    list(x = 1:3)
  )

  scope = infer_types(expr)$scope

  i_type = scope$get("i")
  expect_true(has_context(i_type, "iterator"))
  expect_is(i_type, "IntegerType")
})


test_that("types are collected for for loops", {
  expr = substitute(
    for (i in vec) {
      x = i
      y = 42L
    }, list(vec = c("a", "b", "c"))
  )

  scope = infer_types(expr)$scope

  expect_is(scope$get("x"), "CharacterType")
  expect_is(scope$get("y"), "IntegerType")
})
