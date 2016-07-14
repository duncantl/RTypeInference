# Description:
#   Tests of inference of { exprs.

context("syntax")


test_that("type is inferred for empty {", {
  expr = quote({})

  type = infer_types(expr)$type

  expect_is(type, "NullType")
})


test_that("type is inferred for {", {
  expr = quote(
    {
      x = 1.1
      y = "hi"
    }
  )

  type = infer_types(expr)$type

  expect_is(type, "CharacterType")
})


test_that("type is collected for {", {
  expr = quote(
    {
      a = 15L
      b = 0+1i
      c = a
    }
  )

  scope = infer_types(expr)$scope

  expect_is(scope$get("a"), "IntegerType")
  expect_is(scope$get("b"), "ComplexType")
  expect_is(scope$get("c"), "IntegerType")
})
