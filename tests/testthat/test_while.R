# Description:
#   Tests of inference for while loops.

context("while loops")


test_that("types are collected for simple while loops", {
  expr = quote(
    while (i < 3)
      i = i + 1
  )
  scope = Scope()
  scope$set("i", IntegerType())

  result = infer_types(expr, scope)

  expect_is(result$type, "IntegerType")
  expect_is(result$scope$get("i"), "IntegerType")
})


test_that("types are collected for while loops", {
  expr = quote(
    while (j < 10) {
      x = j
      y = x / 1i
      j = j + 1
    }
  )
  scope = Scope()
  scope$set("j", IntegerType())

  result = infer_types(expr, scope)

  expect_is(result$type, "IntegerType")
  expect_is(result$scope$get("x"), "IntegerType")
  expect_is(result$scope$get("y"), "ComplexType")
  expect_is(result$scope$get("j"), "IntegerType")
})
