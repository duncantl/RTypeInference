# Description:
#   Tests of inference for assignment.

context("assignment")


test_that("type is inferred for literal assignments", {
  expr = call("=", as.name("x"), 5L)

  result = infer_types(expr)
  expect_is(result$type, "IntegerType")

  expr[[3]] = 3.14i
  
  result = infer_types(expr)
  expect_is(result$type, "ComplexType")
})


test_that("type is collected for literal assignments", {
  expr = call("=", as.name("x"), 5L)

  scope = infer_types(expr)$scope

  expect_is(scope$get("x"), "IntegerType")
})


test_that("type is inferred for recursive literal assignments", {
  expr = call("=", as.name("x"), call("=", as.name("y"), 5L))

  result = infer_types(expr)

  expect_is(result$type, "IntegerType")
  expect_is(result$scope$get("x"), "IntegerType")
  expect_is(result$scope$get("y"), "IntegerType")
})


test_that("type is infered for known-type variable assignments", {
  expr = call("=", quote(x), quote(y))

  scope = Scope()
  scope$set("y", RealType())

  result = infer_types(expr, scope)

  expect_is(result$type, "RealType")
  expect_is(result$scope$get("x"), "RealType")
})


test_that("type is inferred for array assignments", {
  # FIXME:
})
