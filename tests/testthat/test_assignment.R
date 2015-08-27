# Description:
#   Tests of inference for assignment.

test_that("type is inferred for literal assignments", {
  expression = call("=", quote(x), 5L)

  result = inferTypes(expression)
  expect_is(result, "IntegerType")

  expression[[3]] = 3.14i
  
  result = inferTypes(expression)
  expect_is(result, "ComplexType")
})

test_that("type is collected for literal assignments", {
  expression = call("=", quote(x), 5L)
  collector = TypeCollector()

  inferTypes(expression, collector)
  expect_is(collector$getVariableType("x"), "IntegerType")
})

test_that("type is inferred for recursive literal assignments", {
  expression = call("=", quote(x), call("=", quote(y), 5L))
  collector = TypeCollector()

  result = inferTypes(expression, collector)
  expect_is(result, "IntegerType")
  expect_is(collector$getVariableType("x"), "IntegerType")
  expect_is(collector$getVariableType("y"), "IntegerType")
})

test_that("type is infered for known-type variable assignments", {
  expression = call("=", quote(x), quote(y))
  collector = TypeCollector()
  collector$setVariableType("y", NumericType())

  result = inferTypes(expression, collector)
  expect_is(result, "NumericType")
  expect_is(collector$getVariableType("x"), "NumericType")
})

test_that("type is inferred for array assignments", {
})
