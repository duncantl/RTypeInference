# Description:
#   Tests of type annotation.

context("annotations")


test_that("type annotations are collected from .typeInfo calls", {
  expression = quote({
    .typeInfo(
      x = IntegerType(),
      y = ArrayType(RealType(), 3L)
    )
    new_x = x
    new_y = y
  })
  collector = TypeCollector()

  .infer_types(expression, collector)

  expect_is(collector$getVariableType("x"), "IntegerType")
  expect_is(collector$getVariableType("new_x"), "IntegerType")

  y_type = collector$getVariableType("y")
  expect_is(y_type, "ArrayType")
  expect_is(element_type(y_type), "RealType")
  expect_equal(length(y_type), 3L)

  new_y_type = collector$getVariableType("new_y")
  expect_is(new_y_type, "ArrayType")
  expect_is(element_type(y_type), "RealType")
  expect_equal(length(y_type), 3L)
})


test_that("type annotations are collected from .typeInfo attributes", {
  expression = function(x, y) {
    new_x = x
    new_y = y
  }
  attr(expression, ".typeInfo") = list(
    x = IntegerType(),
    y = ArrayType(RealType(), 3L)
  )

  collector = TypeCollector()
  .infer_types(expression, collector)

  expect_is(collector$getVariableType("x"), "IntegerType")
  expect_is(collector$getVariableType("new_x"), "IntegerType")

  y_type = collector$getVariableType("y")
  expect_is(y_type, "ArrayType")
  expect_is(element_type(y_type), "RealType")
  expect_equal(length(y_type), 3L)

  new_y_type = collector$getVariableType("new_y")
  expect_is(new_y_type, "ArrayType")
  expect_is(element_type(y_type), "RealType")
  expect_equal(length(y_type), 3L)
})


test_that("type annotations are collected from .typeInfo parameter", {
  expression = function(x, y) {
    new_x = x
    new_y = y
  }
 type_list = list(
    x = IntegerType(),
    y = ArrayType(RealType(), 3L)
  )

  collector = TypeCollector()
  .infer_types(expression, collector, .typeInfo = type_list)

  expect_is(collector$getVariableType("x"), "IntegerType")
  expect_is(collector$getVariableType("new_x"), "IntegerType")

  y_type = collector$getVariableType("y")
  expect_is(y_type, "ArrayType")
  expect_is(element_type(y_type), "RealType")
  expect_equal(length(y_type), 3L)

  new_y_type = collector$getVariableType("new_y")
  expect_is(new_y_type, "ArrayType")
  expect_is(element_type(y_type), "RealType")
  expect_equal(length(y_type), 3L)
})
