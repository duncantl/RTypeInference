# Description:
#   Tests of type annotation.

context("annotations")


test_that("type annotations are collected from .typeInfo calls", {
  expr = quote({
    .typeInfo(
      x = IntegerType(),
      y = ArrayType(RealType(), 3L)
    )
    new_x = x
    new_y = y
  })

  scope = infer_types(expr)$scope

  expect_is(scope$get("x"), "IntegerType")
  expect_is(scope$get("new_x"), "IntegerType")

  y_type = scope$get("y")
  expect_is(y_type, "ArrayType")
  expect_is(element_type(y_type), "RealType")
  expect_equal(length(y_type), 3L)

  new_y_type = scope$get("new_y")
  expect_is(new_y_type, "ArrayType")
  expect_is(element_type(y_type), "RealType")
  expect_equal(length(y_type), 3L)
})


test_that("type annotations are collected from .typeInfo attributes", {
  expr = function(x, y) {
    new_x = x
    new_y = y
  }
  attr(expr, ".typeInfo") = list(
    x = IntegerType(),
    y = ArrayType(RealType(), 3L)
  )

  type = infer_types(expr)$type
  fn_scope = type@scope

  expect_is(fn_scope$get("x"), "IntegerType")
  expect_is(fn_scope$get("new_x"), "IntegerType")

  y_type = fn_scope$get("y")
  expect_is(y_type, "ArrayType")
  expect_is(element_type(y_type), "RealType")
  expect_equal(length(y_type), 3L)

  new_y_type = fn_scope$get("new_y")
  expect_is(new_y_type, "ArrayType")
  expect_is(element_type(y_type), "RealType")
  expect_equal(length(y_type), 3L)
})


test_that("type annotations are collected from .typeInfo parameter", {
  expr = function(x, y) {
    new_x = x
    new_y = y
  }
 type_list = list(
    x = IntegerType(),
    y = ArrayType(RealType(), 3L)
  )

  type = infer_types(expr, .typeInfo = type_list)$type
  fn_scope = type@scope

  expect_is(fn_scope$get("x"), "IntegerType")
  expect_is(fn_scope$get("new_x"), "IntegerType")

  y_type = fn_scope$get("y")
  expect_is(y_type, "ArrayType")
  expect_is(element_type(y_type), "RealType")
  expect_equal(length(y_type), 3L)

  new_y_type = fn_scope$get("new_y")
  expect_is(new_y_type, "ArrayType")
  expect_is(element_type(y_type), "RealType")
  expect_equal(length(y_type), 3L)
})
