# Description:
#   Tests of type annotation.

test_that("type annotations are collected from .typeInfo calls", {
  expression = quote({
    .typeInfo(
      x = IntegerType(),
      y = VectorType(NumericType(), 3L)
    )
    new_x = x
    new_y = y
  })
  collector = TypeCollector()

  inferTypes(expression, collector)

  expect_is(collector$getType("x"), "IntegerType")
  expect_is(collector$getType("new_x"), "IntegerType")

  y_type = collector$getType("y")
  expect_is(y_type, "VectorType")
  expect_is(atomicType(y_type), "NumericType")
  expect_equal(length(y_type), 3L)

  new_y_type = collector$getType("new_y")
  expect_is(new_y_type, "VectorType")
  expect_is(atomicType(y_type), "NumericType")
  expect_equal(length(y_type), 3L)
})

test_that("type annotations are collected from .typeInfo attributes", {
  expression = function(x, y) {
    new_x = x
    new_y = y
  }
  attr(expression, ".typeInfo") = list(
    x = IntegerType(),
    y = VectorType(NumericType(), 3L)
  )

  collector = TypeCollector()
  inferTypes(expression, collector)

  expect_is(collector$getType("x"), "IntegerType")
  expect_is(collector$getType("new_x"), "IntegerType")

  y_type = collector$getType("y")
  expect_is(y_type, "VectorType")
  expect_is(atomicType(y_type), "NumericType")
  expect_equal(length(y_type), 3L)

  new_y_type = collector$getType("new_y")
  expect_is(new_y_type, "VectorType")
  expect_is(atomicType(y_type), "NumericType")
  expect_equal(length(y_type), 3L)
})

test_that("type annotations are collected from .typeInfo parameter", {
  expression = function(x, y) {
    new_x = x
    new_y = y
  }
 type_list = list(
    x = IntegerType(),
    y = VectorType(NumericType(), 3L)
  )

  collector = TypeCollector()
  inferTypes(expression, collector, .typeInfo = type_list)

  expect_is(collector$getType("x"), "IntegerType")
  expect_is(collector$getType("new_x"), "IntegerType")

  y_type = collector$getType("y")
  expect_is(y_type, "VectorType")
  expect_is(atomicType(y_type), "NumericType")
  expect_equal(length(y_type), 3L)

  new_y_type = collector$getType("new_y")
  expect_is(new_y_type, "VectorType")
  expect_is(atomicType(y_type), "NumericType")
  expect_equal(length(y_type), 3L)
})
