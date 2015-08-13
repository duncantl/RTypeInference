# Description:
#   Tests of type annotation.

test_that("type annotations are collected", {
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

