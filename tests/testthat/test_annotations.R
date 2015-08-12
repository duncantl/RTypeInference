# Description:
#   Tests of type annotation.

test_that("type annotations are collected", {
  expression = quote({
    .typeInfo(x = IntegerType(), y = NumericVectorType(length = 3L))
    new_x = x
    new_y = y
  })
  collector = TypeCollector()

  inferTypes(expression, collector)
  expect_is(collector$getType("x"), "IntegerType")
  expect_is(collector$getType("new_x"), "IntegerType")
  y_type = collector$getType("y")
  expect_is(y_type, "NumericVectorType")
  expect_equal(y_type@length, 3L)
  new_y_type = collector$getType("new_y")
  expect_is(new_y_type, "NumericVectorType")
  expect_equal(new_y_type@length, 3L)
})

