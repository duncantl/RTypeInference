# Description:
#   Tests of inference for logical operators.

test_that("scalar | works", {
  expr = quote(
    TRUE | FALSE
  )

  type = inferTypes(expr)
  expect_is(type, "LogicalType")
})

test_that("vector-vector | works", {
  x = c(FALSE, TRUE, TRUE)
  expr = substitute(
    c(TRUE, TRUE, FALSE) | x,
    list(x = x)
  )

  type = inferTypes(expr)
  expect_is(type, "VectorType")
  expect_is(atomicType(type), "LogicalType")
  expect_equal(length(type), length(x))
})

test_that("vector || and && work", {
  expr = quote(
    c(TRUE, FALSE, TRUE) || c(TRUE, TRUE, TRUE)
  )

  type = inferTypes(expr)
  expect_is(type, "LogicalType")

  expr = quote(
    c(TRUE, TRUE, FALSE) && c(FALSE, FALSE, FALSE)
  )

  type = inferTypes(expr)
  expect_is(type, "LogicalType")
})
