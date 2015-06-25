# Description:
#   Tests of inference for math operators.

test_that("scalar multiplication works", {
  expr = quote(
    1L * 2.3
  )

  type = inferTypes(expr)
  expect_is(type, "NumericType")
})

test_that("scalar division works", {
  expr = quote(
    5L / 5L
  )

  type = inferTypes(expr)
  expect_is(type, "NumericType")

  # Mixed types
  expr = quote(
    TRUE / 1.4
  )

  type = inferTypes(expr)
  expect_is(type, "NumericType")

  # Complex types
  expr = quote(
    1i / 2.5
  )

  type = inferTypes(expr)
  expect_is(type, "ComplexType")
})

test_that("scalar exponentiation works", {
  expr = quote(
    1L ^ 2L
  )

  type = inferTypes(expr)
  expect_is(type, "NumericType")

  # Complex types
  expr = quote(
    TRUE ^ 1i
  )

  type = inferTypes(expr)
  expect_is(type, "ComplexType")
})

test_that("scalar-vector addition works", {
  x = c(1.1, 2.3, 5.1)
  expr = substitute(
    5 + x,
    list(x = x)
  )

  type = inferTypes(expr)
  expect_is(type, "NumericVectorType")
  expect_equal(type@length, length(x))
})

test_that("vector-vector addition works", {
  x = c(TRUE, FALSE, TRUE)
  expr = substitute(
    c(TRUE, TRUE, TRUE) + x,
    list(x = x)
  )

  type = inferTypes(expr)
  expect_is(type, "IntegerVectorType")
  expect_equal(type@length, length(x))
})

test_that("recycled vector-vector addition works", {
  x = c(1.1, 2.3, 5.1, -1.4, 99.1)
  expr = substitute(
    c(1.3, -1.7) + x,
    list(x = x)
  )

  type = inferTypes(expr)
  expect_is(type, "NumericVectorType")
  expect_equal(type@length, length(x))
})
