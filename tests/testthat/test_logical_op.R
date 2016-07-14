# Description:
#   Tests of inference for logical operators.

context("logical ops")


test_that("scalar | works", {
  expr = quote(
    TRUE | FALSE
  )

  type = infer_types(expr)$type

  expect_is(type, "BooleanType")
})


test_that("vector-vector | works", {
  x = c(FALSE, TRUE, TRUE)
  expr = substitute(
    c(TRUE, TRUE, FALSE) | x,
    list(x = x)
  )

  type = infer_types(expr)$type

  expect_is(type, "ArrayType")
  expect_is(element_type(type), "BooleanType")
  expect_equal(length(type), length(x))
})


test_that("vector || and && work", {
  expr = quote(
    c(TRUE, FALSE, TRUE) || c(TRUE, TRUE, TRUE)
  )

  type = infer_types(expr)$type

  expect_is(type, "BooleanType")

  expr = quote(
    c(TRUE, TRUE, FALSE) && c(FALSE, FALSE, FALSE)
  )

  type = infer_types(expr)$type

  expect_is(type, "BooleanType")
})
