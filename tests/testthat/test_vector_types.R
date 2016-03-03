# Description:
#   Tests of inference for vector types.

test_that("logical vector types work", {
  x = c(TRUE, FALSE, TRUE)

  type = inferTypes(x)
  expect_is(type, "ArrayType")
  expect_is(element_type(type), "BooleanType")
  expect_equal(length(type), length(x))
})

test_that("integer vector types work", {
  x = c(-3L, 1L)

  type = inferTypes(x)
  expect_is(type, "ArrayType")
  expect_is(element_type(type), "IntegerType")
  expect_equal(length(type), length(x))
})

test_that("numeric vector types work", {
  x = c(3.17, 3.14, 1.4, 4.2)

  type = inferTypes(x)
  expect_is(type, "ArrayType")
  expect_is(element_type(type), "RealType")
  expect_equal(length(type), length(x))
})

test_that("complex vector types work", {
  x = c(0+0i, -3-1i, 1-12i)

  type = inferTypes(x)
  expect_is(type, "ArrayType")
  expect_is(element_type(type), "ComplexType")
  expect_equal(length(type), length(x))
})

test_that("character vector types work", {
  x = c("hello", "to", "you")

  type = inferTypes(x)
  expect_is(type, "ArrayType")
  expect_is(element_type(type), "CharacterType")
  expect_equal(length(type), length(x))
})
