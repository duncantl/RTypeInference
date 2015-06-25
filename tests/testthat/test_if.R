# Description:
#   Tests of inference for if statements.

test_that("simple if statements work", {
  expr = quote(
    if (y == 5) 10.1
  )

  type = inferTypes(expr)
  expect_is(type, "NumericType")
})

test_that("simple if-else statements work", {
  expr = quote(
    if (y > -1)
      "hello"
    else
      "goodbye"
  )

  type = inferTypes(expr)
  expect_is(type, "CharacterType")
})

test_that("simple if-else-if statements work", {
  expr = quote(
    if (y < 42)
      3i
    else if (y < 50)
      -3i
    else
      -1i
  )

  type = inferTypes(expr)
  expect_is(type, "ComplexType")
})

test_that("if-else statements work", {
  expr = quote(
    if (x + 1 > 10)
      "cookies"
    else
      1L
  )

  types = inferTypes(expr)
  expect_is(types, "list")
  expect_equal(length(types), 2)
  expect_is(types[[1]], "CharacterType")
  expect_is(types[[2]], "IntegerType")
})

test_that("if-else statements collapse vector types", {
  # TODO: Is this the right behavior?
  expr = quote(
    if (x + 1 > 10)
      "cookies"
    else
      c("cookies", "cake")
  )

  types = inferTypes(expr)
  expect_is(types, "CharacterVectorType")
})

