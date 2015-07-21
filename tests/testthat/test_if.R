# Description:
#   Tests of inference for if statements.

test_that("simple if statements work", {
  expr = quote(
    if (y == 5) 10.1
  )

  type = inferTypes(expr)
  expect_is(type, "ConditionalType")
  expect_is(type@conditions[[1]]@type, "NumericType")
  expect_is(type@default, "NullType")
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

  type = inferTypes(expr)
  expect_is(type, "ConditionalType")
  expect_is(type@conditions[[1]]@type, "CharacterType")
  expect_is(type@default, "IntegerType")
})

#test_that("if-else statements collapse vector types", {
#  # TODO: Is this the right behavior?
#  expr = quote(
#    if (x + 1 > 10)
#      "cookies"
#    else
#      c("cookies", "cake")
#  )
#
#  types = inferTypes(expr)
#  expect_is(types, "CharacterVectorType")
#})

