# Description:
#   Tests of inference for if statements.

context("if statements")


test_that("simple if statements work", {
  expr = quote(
    if (y == 5) 10.1
  )

  type = .infer_types(expr)
  expect_is(type, "ConditionalType")
  branch_types = getBranchTypes(type)
  expect_equal(branch_types, c("RealType", "NullType"))
})


test_that("simple if-else statements work", {
  expression = quote(
    if (y > -1)
      "hello"
    else
      "goodbye"
  )

  result = .infer_types(expression)
  expect_is(result, "CharacterType")
  expect_is(value(result), "UnknownValue")
})


test_that("simple if-else-if statements work", {
  expression = quote(
    if (y < 42)
      3i
    else if (y < 50)
      -3i
    else
      -1i
  )

  result = .infer_types(expression)
  expect_is(result, "ComplexType")
  expect_is(value(result), "UnknownValue")
})


test_that("if-else statements work", {
  expr = quote(
    if (x + 1 > 10)
      "cookies"
    else
      1L
  )

  type = .infer_types(expr)
  expect_is(type, "ConditionalType")
  branch_types = getBranchTypes(type)
  expect_equal(branch_types, c("CharacterType", "IntegerType"))
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
#  types = .infer_types(expr)
#  expect_is(types, "CharacterVectorType")
#})

