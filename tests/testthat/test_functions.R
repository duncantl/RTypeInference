# Description:
#   Tests of inference for functions.

context("functions")


test_that("literal return type is inferred for functions", {
  fun = function() 8.1

  type = infer_types(fun)$type

  expect_is(type, "FunctionType")
  expect_is(type@return_type, "RealType")

  fun = function() {
    42L
  }

  type = infer_types(fun)$type

  expect_is(type, "FunctionType")
  expect_is(type@return_type, "IntegerType")
})


test_that("types are collected for functions", {
  fun = function() {
    x = 31L
    y = x
    z = "Hello"
  }

  type = infer_types(fun)$type

  expect_is(type, "FunctionType")
  expect_is(type@return_type, "CharacterType")
  expect_is(type@scope$get("x"), "IntegerType")
  expect_is(type@scope$get("y"), "IntegerType")
  expect_is(type@scope$get("z"), "CharacterType")
})


# FIXME:
test_that("return type is inferred for functions that aren't type-stable", {
  fun = function() {
    if (rbinom(1, 1, 0.5) == 1)
      return(13L)

    return("Hello")
  }

  type = infer_types(fun)$type
#  expect_is(result, "ConditionalType")
})
