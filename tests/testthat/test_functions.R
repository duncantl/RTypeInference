# Description:
#   Tests of inference for functions.

test_that("literal return type is inferred for functions", {
  fun = function() 8.1

  result = inferTypes(fun)
  expect_is(result, "RealType")

  fun = function() {
    42L
  }

  result = inferTypes(fun)
  expect_is(result, "IntegerType")
})

test_that("types are collected for functions", {
  fun = function() {
    x = 31L
    y = x
    z = "Hello"
  }
  collector = TypeCollector()

  result = inferTypes(fun, collector)
  expect_is(result, "CharacterType")
  expect_is(collector$getVariableType("x"), "IntegerType")
  expect_is(collector$getVariableType("y"), "IntegerType")
  expect_is(collector$getVariableType("z"), "CharacterType")
})

test_that("return type is inferred for functions that aren't type-stable", {
  fun = function() {
    if (rbinom(1, 1, 0.5) == 1)
      return(13L)

    return("Hello")
  }
  collector = TypeCollector()

  result = inferTypes(fun, collector)
  expect_is(result, "ConditionalType")
})
