# Description:
#   Tests of inference for functions.

test_that("literal return type is inferred for functions", {
  fun = function() 8.1

  result = inferTypes(fun)
  expect_is(result, "NumericType")

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
  expect_is(collector$getType("x"), "IntegerType")
  expect_is(collector$getType("y"), "IntegerType")
  expect_is(collector$getType("z"), "CharacterType")
})
