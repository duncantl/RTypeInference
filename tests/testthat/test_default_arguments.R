# Description:
#   Tests using default arguments to infer type.

test_that("default arguments are used to infer type", {
  fun = function(x, y = 3L, text = "Hello") {
    z = x
    return(text)
  }
  collector = TypeCollector()

  result = inferTypes(fun, collector)
  expect_is(result, "CharacterType")
  expect_is(collector$getType("x"), "UnknownType")
  expect_is(collector$getType("y"), "IntegerType")
  expect_is(collector$getType("z"), "UnknownType")
})
