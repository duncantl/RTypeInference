# Description:
#   Tests using default arguments to infer type.

context("default args")


test_that("default arguments are used to infer type", {
  fun = function(x, y = 3L, text = "Hello") {
    z = x
    return(text)
  }

  fn = infer_types(fun)$type

  expect_is(fn@return_type, "CharacterType")
  expect_is(fn@scope$get("x"), "UnknownType")
  expect_is(fn@scope$get("y"), "IntegerType")
  expect_is(fn@scope$get("z"), "UnknownType")
})
