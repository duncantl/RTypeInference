context("return_type")


test_that("identical return types are simplified", {
  x = as_solution_set(list(
      "._return_1" = typesys::IntegerType()
      , "x_1" = typesys::ComplexType()
      , "._return_2" = typesys::IntegerType()
  ))

  result = return_type(x)

  # -----
  expect_is(result, "typesys::IntegerType")
})


test_that("distinct return types are unioned", {
  x = as_solution_set(list(
      "._return_1" = typesys::IntegerType()
      , "x_1" = typesys::ComplexType()
      , "._return_2" = typesys::NumericType()
  ))

  result = return_type(x)

  # -----
  expect_is(result, "typesys::Union")
})
