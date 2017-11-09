context("apply_solution")


test_that("apply_solution simplifies unions", {
  x = typesys::Union(typesys::IntegerType(), "x")
  soln = structure(list(typesys::IntegerType()), names = "x")

  result = apply_solution(x, soln)

  # -----
  expect_is(result, "typesys::IntegerType")
})
