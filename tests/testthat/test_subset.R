# Description:
#   Tests of inference for subsetting.

context("subsets")


test_that("type inferred for 1d literal index subsetting", {
  idx = 1:5
  expr = call("[", as.name("x"), idx)

  type = infer_types(expr)$type

  expect_is(type, "ArrayType")
  expect_is(element_type(type), "UnknownType")
  expect_equal(length(type), length(idx))
})


test_that("type inferred for 1d literal logical subsetting", {
  subs = c(FALSE, TRUE, TRUE)
  expr = call("[", as.name("x"), subs)

  type = infer_types(expr)$type

  expect_is(type, "ArrayType")
  expect_is(element_type(type), "UnknownType")
  expect_equal(length(type), sum(subs))
})


test_that("type inferred for 1d variable index subsetting", {
  expr = call("[", quote(x), quote(idx))
  scope = Scope()

  len = 5L
  scope$set("x", ArrayType(ComplexType(), 10L))
  scope$set("idx", ArrayType(IntegerType(), len))

  type = infer_types(expr, scope)$type

  expect_is(type, "ArrayType")
  expect_is(element_type(type), "ComplexType")
  expect_equal(length(type), len)
})

