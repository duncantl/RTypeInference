# Description:
#   Tests of inference on example functions (case studies).

context("case studies")


# FIXME: This should use the user-facing `infer_types` function.
test_that("types are inferred for isum()", {
  isum =
  function(x, n)
  {
    total = 0
    for(i in 1:n)
        total = total + x[i]
    total
  }
  attr(isum, ".typeInfo") =
    list(x = ArrayType(RealType(), NA), n = IntegerType())

  type = infer_types(isum)$type
  fn_scope = type@scope

  expect_true(has_context(fn_scope$get("i"), "iterator"))
  expect_is(fn_scope$get("i"), "IntegerType")
  expect_is(fn_scope$get("total"), "RealType")
  expect_is(type@return_type, "RealType")
})
