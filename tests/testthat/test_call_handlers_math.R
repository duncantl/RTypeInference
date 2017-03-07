context("call handlers (math)")


test_that("unary ops preserve scalar types", {
  args = list(typesys::RealType())

  type_add = CALL_HANDLERS[["+"]](args)
  type_sub = CALL_HANDLERS[["-"]](args)

  # -----
  expect_is(type_add, "typesys::RealType")
  expect_is(type_sub, "typesys::RealType")
})


test_that("binary_ops preserve same scalar types", {
  args = list(typesys::RealType(), typesys::RealType())

  type_add = CALL_HANDLERS[["+"]](args)
  type_sub = CALL_HANDLERS[["-"]](args)
  type_mul = CALL_HANDLERS[["*"]](args)
  type_div = CALL_HANDLERS[["/"]](args)
  type_exp = CALL_HANDLERS[["^"]](args)

  # -----
  expect_is(type_add, "typesys::RealType")
  expect_is(type_sub, "typesys::RealType")
  expect_is(type_mul, "typesys::RealType")
  expect_is(type_div, "typesys::RealType")
  expect_is(type_exp, "typesys::RealType")
})


test_that("binary ops upcast different scalar types", {
  args = list(typesys::IntegerType(), typesys::ComplexType())

  type_add = CALL_HANDLERS[["+"]](args)
  type_sub = CALL_HANDLERS[["-"]](args)
  type_mul = CALL_HANDLERS[["*"]](args)
  type_div = CALL_HANDLERS[["/"]](args)
  type_exp = CALL_HANDLERS[["^"]](args)

  # -----
  expect_is(type_add, "typesys::ComplexType")
  expect_is(type_sub, "typesys::ComplexType")
  expect_is(type_mul, "typesys::ComplexType")
  expect_is(type_div, "typesys::ComplexType")
  expect_is(type_exp, "typesys::ComplexType")
})


test_that("/ and ^ upcast to numeric or higher", {
  args = list(typesys::IntegerType(), typesys::IntegerType())

  type_div = CALL_HANDLERS[["/"]](args)
  type_exp = CALL_HANDLERS[["^"]](args)

  # -----
  expect_is(type_div, "typesys::RealType")
  expect_is(type_exp, "typesys::RealType")
})

# TODO: scalar-vector binary ops
# TODO: vector-vector binary ops
# TODO: recycling
