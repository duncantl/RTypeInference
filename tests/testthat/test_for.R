# Description:
#   Tests of inference for for loops.

test_that("iterator type is inferred for simple for loops", {
  expression = substitute(
    for (i in x) print(i),
    list(x = 1:3)
  )
  collector = TypeCollector()

  result = inferTypes(expression, collector)
  type = collector$getType("i")
  expect_is(type, "IteratorType")
  expect_is(atomicType(type), "IntegerType")
})

test_that("types are collected for for loops", {
  expression = substitute(
    for (i in vec) {
      x = i
      y = 42L
    }, list(vec = c("a", "b", "c"))
  )
  collector = TypeCollector()

  result = inferTypes(expression, collector)
  expect_is(collector$getType("x"), "CharacterType")
  expect_is(collector$getType("y"), "IntegerType")
})
