context("constrain")


test_that("constraints generated from default arguments", {
  code = function(n = 0L) {
    n = n - 1
    return (n)
  }

  cfg = rstatic::to_cfg(rstatic::to_ast(code))
  result = constrain(cfg)

  # -----
  expect_is(result[[1]][[2]], "typesys::IntegerType")
})


test_that("no error when default arguments are unspecified", {
  code = function(x) {
    return (x)
  }

  cfg = rstatic::to_cfg(rstatic::to_ast(code))
  result = constrain(cfg)

  # -----
  # Okay as long as there was no error.
})
