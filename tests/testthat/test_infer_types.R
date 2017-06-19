context("infer_types")


test_that("complicated", {
  code = quote({
    y = 1L
    if (x < 10)
        x = 3
    else x = 4
    for (i in 1:10) {
        if (x == 4)
            break
        else x = x + 1
        y = y + i
    }
  })

  result = infer_types(code)

  #browser()
  # -----
})


test_that("2d random walk", {
  # 2d Random Walk, from Ross Ihaka
  #rw2d = function(n = 100) {
  rw2d = quote({
    n = 100
    # FIXME:
    # xpos = ypos = numeric(n)
    xpos = numeric(n)
    ypos = xpos
    for(i in 2:n) {
      # Decide whether we are moving horizontally or vertically.
      if (runif(1) > .5)
        delta = 1
      else
        delta = -1

      if (runif(1) > .5) {
        xpos[i] = xpos[i - 1] + delta
        ypos[i] = ypos[i - 1]
      }
      else {
        xpos[i] = xpos[i - 1]
        ypos[i] = ypos[i - 1] + delta
      }
    }
    list(x = xpos, y = ypos)
  })

  cfg = rstatic::to_cfg(rstatic::to_ast(rw2d))
  set = constrain(cfg)
  #result = infer_types(rw2d)

  #browser()
  # -----
})


test_that("fun", {
  foo = function() {
    return (42L)
  }

  ast = rstatic::to_ast(foo)
  cfg = rstatic::to_cfg(ast)

  #browser()
})


test_that("fun2", {
  foo = function(x = 3) {
    return (x)
  }

  ast = rstatic::to_ast(foo)
  cfg = rstatic::to_cfg(ast)
  set = constrain(cfg)

  #browser()
})
