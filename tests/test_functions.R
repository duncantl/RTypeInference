test_function = function(x) {
  y = rnorm(x)
  z = y^2
  z <- z[which(z < 1)]
  blah = 2.5
  blah1 = 3.5
  blah2 = 1
  yavar = (10L * 1:10)
  foo1 = length(yavar)

  ll = y > .25
  ll2 = blah > 2
  
  if(length(z) > (x / 2))
  {
    a = 2
    b = 1
    c = a * b
    return(length(z) + c)
  }
  
  return(0)
}
