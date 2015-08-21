# Description:
#   Tests of return type inference.

#test_that("implicitly returned constants", {
#    const_func = function() 1.0
#
#    return_type = inferType(const_func)$returnTypes[[1]]
#    expect_equal("double", return_type)
#})
#
#test_that("implicitly returned assignment", {
#    double_func = function() x = 1.0
#
#    return_type = inferType(double_func)$returnTypes[[1]]
#    expect_equal("double", return_type)
#})
#
#test_that("returned constants", {
#    const_func = function() return(1.0)
#
#    return_type = inferType(const_func)$returnTypes[[1]]
#    expect_equal("double", return_type)
#})
