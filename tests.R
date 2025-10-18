library(testthat)
source("LassoFunctions.R")

set.seed(100)
X <- matrix(rnorm(50 * 50, 10, 3), 50, 50)
Y <- rnorm(50, 15, 2)

out <- standardizeXY(X, Y)
test_that(
  "Standardize function works properly", {
  expect_equal(apply(out$Xtilde, 2, crossprod)/50, rep(1, 50))
  expect_equal(colMeans(out$Xtilde), numeric(50))
  expect_equal(mean(out$Ytilde), 0)
})
