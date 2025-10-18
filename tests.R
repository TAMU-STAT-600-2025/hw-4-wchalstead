library(testthat)
source("LassoFunctions.R")

# Test standardizing function
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


# Test softmax function
test_that(
  "Softmax function works properly", {
    expect_equal(soft(5, 2), 3)
    expect_equal(soft(-5, 2), -3)
    expect_equal(soft(-5, 7), 0)
    expect_equal(soft(5, 7), 0)
  })
