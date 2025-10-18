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


# Test fitting Lasso function
# Fits between Lasso with lambda zero should be close to that given by LM
set.seed(100)
X <- matrix(rnorm(100 * 5, 10, 3), 100, 5)
Y <- X %*% 1:5

out <- standardizeXY(X, Y)
lm(out$Ytilde ~ out$Xtilde - 1)
fitLASSOstandardized(out$Xtilde, out$Ytilde, lambda = 0, eps = 1e-5)

set.seed(100)
X <- matrix(rnorm(100 * 5, 10, 3), 100, 5)
Y <- X %*% c(6,3,4,2,10)

out <- standardizeXY(X, Y)
lm(out$Ytilde ~ out$Xtilde - 1)
fitLASSOstandardized(out$Xtilde, out$Ytilde, lambda = 0, eps = 1e-5)

# Test sequence function
fitLASSOstandardized_seq(out$Xtilde, out$Ytilde)

# Test sequence function (unscaled)
set.seed(100)
X <- matrix(rnorm(100 * 5, 10, 3), 100, 5)
Y <- X %*% c(6,3,4,2,10) + 1

fitLASSO(X, Y)
lm(Y ~ X)

