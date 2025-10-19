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
fitLASSOstandardized(out$Xtilde, out$Ytilde, lambda = 0, eps = 0.01)

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

cvLASSO(X, Y)

# Test cvLASSO
set.seed(100)
X <- matrix(rnorm(20 * 50, 10, 3), 20, 50)
Y <- X %*% rgeom(50, 0.75)

scaled <- standardizeXY(X, Y)
Xtilde <- scaled$Xtilde
Ytilde <- scaled$Ytilde

fit <- fitLASSOstandardized(Xtilde, Ytilde, 0.001)
betas <- fit$beta
for(i in 1:200) {
  fit <- fitLASSOstandardized(Xtilde, Ytilde, 0.001, beta_start = betas)
  betas <- fit$beta
}
betas

cvLASSO(X, Y)




#### Debugging
LASSOfit <- fitLASSO(X, Y, n_lambda = 60)
stand <- standardizeXY(X, Y)
n <- nrow(X)
lambda_max <- max(abs(crossprod(stand$Xtilde, stand$Ytilde)/n))

exp(seq(log(lambda_max), log(0.01), length = 58))

lasso(stand$Xtilde, stand$Ytilde, )

fitLASSOstandardized(stand$Xtilde, stand$Ytilde, 0.01)


plot(LASSOfit$beta_mat[1, ])
plot(LASSOfit$beta_mat[10, ])

