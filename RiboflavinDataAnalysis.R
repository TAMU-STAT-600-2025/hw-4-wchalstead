# Load the riboflavin data

# Uncomment below to install hdi package if you don't have it already; 
# install.packages("hdi") 
library(hdi)
data(riboflavin) # this puts list with name riboflavin into the R environment, y - outcome, x - gene expression
dim(riboflavin$x) # n = 71 samples by p = 4088 predictors
?riboflavin # this gives you more information on the dataset

# This is to make sure riboflavin$x can be converted and treated as matrix for faster computations
class(riboflavin$x) <- class(riboflavin$x)[-match("AsIs", class(riboflavin$x))]


# Get matrix X and response vector Y
X = as.matrix(riboflavin$x)
Y = riboflavin$y

# Source your lasso functions
source("LassoFunctions.R")

#  Use your fitLASSO function on the riboflavin data with 60 tuning parameters
LASSOfit <- fitLASSO(X, Y)
#  Based on the above output, plot the number of non-zero elements in each beta versus the value of tuning parameter
plot(LASSOfit$lambda_seq, (colSums(LASSOfit$beta_mat != 0) + (LASSOfit$beta0_vec != 0)),
     type = 'o',
     xlab = 'Tuning Parameter',
     ylab = 'Number of Non-Zero Beta Values')

#  Use microbenchmark 10 times to check the timing of your fitLASSO function above with 60 tuning parameters
microbenchmark::microbenchmark(fitLASSO(X,Y), times = 10)
#  Report your median timing in the comments here: (~5.8 sec for Irina on her laptop)
# My fitLASSO function had a median time of 6.450513 seconds


#  Use cvLASSO function on the riboflavin data with 30 tuning parameters (just 30 to make it faster)
cvs <- cvLASSO(X, Y, n_lambda = 30)
#  Based on the above output, plot the value of CV(lambda) versus tuning parameter. Note that this will change with each run since the folds are random, this is ok.
plot(cvs$lambda_seq, cvs$cvm,
     type = 'o',
     xlab = 'Tuning Parameter, lambda',
     ylab = 'CV(lambda)')
