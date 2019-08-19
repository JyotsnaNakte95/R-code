## Exercise 5.4.6
## Jyotsna Nakte
## Summer 2019

# We continue to consider the use of a logistic regression model to predict 
# the probability of default using income and balance on the Default data set. 
# In particular, we will now compute estimates for the standard errors of the 
# income and balance logistic regression coeﬃcients in two diﬀerent ways: 
# (1) using the bootstrap, and (2) using the standard formula for computing 
# the standard errors in the glm() function. Do not forget to set a random seed 
# before beginning your analysis.

# (a) Using the summary() and glm() functions, determine the estimated standard 
#     errors for the coeﬃcients associated with income and balance in a multiple 
#     logistic regression model that uses both predictors. 

install.packages('ISLR')
require(ISLR)
set.seed(1)
attach(Default)
summary(Default)
glmModel <- glm(default ~ income + balance,data = Default, family = "binomial")
summary(glmModel)
summary(glmModel)$coefficients

# (b) Write a function, boot.fn(), that takes as input the Default data set as well 
#     as an index of the observations, and that outputs the coeﬃcient estimates for 
#     income and balance in the multiple logistic regression model. 

boot.fn <- function( data, index ) {
  mlrModel <- glm(default ~ income + balance, data = data, family = "binomial", subset = index)
  coefEst <- coef(mlrModel)
  return (coefEst)
}

# (c) Use the boot() function together with your boot.fn() function to estimate 
#     the standard errors of the logistic regression coeﬃcients for income and balance. 

library(boot)
boot(Default, boot.fn, 100)
boot(Default, boot.fn, 1000)

# (d) Comment on the estimated standard errors obtained using the glm() function and 
#     using your bootstrap function.

## The estimated standard errors using glm() function obtained is 4.985167e-06, 2.273731e-04.
## The estimated standard errors using bootstrap function for 100 observations is 
## 4.650176e-06, 2.016055e-04.
## The estimated standard errors using bootstrap function for 1000 observations is 
##  4.870014e-06, 2.292088e-04. 

## The bootstrap function is used to quantify the uncertainty associated with a given estimator.
## It gives measure of variablity.
## The estimated standard errors of glm() function are different for 100 observations bootstrap function 
## they are slightly smaller and slightly similar to 1000 observations of bootstrap function.
## glm() function is called noise model or error model it relies upon fixed noise variance and inputs.
## On otherhand, the bootstrap function as explained allows assigning measures of accuracy in terms of 
## bias, variance, confidence intervals, prediction error to sample estimates giving accurate results.
## The both methods show slight differences in results between them.
