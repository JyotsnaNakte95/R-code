## Exercise 6.8.10
## Jyotsna Namdeo Nakte
## Summer 2019

# We have seen that as the number of features used in a model increases, 
# the training error will necessarily decrease, but the test error may not. 
# We will now explore this in a simulated data set.


# (a) Generate a data set with p = 20 features, n = 1,000 observations, 
#     and an associated quantitative response vector generated according 
#     to the model Y =Xβ+ε, where β has some elements that are exactly 
#     equal to zero.

p=20
n=1000
library(leaps)
set.seed(1)
X <- matrix(rnorm(n*p), n, p)
g <- rnorm(p)
g[sample(seq(20),5)] <- 0
Y <- X %*% g + rnorm(n)

# (b) Split your dataset into a training set containing 100 observations and 
#     a test set containing 900 observations.

trainingData = sample( 1:1000, 100 )
testingData = -trainingData
X.trainSet <- X[trainingData, ]
X.testSet <- X[testingData, ]
Y.trainSet <- Y[trainingData]
Y.testSet <- Y[testingData]

# (c) Perform best subset selection on the training set, and plot the 
#     training set MSE associated with the best model of each size.

data.trainingSet <- data.frame(Y = Y.trainSet, X = X.trainSet)
regfit <- regsubsets(Y ~ ., data = data.trainingSet, nvmax = 20)
trainingData.matrix <- model.matrix(Y ~ ., data = data.trainingSet, nvmax = 20)
value.errors <- rep(NA, 20)
for (i in 1:20) {
  coefi <- coef(regfit, id = i)
  predictModel <- trainingData.matrix[, names(coefi)] %*% coefi
  value.errors[i] <- mean((predictModel - Y.trainSet)^2)
}
plot(value.errors, xlab = "Number of predictors", ylab = "Training MSE", pch = 19, type = "o", col = "blue")

# (d) Plot the test set MSE associated with the best model of each size.

testingData.matrix <- model.matrix(Y~.,data = data.frame(X=X.testSet, Y=Y.testSet), nvmax=20)
testingData.errors <- rep(NA,20)
for (i in 1:20) {
  coef <- coef(regfit,id=i)
  predict <- testingData.matrix[,names(coef)] * coef
  testingData.errors[i]<- mean((predict-Y.testSet)^2)
}
plot(testingData.errors,ylab = "Tested MSE",xlab = " Number of predictors",pch = 19,type = "o", col="red")
points(which.min(testingData.errors),testingData.errors[which.min(testingData.errors)],
       col="blue",pch=19)

# (e) For which model size does the test set MSE take on its minimum value? 
#     Comment on your results. If it takes on its minimum value for a model 
#     containing only an intercept or a model containing all of the features, 
#     then play around with the way that you are generating the data in (a) 
#     until you come up with a scenario in which the test set MSE is minimized 
#     for an intermediate model size.

minimumValue <- which.min(testingData.errors)
minimumValue

## For the model size 3 the test set MSE to take on its minimum value.
## The model is tested for various five features as mentioned above which are set to zero from graph obtained
## we can see that model size 3 has minimum value. The test set MSE is minimized trying various features for 
## an intermediate model size. The above command suggests the best model is the one that contains 3 variables.
## From the graph plot obtained we can see that the third variable has the lowest test MSE.


# (f) How does the model at which the test set MSE is minimized compare to 
#     the true model used to generate the data? Comment on the coefficient 
#     values.

coef(regfit, minimumValue)

## The function shows coefficients value of the variables, the results show there is has values for 5,9, 18 
## where we had set five values to zero in (a), model size 3 the test set MSE is minimized. The true model
## used to generate the data contains model size of 20. 
## The coefficient values are ranging from 1.411 to 1.656, with intercept value -0.4496349 and 
## no values to rest no of predictors except 5,9,18.

# (g) Create a plot displaying j=1(βj − βj ) for a range of values of r, 
#     where βˆjr is the jth coefficient estimate for the best model containing 
#     r coefficients. Comment on what you observe. How does this compare to 
#     the test MSE plot from (d)?

val.errors <- rep(NA, 20)
X_Cols = colnames(X, do.NULL = FALSE, prefix = "X.")
for (i in 1:20) {
  coeffict <- coef(regfit, id = i)
  val.errors[i] <- sqrt(sum((g[X_Cols  %in% names(coeffict)] - coeffict[names(coeffict) %in% X_Cols ])^2) + sum(g[!(X_Cols  %in% names(coeffict))])^2)
}
plot(val.errors, xlab = "Number of coefficients", ylab = "Error between estimated and true coefficients", pch = 19, type = "b", col="red")
points(which.min(val.errors),val.errors[which.min(val.errors)],col="blue",pch=19)
minimumValue3 <- which.min(val.errors)
coef(regfit,minimumValue3)

## For the model size 12 the (g) set MSE to take on its minimum value. Its minimum value is not less than the one found above in 
## test set MSE(d). The graph shows minimum error estimated and true coefficients is minimum for model of size 12. The 
## MSE found in (d) set is of model size 3. The coefficient values range from -1.386 to 1.887, the values 1, 2, 4, 6, 13, 14,
## 16,17 have no values. 
## Above predicted model concludes suitable fit for the true coefficients will not relate to the minimized test error directly.
## This model with different variables for the test error was not the minimum in (d) or the test error was not minimum for different variables.
## Therefore, we could conclude the lowest error does not always mean lowest test MSE.
## Moreover the model follow some similar pattern to the train model which is decreasing gradually over the number of 
## predictors.


