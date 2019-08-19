## Exercise 9.7.5
## Jyotsna Nakte
## Summer 2019

# We have seen that we can fit an SVM with a non-linear kernel in order to perform classification using 
# a non-linear decision boundary. We will now see that we can also obtain a non-linear decision boundary 
# by performing logistic regression using non-linear transformations of the features.

# (a) Generate a data set with n = 500 and p = 2, such that the observations belong to two classes with 
#     a quadratic decision boundary between them. For instance, you can do this as follows:

set.seed(1)
x1 <- runif(500) - 0.5
x2 <- runif(500) - 0.5
y <- 1 * (x1^2 - x2^2 > 0)


# (b) Plot the observations, colored according to their class labels. Your plot should display X1 on the x-axis, 
#     and X2 on the y- axis.

plot(x1, x2, xlab = "X1", ylab = "X2", col = (4 - y), pch = (3 - y))

## ![Quadratic decision boundary colored according to their class labels ](viiia.pdf)

##  The plot generated above the value of y=0 class 0 it displays blue color and for value of y=1 class 1 
##  it displays with color is green. 

# (c) Fit a logistic regression model to the data, using X1 and X2 as predictors.

logisregfit <- glm(y ~ x1 + x2, family = "binomial")
summary(logisregfit)

##  Predictors show relatively large p-values which indicate that the variables are not statistically significant.

# (d) Apply this model to the training data in order to obtain a predicted class label for each training observation. 
#     Plot the observations, colored according to the predicted class labels. The decision boundary should be linear.

data <- data.frame(x1 = x1, x2 = x2, y = y)
probabilties <- predict(logisregfit, data, type = "response")
predictions <- rep(0, 500)
predictions[probabilties > 0.47] <- 1
plot(data[predictions == 1, ]$x1, data[predictions == 1, ]$x2, col = (4 - 1), pch = (3 - 1), xlab = "X1", ylab = "X2")
points(data[predictions == 0, ]$x1, data[predictions == 0, ]$x2, col = (4 - 0), pch = (3 - 0))

## ![observations of logistic regression applied to training model using linear function](viiib.pdf)

##  The prediction used the probability threshold of 0.47 put all the points to be classified to a single class,
##  Therefore no decision boundary detected.
##  The probability threshold of 0.47 gives us a linear decision boundary.

# (e) Now fit a logistic regression model to the data using non-linear functions of X1 and X2 as predictors 
#     (e.g. X12, X1 ×X2, log(X2), and so forth).

logitregnlfit <- glm(y ~ poly(x1, 2) + poly(x2, 2) + I(x1 * x2), family = "binomial")
summary(logitregnlfit)

##  As you can observe p-value of the predictors is relatively large which indicate the predictor are not statistically
##  significant to predict y. 

# (f) Apply this model to the training data in order to obtain a predicted class label for each training 
#     observation. Plot the observations, colored according to the predicted class labels. The decision boundary 
#     should be obviously non-linear. If it is not, then repeat (a)-(e) until you come up with an example in which the 
#     predicted class labels are obviously non-linear.

probabilties <- predict(logitregnlfit, data, type = "response")
predictions <- rep(0, 500)
predictions[probabilties > 0.47] <- 1
plot(data[predictions == 1, ]$x1, data[predictions == 1, ]$x2, col = (4 - 1), pch = (3 - 1), xlab = "X1", ylab = "X2")
points(data[predictions == 0, ]$x1, data[predictions == 0, ]$x2, col = (4 - 0), pch = (3 - 0))

## ![[observations of logistic regression applied to training model using non-linear function](viiic.pdf)

##  The plot shows the non-linear decision boundary between two classes closely resembling the true decision boundary.

# (g) Fit a support vector classifier to the data with X1 and X2 as predictors. Obtain a class prediction for 
#     each training observation. Plot the observations, colored according to the predicted class labels.

library(e1071)

data$y <- as.factor(data$y)
svmfit <- svm(y ~ x1 + x2, data, kernel = "linear", cost = 0.01)
predictions <- predict(svmfit, data)
plot(data[predictions == 0, ]$x1, data[predictions == 0, ]$x2, col = (4 - 0), pch = (3 - 0), xlab = "X1", ylab = "X2")
points(data[predictions == 1, ]$x1, data[predictions == 1, ]$x2, col = (4 - 1), pch = (3 - 1))

## ![observation after fitting svm classifier to data using linear kernel](viiid.pdf)

##  The plot shows how linear kernel fails to classify data into two classes.
##  It fails to find non-linear decision boundary and classifies all points to a single class.

# (h) Fit a SVM using a non-linear kernel to the data. Obtain a class prediction for each training observation. 
#     Plot the observations, colored according to the predicted class labels.
data$y <- as.factor(data$y)
svmnlfit <- svm(y ~ x1 + x2, data, kernel = "radial", gamma = 1)
predictions<- predict(svmnlfit, data)
plot(data[predictions == 0, ]$x1, data[predictions == 0, ]$x2, col = (4 - 0), pch = (3 - 0), xlab = "X1", ylab = "X2")
points(data[predictions == 1, ]$x1, data[predictions == 1, ]$x2, col = (4 - 1), pch = (3 - 1))

## ![observation after fitting svm to training data using non-linear kernel](viiie.pdf)

##  The plot shows a non-linear decision boundary between two classes when SVM uses non-linear kernel.
##  It is similar to the true decision boundary.

# (i) Comment on your results.

##  The plot generated in ‘(c)’ and '(e)' show that none of the variables are statistically significant because of larger p value.
##  The plot generated in '(d)' we can see that the decision boundary is linear.
##  The plot generated in '(f)' and '(h)' we can observe that the non-linear decision boundary is very similar to the true decision 
##  boundary.The plot generated in '(g)' we can see that the svm classifier with low cost classifies all points to single class.

##  The above plots and analysis say that the support vector machine with non-linear kernel help us findnon-linear decision 
##  boundaries easily and logistic regression with interaction terms is useful in finding non-linear decision boundaries.
##  SVM classifier with linear kernel and logistic regression without interaction terms does not find non-linear decision boundaries. It 
##  becomes difficult more if the number of features increase.
##  Moreover, manual tuning is required to find the right interaction terms for logistic regression but for svm we need 
##  to tune gamma which is done easily using cross-validation.
