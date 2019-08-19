## Exercise 8.4.8
## Jyotsna Nakte
## Summer 2019

# In the lab, a classification tree was applied to the Carseats data set after converting 
# Sales into a qualitative response variable. Now we will seek to predict Sales using regression 
# trees and related approaches, treating the response as a quantitative variable.

# (a) Split the data set into a training set and a test set.

library(ISLR)
set.seed(1)
train <- sample(1:nrow(Carseats), nrow(Carseats) / 2)
trainCarseats <- Carseats[train, ]
testCarseats <- Carseats[-train, ]

# (b) Fit a regression tree to the training set. Plot the tree, and interpret the results. 
#     What test MSE do you obtain?

library(tree)
tree.seats <- tree(Sales ~ ., data = trainCarseats)
summary(tree.seats)
plot(tree.seats)
text(tree.seats,pretty=0,cex=0.6)

## ![Decision tree of the training set of Carseats](viia.pdf)

## The decision tree interprets that "ShelveLoc", "Price", "Age","CompPrice" are mainly used to make decisions. The root of the tree is seperated
## by Good/Bad,Medium. Moreover, the sales amount is highest 11.640 where ShelveLoc is Good and price is less than 94.5 and average age greater than 39.5. The sales amount lowest is 3.487
## where ShelveLoc is categorized Bad with price greater than 130. There are total of 18 node leafs/terminals used and ShelveLoc being root node main criteria
## to decide. Therefore, lesser the price greater the sales and less average age, more cost on advertising,more CompPrice lead to more sales. Lastly, lesser income lesser sale.

yhat <- predict(tree.seats, newdata = testCarseats)
mean((yhat - testCarseats$Sales)^2)

## Test MSE obtained is 4.922039.

# (c) Use cross-validation in order to determine the optimal level of tree complexity. 
#     Does pruning the tree improve the test MSE?

crossval.carseats <- cv.tree(tree.seats)
plot(crossval.carseats$size, crossval.carseats$dev, type = "b")

## ![plot of cross validation error rate by the level of tree complexity](viib.pdf)

## The plot interprets optimal level of tree complexity is 9.

tree.minimum <- which.min(crossval.carseats$dev)
points(tree.minimum, crossval.carseats$dev[tree.minimum], col = "blue", cex = 2, pch = 20)
pruning.carseats <- prune.tree(tree.seats, best = 9)
plot(pruning.carseats)
text(pruning.carseats, pretty = 0)

## ![decision tree for the optimal level of tree complexity after pruning of tree ](viic.pdf)

yhat <- predict(pruning.carseats, newdata = testCarseats)
mean((yhat - testCarseats$Sales)^2)

## Test MSE obtained is 4.918134 which is almost similar to above so we can say it made minute or changes to improve the tree.

# (d) Use the bagging approach in order to analyze this data. What test MSE do you obtain? 
#     Use the importance() function to determine which variables are most important.

library(randomForest)
bagging.Seats <- randomForest(Sales ~ ., data = trainCarseats, mtry = 10, ntree = 500, importance = TRUE)
yhat.bagging <- predict(bagging.Seats, newdata = testCarseats)
mean((yhat.bagging - testCarseats$Sales)^2)

## Test MSE obtained is 2.657296 which is much lower than the MSE obtained in pruning tree and first build decision tree.

importance(bagging.Seats)
varImpPlot(bagging.Seats)

## ![Visualization of the variables most important in bagging approach](viid.pdf)

## Price and Shevloc variables have the highest importance and urban, education and population having less importance.

# (e) Use random forests to analyze this data. What test MSE do you obtain? Use the importance() 
#     function to determine which variables are most important. Describe the effect of m, the number 
#     of variables considered at each split, on the error rate obtained.

library(randomForest)
randomf.Seats <- randomForest(Sales ~ ., data = trainCarseats, mtry = 3, ntree = 500, importance = TRUE)
yhat.randomf <- predict(randomf.Seats, newdata = testCarseats)
mean(( yhat.randomf- testCarseats$Sales)^2)

## Test MSE obtained is 3.049406 which is more than the MSE obtained by the bagging method but less than first two methods.

importance(randomf.Seats)
varImpPlot(randomf.Seats)

## ![Visualization of the variables most important performing random forest](viie.pdf)

## Trying different values of m we see that as we increase the value of m that is increase the number of variables considered at each split the error rate decreases.
## At value m = 7 the error rate decreases from 2.65 obtained by bagging approach.
## Price and Shevloc variables have the highest importance and urban, education and population having less importance.
