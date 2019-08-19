## Exercise 3.7.9
## Jyotsna Namdeo Nakte
## Summer 2019

# This question involves the use of multiple linear regression on the Auto data set.

## Reading the file

auto <- read.csv("C:/Users/jyots/Desktop/KDD/Homework02/Auto.csv", header=T, na.strings="?")
summary(auto)

# (a) Produce a scatterplot matrix which includes all of the variables in the data set.

pairs(auto)

# (b) Compute the matrix of correlations between the variables using the function cor(). 
#     You will need to exclude the name variable, which is qualitative.

autonumericlform = auto[,-9]
cor(autonumericlform)

# (c) Use the lm() function to perform a multiple linear regression with mpg as the 
#     response and all other variables except name as the predictors. Use thesummary() 
#     function to print the results. Comment on the output. For instance: 

multipleLinearRegression = lm(mpg~.-name, data = auto)
summary(multipleLinearRegression)

#     i.   Is there a relationship between the predictors and the response? 
##  The summary shows there is relationship in between of predictors and response and 
##  the small p-value indicates that not all the predictors have strong relationship 
##  with the response. 

#     ii.  Which predictors appear to have a statistically signiﬁcant relationship to the response? 
## The predictors displacement, acceleration, year, origin have a statistically signiﬁcant relationship 
## to the response as the p-values are considerably small. 

#     iii. What does the coeﬃcient for the year variable suggest? 
## The coeﬃcient for the year variable has positive relationship with the response 
## and value of mpg increases 0.75 with increase in every year. The change in coeﬃcient
## value suggest that one model is not sufficient.

# (d) Use the plot() function to produce diagnostic plots of the linear regression ﬁt. 
#     Comment on any problems you see with the ﬁt. Do the residual plots suggest any 
#     unusually large outliers? Does the leverage plot identify any observations with
#     unusually high leverage? 

par(mfrow = c(2, 2))
plot(multipleLinearRegression)

## Residual vs fitted plot shows non-linear variablity, the values indicate unusual large outliers.
## In the Normal Q-Q the residuals is not normally distributed.
## There are points greater than 3 that show many outliers and 
## the point 14 indicates the highest leverage point.

# (e) Use the * and : symbols to ﬁt linear regression models with interaction eﬀects. 
#     Do any interactions appear to be statistically signiﬁcant? 

linearRegressionModel = lm(mpg ~ horsepower * displacement + displacement * weight, data = auto[, 1:8])
summary(linearRegressionModel)

## The correlation matrix shows displacement and weight are highly correlated therefore 
## their interaction is significant as they have low p-value and high t-statistic value.
## Moreover, mpg with horsepower, displacement are not correlated having not a linear relation
## and their interaction is significat as they have low p-value and high t-statistic value.

linearRegressionModel = lm(mpg ~ cylinders * displacement + displacement * weight, data = auto[,-9])
summary(linearRegressionModel)

## The correlation matrix shows cylinders and displacement, displacement and weight are highly correlated
## and the interaction is significant as the summary shows they have low p-value and high t-statistic value.

# (f) Try a few diﬀerent transformations of the variables, such as log(X), √X, X2. 
#     Comment on your ﬁndings.

## The transformations show similar pattern  with log and sqrt transformation almost 
## similar and square transformation clustered.
par(mfrow = c(2,2))
plot(sqrt(auto$weight), auto$mpg)
plot((auto$weight)^2, auto$mpg)
plot(log(auto$weight), auto$mpg)

## The transformations show similar pattern with log transformation dstributed 
## sqrt transformation clustered and square transformation most clustered.
par(mfrow = c(2, 2))
plot(log(auto$horsepower), auto$year)
plot(sqrt(auto$horsepower), auto$year)
plot((auto$horsepower)^2, auto$year)

## The transformations show similar pattern with the log being most scattered 
## sqrt transformation little clustered and square transformation most clustered and 
## displacement and cylinders are positively correlated.
par(mfrow = c(2,2))
plot(sqrt(auto$displacement), auto$cylinders)
plot((auto$displacement)^2, auto$cylinders)
plot(log(auto$displacement), auto$cylinders)

## The transformations show completely similar patterns for all the three transformation.
par(mfrow = c(2,2))
plot(sqrt(auto$year), auto$mpg)
plot((auto$year)^2, auto$mpg)
plot(log(auto$year), auto$mpg)