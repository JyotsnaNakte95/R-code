## Exercise 7.9.7
## Jyotsna Nakte
## Summer 2019

# The Wage data set contains a number of other features not explored in this chapter, 
# such as marital status (maritl), job class (jobclass), and others. Explore the relationships 
# between some of these other predictors and wage, and use non-linear fitting techniques in 
# order to fit flexible models to the data. Create plots of the results obtained, and write 
# a summary of your findings.

## install.packages("ISLR")
library("ISLR")
set.seed(1)
attach(Wage)
## Wage dataset attached

summary(maritl)
summary(race)
summary(jobclass)
summary(health)
summary(health_ins)

## Summaries of the attributes explored to understand the relationship of features.


plot(maritl, wage,xlab = "maritl", ylab = "wage")

## From the plot you can see there are five marital status which are never married, married, widowed, divorced,
## separated. The median of the wages of married class is more followed by Widowed, divorced, separated and never 
## married. You can see many outliers in the never married and married maritl with few on the divorced and separated.
## The Widowed class has no outliers and on the average the married seems to earn more than average amongst the other 
## classes.

plot(jobclass, wage, xlab = "jobclass", ylab = "wage")

## From the plot you can see there are two job classes industrial and information. The Information has higher 
## median than the industrial class. The graph depicts the class having more information about the job has 
## higher wages than the industrial class. The boxplot distribution seems uniform in both classes with clustered
## outliers.

plot(health, wage,xlab = "health", ylab = "wage")

## From the plot you can see that the wages median with the one with very good health is more. As you know if health is
## good, you tend to do the work by fresh minds and faster. The boxplots are uniformly distributed and median of both 
## classes are pretty close to each other.

plot(health_ins, wage.xlab = "health_ins", ylab = "wage")

## From the plot you can see that health insurance is directly related to wages, it can clearly be seen that 
## people having insurance have more wages compared to people not having health insurance.

plot(race, wage, xlab = "race", ylab = "wage")

## From the plot you can see there are four race classes considered in the data white, black, asian and others.
## The median of asian wages are highest amongst all followed by the white, black and others. The data amongst the 
## Asian is uniformly distributed earning the highest wages with few outliers that go above 300.
## Moreover, the boxplot of white black and others are skewed with white having the most clustered outliers around 270-300.
## The box plot of black and others have data distributed left skewed, white class slightly left skewed.


## The summary of the linear model gives us information of the formula, residual, coefficient of the predicted linear 
## model. The formula gives information about the predictor and response value. The residual are the difference between the 
## actual observed response values and the response values that the model predicted.
## The coefficients represent intercepts of the linear model with estimate, standard error, t.value and small p-value.

lm1=lm(wage ~ maritl, data = Wage)
summary(lm1)
## The lm1 model uses maritl as the predictor and wage as the target/response value from data Wage. The Residuals of 
## model gives the symmetrical distribution across the points using the five sections. Looking at the values the model
## does not seem to be distributed symmetrically that means that the model predicts certain points that fall far away 
## from the actual observed points. The coefficient here have different estimates to the classes with standard error 
## almost similar of married class, the tvalue measure how many standard deviation away from 0 used to calculate small 
## p-value.


lm2 = lm(wage ~ jobclass, data = Wage)
coef(summary(lm2))
## The lm2 model uses jobclass as predictor for response value wage, the standerror seems similar rest are quite different
## then the intercept values.

lm3 = lm(wage ~ maritl + jobclass, data = Wage)
coef(summary(lm3))

## Observing the three models the lm3 model using maritl and jobclass as predictor with response wage seems to work
## fit and predict better than the above previous model observing the t-value, standerror lesser than the above model
## for maritl. However, the jobclass seem to give approx similar but less value and seems fit better.

anova(lm1, lm2, lm3)

## anova command/function is used to observe the ANOVA table, that helps if features/ attributes are categorical having
## more than two levels. The linear model make it difficult to interept so anova tells us that the variables alone are not 
## perfectly uncorrelated but together make better model, the values are different as observed in summaries of linear model.
## We further explore the features using gam (Generalized Additive Models).


library(gam)

gamfit1 <- gam(wage ~ lo(year, span = 0.7) + s(age, 5) + education, data = Wage)
gamfit2 <- gam(wage ~ lo(year, span = 0.7) + s(age, 5) + education + jobclass, data = Wage)
gamfit3 <- gam(wage ~ lo(year, span = 0.7) + s(age, 5) + education + maritl, data = Wage)
gamfit4 <- gam(wage ~ lo(year, span = 0.7) + s(age, 5) + education + jobclass + maritl, data = Wage)
anova(gamfit1, gamfit2, gamfit3, gamfit4)

par(mfrow = c(2, 3))
plot(gamfit4, se = T, col = "blue")


## The jobclass and martial features are categorical/qualitative therefore we use anova table with age attribute with 5 degrees of freedom.
## As explained above the p-value suggests if the model is statistically significant or not, looking at the  p-value and deviance we 
## see that the models with marital status and job class have been improved significantly than the linear model predicted above as gam are
## generalized linear models with smoothing function. Here, we use wage as target/response value and predictors year,
## age, education, jobclass, maritl with year and age as a function i.e. smoothening function. We use various models to compare
## and find improved class here gamfit4 works improved and significant.
## Analysing both model the gam model is better fit to the data with jobclass and maritl used together as predictors.




