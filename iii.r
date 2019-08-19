## Exercise 4.7.11
## Jyotsna Nakte
## Summer 2019

# In this problem, you will develop a model to predict whether a given car gets 
# high or low gas mileage based on the Auto data set.

## Reading the Auto data set file

auto <- read.csv("C:/Users/jyots/Desktop/KDD/Homework03/Auto.csv", header=T, na.strings="?")
auto <- na.omit(auto)
summary(auto)

# (a) Create a binary variable, mpg01, that contains a 1 if mpg contains a value 
#     above its median, and a 0 if mpg contains a value below its median. You can 
#     compute the median using the median() function. Note you may ﬁnd it helpful 
#     to use the data.frame() function to create a single data set containing both 
#     mpg01 and the other Auto variables.

attach(auto)
mpg01 <- rep(1, length(mpg))
mpg01[mpg < median(mpg)] <- 0
auto <- data.frame(auto, mpg01)
summary(auto)

# (b) Explore the data graphically in order to investigate the association between 
#     mpg01 and the other features. Which of the other features seem most likely to 
#     be useful in predicting mpg01? Scatterplots and boxplots may be useful tools 
#     to answer this question. Describe your ﬁndings

cor(auto[,-9])

## The correlation matrix show variables like cylinders, displacement, horsepower, weight have 
## correlation score more than -0.6 which show how these variables could help predict the variable mpg01.

par(mfrow = c(2,2))
plot(x=auto$mpg01,y=auto$cylinders)
boxplot(cylinders ~ mpg01,xlab="mpg01", ylab="cylinders")
plot(x=auto$mpg01,y=auto$year)
boxplot(year ~ mpg01, xlab="mpg01", ylab="year")



## From the scatterplot and boxplots we can see there is more data accomodated of mpg01 as 0
## for cylinders greater than 6, as the cylinder value increase the mpg value is less than median.
## Moreover, as the value of cylinders is 4 the mpg is greater than median value.
## From second set of boxplot we find that mpg value is greater than median as the count of years go above this shows
## both variables are negatively correlated as shown from correlation matrix.  

par(mfrow = c(2,2))
plot(x=auto$weight, y=auto$mpg01)
boxplot(weight ~ mpg01, xlab="mpg01", ylab="Weight")
plot(x=auto$mpg01,y=auto$acceleration)
boxplot(acceleration ~ mpg01, xlab="mpg01", ylab="Acceleration")

## The plots of weight and mpg01 show as the value of weight increases the mpg value decreases that is less than median.
## The plots of acceleration and mpg01 show that as the acceleration increases the value of mpg 
## value increases above median shows how these variables are correlated in the same direction.

par(mfrow = c(2,2))
plot(x=auto$mpg01,y=auto$displacement)
boxplot(displacement ~ mpg01, xlab="mpg01", ylab="Displacement")
plot(x=auto$mpg01,y=auto$horsepower)
boxplot(horsepower ~ mpg01, xlab="mpg01", ylab="Horsepower" )

## The both plots show that as the displacement and horsepower increases the mpg value decreases which is less than median.
## The above findings show how cylinders, weight, displacement, horsepower are correlated 
## which could be used to predict mpg01 variable.

# (c) Split the data into a training set and a test set. 

trainingData = (year %% 2 == 0)
auto.trainingData <- auto[trainingData, ]
auto.testingData <- auto[!trainingData, ]
mpg01.testingData <- mpg01[!trainingData]
mpg01.testingData

# (d) Perform LDA on the training data in order to predict mpg01 using the variables 
#     that seemed most associated with mpg01 in (b). What is the test error of the 
#     model obtained? 

require(MASS)
ldaModel = lda(mpg01 ~ cylinders + weight + displacement + horsepower, data = auto, subset = trainingData)
ldaModel
pred.ldaModel <- predict(ldaModel, auto.testingData)
table(pred.ldaModel$class, mpg01.testingData)
mean(pred.ldaModel$class != mpg01.testingData)


## The test error of the model obtained is 12.64.

# (e) Perform QDA on the training data in order to predict mpg01 using the variables 
#     that seemed most associated with mpg01 in (b). What is the test error of the 
#     model obtained? 

qdaModel<-qda(mpg01 ~ cylinders + weight + displacement + horsepower, data = auto, subset = trainingData)
pred.qdaModel<-predict(qdaModel,auto.testingData)
table(pred.qdaModel$class,mpg01.testingData)
mean(pred.qdaModel$class!=mpg01.testingData)

## The test error of the model obtained is 13.19.

# (f) Perform logistic regression on the training data in order to predict mpg01 using 
#     the variables that seemed most associated with mpg01 in (b). What is the test error 
#     of the model obtained? 


lrModel <- glm(mpg01 ~ cylinders + weight + displacement + horsepower , data = auto, family = binomial, subset = trainingData)
sumary(lrModel)
lrModelProb <- predict(lrModel, auto.testingData, type = "response")
pred.lrModel <- rep(0, length(lrModelProb))
pred.lrModel[lrModelProb > 0.5] <- 1
table(pred.lrModel, mpg01.testingData)
mean(pred.lrModel != mpg01.testingData)

## The test error of the model obtained is 12.09.

# (g) Perform KNN on the training data, with several values of K, in order to predict mpg01. 
#     Use only the variables that seemed most associated with mpg01 in (b). What test errors 
#     do you obtain? Which value of K seems to perform the best on this data set?

library(class)
training = cbind(cylinders, weight, displacement, horsepower)[trainingData, ]
testing = cbind(cylinders, weight, displacement, horsepower)[!trainingData, ]
train.mpg01 = mpg01[trainingData]
set.seed(1)
errorList = {}
K = list(1,2,3,4,5,10,15,20,50,100)
for(i in K) {
  nearest <- knn(training, testing, train.mpg01, k=i)
  errorList[i] = mean(nearest != mpg01.testingData)
}

for(i in K) {
  cat("K = ",i,", Error = ",errorList[i],"\n")
}

## The various test errors are printed using for loop for various values of K.
## From the observation we notice the value k=3 seems to perform the best on the 
## data set with low error 13.74.