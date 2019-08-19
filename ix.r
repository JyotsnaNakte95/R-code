##	Exercise	10.7.10
##	Jyotsna Nakte
##	Summer	2019

# In this problem, you will generate simulated data, and then perform PCA and K-means clustering on the data.
# (a) Generate a simulated data set with 20 observations in each of three classes (i.e. 60 observations total), 
#     and 50 variables.

# Hint: There are a number of functions in R that you can use to generate data. One example is the rnorm() function; 
# runif() is another option. Be sure to add a mean shift to the observations in each class so that there are three distinct classes.
set.seed(1)
X1<-matrix(rnorm(20*50, mean = 0), nrow = 20)
X2<- matrix(rnorm(20*50, mean=0.7), nrow = 20)
X3<- matrix(rnorm(20*50, mean=1.4), nrow = 20)
X <- rbind(X1,X2,X3)
true.labels<-c( rep(1,20), rep(2,20), rep(3,20))

# (b) Perform PCA on the 60 observations and plot the first two principal component score vectors. 
#     Use a different color to indicate the observations in each of the three classes. If the three classes 
#     appear separated in this plot, then continue on to part (c). If not, then return to part (a) and modify 
#     the simulation so that there is greater separation between the three classes. Do not continue to part (c) 
#     until the three classes show at least some separation in the first two principal component score vectors.

pr.out = prcomp(X)$x
summary(pr.out)
plot(pr.out[, 1:2],  col=true.labels, xlab = "Z1", ylab = "Z2", pch = 19)

## ![plot for three classes of observations](ixa.pdf)

##  Trying multiple variations on the data in part (a) we got plot with three different clusters which are represented in 
##  by three separated class in colors black, red, green.


# (c) Perform K-means clustering of the observations with K = 3. How well do the clusters that you obtained 
#     in K-means cluster- ing compare to the true class labels?
# Hint: You can use the table() function in R to compare the true class labels to the class labels obtained by 
# clustering. Be careful how you interpret the results: K-means clustering will arbitrarily number the clusters, 
# so you cannot simply check whether the true class labels and clustering labels are the same.

km.out <- kmeans(X, 3)
table(true.labels, km.out$cluster)

##  K-means clustering with k=3 we see that the observations are perfectly clustered.  

# (d) Perform K-means clustering with K = 2. Describe your results.

km.out <- kmeans(X, 2, nstart = 20)
table(true.labels, km.out$cluster)

##  The middle class is forced to a wrong class. The extreme classes are classified correctly
##  We can see that the data has been divided into two clusters using k=2 and the obervations show that the class 1 and 3
##  the extreme classes are clustered properly. The middle class 2 is being clustered to wrong classes.


# (e) Now perform K-means clustering with K = 4, and describe your
#     results.

km.out <- kmeans(X, 4, nstart = 20)
table(true.labels, km.out$cluster)

## The table observations with K=4 split one of the classes into 2 classes with count 9 and 11 with more clusters than intended.

# (f) Now perform K-means clustering with K = 3 on the first two principal component score vectors, rather than on the 
#     raw data. That is, perform K-means clustering on the 60 × 2 matrix of which the first column is the first principal 
#     component score vector, and the second column is the second principal component score vector. Comment on the results.

km.out <- kmeans(pr.out[, 1:2], 3, nstart = 20)
table(true.labels, km.out$cluster)

## The first two principal component score vectors have perfectly clustered results for K=3.
## The first two principal components could be observed in (b) through summary(pr.out) 


# (g) Using the scale() function, perform K-means clustering with K = 3 on the data after scaling each variable to have standard 
#     deviation one. How do these results compare to those obtained in (b)? Explain.

km.out <- kmeans(scale(X), 3, nstart = 20)
table(true.labels, km.out$cluster)

##  After scaling the data we get perfect clusters for k=3, scaling gave similar results as above. The observations are equally divided
##  into 3 clusters. Scaling improves clustering or maintains the result but does not worsen the results. It is a good practise to
##  scale data before clustering as Euclidean distance is sensitive to any changes in the differences.