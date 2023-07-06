# make original sample of skewed data
X<-matrix(rgamma(100,2,1), nrow=100, ncol=1)

# some descriptives of the original sample
summary(X)
hist(X)


# use a seed to start the drawing of pseudo-random numbers
set.seed(101)

# take 1000 bootstrap samples and compute the median on each sample
# using the in-built sample() function.

nsamp<-1000
Median<-matrix(NA, nrow=nsamp, ncol=1)
for (i in 1:nsamp){
    Median[i] <- median(sample(X, replace=T))
     }

# compute the standard deviation of the bootstrap distribution of
# median values.
se.b<-sqrt(var(Median))
se.b

# median in original sample
median(X)
# mean of the bootstrap distribution of medians
mean(Median)
# bias between median in original sample and mean of median values
bias <- median(X) - mean(Median)
bias

# root mean squared error
RMSE <- sqrt((bias^2)+(var(Median)))
RMSE

# 95% percentile confidence interval of bootstrap distribution
CI <- quantile(Median, p = c(0.025, 0.975))
CI

# Make a histogram of bootstrap distribution of median with lower and upper limits of 
# 95% percentile confidence interval.

hist(Median, main='Bootstrap distribution median with 95% percentile confidence interval', 
nclass=50)
abline(v=CI, col="blue")

# compare histogram of bootstrap distribution with standard normal distribution.

par(mfrow=c(1,2))
hist(Median)
qqnorm(Median)
