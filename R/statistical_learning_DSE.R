##################################################################
###################### STATISTICAL LEARNING ######################
##################################################################

##### MAXIMUM LIKELIHOOD ESTIMATOR #####
#R gives us also the confidence interval, hessian matrix ecc
#instead of max the log likelihood, it min -likelihood. many of the software do it

set.seed(123)
install.packages('stats4')
library('stats4') #MLE function is inside this library

x = rnorm(100,5,4) #we generate a random variable #we define the POPULATION MEAN and ST DEV
hist(x)
log.lik = function(mu=1, sigma=1)
  -sum(dnorm(x,mean=mu,sd=sigma,log=TRUE)) #we want to use the log, that's why we assign T.
#we want to use dnorm (density distribution -> continuous). we assign the shape of the normal
#and initialization of parameters

fit = mle(log.lik, lower=c(0,0), method='L-BFGS-B') #mle of the log likelihood of x. we want to max
#the prob of the observed sample. in this case we fix the sample observed 18 ??
#we define a positive range of the parameter
fit #we obtain
#mu=5.36 (very close to 5, we generated the data with 5)
#sigma=3.63 (close to 4, for which we generated the data)

#we compute the SAMPLE MEAN and ST DEV with them: result when we use the classical estimator
#the MLE converges to the estimator we usually know. the std dev is different since in sample
#st dev we use n-1, with MLE we use n. but both are consistent
mean(x)
sd(x)

logLik(fit) #the object fit is not a simple vector, but it contains other info, such as the value
#of likelihood. 'df' is related to flexibility of the model (n. of parameters) and '- log lik'
vcov(fit) #variance of fit -> hessian matrix
confint(fit) #confidence interval. if we know the shape and the variance, we can compute the
#confidence interval -> (paramter +/- std error of parameter)*percentile of distribution of parameter
#fixing alpha
#we know:
# - std error: inverse of fisher
# - shape: asymptotically normal 28:00
#the confint length is a measure of error. the value of the estimator is true in the population depending
#on the variability. if the interval of the parameter includes 0, it means that it can be also 0,
#so not needed
summary(fit) #summary of all the results. '-2 log L'
