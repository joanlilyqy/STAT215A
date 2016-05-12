

image1.full <- image1;
image1 <- subset(image1, expert !=0);


## functions for LDA and QDA are in MASS package
library(MASS)
?lda
?qda

## MODEL SELECTION USING AIC OR BIC
# in general, BIC (bayesian information criterion) is more conservative than AIC
# the output of this is actually kind of confusing to me atm and i haven't had time to figure it out more thoroughly, but i have used it before with success...
stepAIC(glm(as.factor(expert) ~ ., data=image1, subset=sample(1:nrow(image1), 5000), family=binomial(link="logit")))



## SOME COMPUTATIONAL NOTES...
## K-FOLD CROSS-VALIDATION CODE
# you don't want to have to define five different training sets; much better to use tools like lapply and mapply in R
# lapply = apply function to each element of a list
# mapply = multivariate version of apply()


#### determining optimal cutoff for lda using v-fold CV
# vector of thresholds to try
cutoffs <- seq(0, 1, 0.1);
# number of folds, v
v <- 4

#### this chunk of code is for obtaining a permutation of your data, so you can split into subsets
#### important: evaluate whether this actually makes sense for data of your type!
# permute rows of image 1
permutation <- sample(1:nrow(image1), nrow(image1), replace=FALSE)
# set group size so that groups are approximately equal in size
groupsize <- floor(nrow(image1)/v)
# groups is a v-element list. each element of the list is a vector of indices, taken from permutation vector above
groups <- lapply(as.data.frame(matrix(permutation[1:(groupsize*v)], ncol=v)), function(x){x})
# tack on the remainder elements of the data to the fourth group, in case the split into v groups was not even
groups[[v]] <- c( groups[[v]], tail(permutation, length(permutation)-(groupsize*v)))


#### cross-validation functions
cpvalidate <- function(thiscp){
  # fit lda model on image1 sans one group
  fit.lda <- function(agroup){lda(expert ~ NDAI + CORR + SD, data = image1[-agroup,])}
  # lda.cv is a v-element list, each element being a model object trained on the data sans that group 
  lda.cv <- lapply(groups, fit.lda)
  
  # function for obtaining fitted values (posterior probabilities) on test set. for clarification:
  # class 1 = those with posterior probability > thiscp
  # class 0 = those with posterior probability <= thiscp
  pred.lda <- function(mod, test){
    2*(as.numeric(predict(mod, image1[test,])$posterior[,2] > thiscp))-1;}
  
  # mapply takes in a function as its first argument; the remaining arguments are arguments to the given function. so this is performing predrpart(rpartcv[1], groups[1]), predrpart(rpartcv[2], groups[2]), etc. 
  lda.pred <- mapply(pred.lda, lda.cv, groups, SIMPLIFY=FALSE)
  
  # function for calculating prediction error
  # note that the table function used here is also valuable for looking at the kinds of errors you're getting, period
  # so you may just want to use that on its own
  # ex: table(predicted = rpartpred[1], actual = image1$expert[test]);
  calcerror <- function(pred, test){
    1 - sum(diag( table(actual=image1$expert[test], predicted=pred)))/length(test)
  }
  
  # after all of that...return average prediction error over the v folds
  mean(mapply(calcerror, lda.pred, groups))
}

image1.4foldcv <- sapply(cutoffs, cpvalidate)

# plot results of 4-fold cv
plot(cutoffs,  image1.4foldcv, pch=19)


# LOGISTIC REGRESSION DIAGNOSTICS
# discussed in class yesterday
# hosmer-lemeshow goodness of fit test (the binned residuals)
# motivation is to use fact that, under logistic regression model, we know expectation of y is exp(t(X) %*% beta)/(1 + exp(t(x) %*% beta))
	# group the fitted values into c classes of roughly equal size (c is typically between 6 to 10)
	# calculate the observed and expected values under the logistic regression model
	# perform a chi-sq. goodness of fit test

logit.model <- glm(as.factor(expert) ~ NDAI + CORR + SD, data = image1, family=binomial(link="logit"))
# extract fitted values on image1
logit.fitted <- fitted(logit.model);
# break this up into bins (factor) by 10% quantile increments
# clarification: this is a vector of fitted value quantiles
c(0, quantile(logit.fitted, p = seq(0.1, 0.9, 0.1)), 1)
logit.binned <- cut(logit.fitted, br = c(0, quantile(logit.fitted, p = seq(0.1, 0.9, 0.1)), 1));
# observe that the result is a factor with 10 levels
levels(logit.binned)
# let's do that again but take off the labels for easier manipulation (labels=F gives us numeric labels)
logit.binned <- cut(logit.fitted, br = c(0, quantile(logit.fitted, p = seq(0.1, 0.9, 0.1)), 1), labels=F);

# how many are in each bin? 
table(logit.binned);
# ^ looks equal-sized, great!

# now calculate expected/observed values to perform chi-sq. test
logit.exp <- matrix(0, nrow=10, ncol=2);
colnames(logit.exp) <- c("1-pi", "pi");
logit.obs <- matrix(0, nrow=10, ncol=2);
colnames(logit.obs) <- c("1-y", "y");
for(j in 1:10){
	logit.exp[j, 2] = sum(logit.fitted[logit.binned == j]);
	logit.exp[j, 1] = sum((1 - logit.fitted)[logit.binned == j]);
	logit.obs[j, 2] = with(image1, sum(((1+expert)/2)[logit.binned == j]));
	logit.obs[j, 1] = with(image1, sum(((1 - expert)/2)[logit.binned == j]))}
	
# calculate chi-sq. statistic
sum( (logit.obs - logit.exp)^2/logit.exp )
# this follows chi.sq distribution with (# of levels) - 2 df
1 - pchisq(sum( (logit.obs - logit.exp)^2/logit.exp ), 8);
# really really significant
# our model doesn't fit very well...


## OTHER KINDS OF DIAGNOSTICS
## also you can look at various kinds of residuals
## deviance residuals
logit.dev <- residuals(logit.model, type = "deviance")
plot(logit.fitted, logit.dev, pch=46, xlab="Posterior probabilities", ylab="Deviance residuals", ylim=c(-4, 4));
abline(h = 0, col="grey");
abline(h =c(-3,3), col="green");
# do any observations jump out as having high deviance? no
# also, we see fairly good homoscedasticity

## standardized residuals should have roughly a N(0,1) distribution
logit.stand <- rstandard(logit.model);
qqnorm(logit.stand, pch=46);
# standardized residuals here indicate that the deviation from normal (in general) is not too bad (fairly straight). but not N(0,1)...

## there are other diagnostic plots to make but I need to look into them more carefully
## in particular, i think standardized deviance residuals should be compared against the explanatory variables


