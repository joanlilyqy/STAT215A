###### Lab_3.R
rm(list=ls())
getwd()
setwd("D:/kangzy/Fall 2011/STAT 215A/Homework/HW3")

## load the data
image1 <- read.table("image1.txt", header=F)
names(image1) <- c("y", "x", "expert", "NDAI", "SD", "CORR", "DF", "CF", "BF", "AF", "AN");

## images aren't rectangular; make them so
image1 <- image1[image1$x >= 70,]
image1 <- image1[image1$y <= 368,]
image1 <- image1[image1$x <= 368,]

## FILLED CONTOUR PLOTS
## to see what sorts of features each of these statistics picks out
## plot expert label
jpeg('1-1_Image1.jpg')
image(70:368, 2:368, matrix(image1$expert[order(-image1$y, image1$x)], 368-70+1, 368-2+1), col=heat.colors(3), main="Expert Labels for Image 1", xlab="x", ylab="y");
dev.off()
# cloud: yellow
# no cloud: red
# unlabeled: orange

## load Other data
image2 <- read.table("image2.txt", header=F)
names(image2) <- c("y", "x", "expert", "NDAI", "SD", "CORR", "DF", "CF", "BF", "AF", "AN");
image2 <- image2[image2$x >= 70,]
image2 <- image2[image2$y <= 368,]
image2 <- image2[image2$x <= 368,]
jpeg('1-1_Image2.jpg')
image(70:368, 2:368, matrix(image2$expert[order(-image2$y, image2$x)], 368-70+1, 368-2+1), col=heat.colors(3), main="Expert Labels for Image 2", xlab="x", ylab="y");
dev.off()

image3 <- read.table("image3.txt", header=F)
names(image3) <- c("y", "x", "expert", "NDAI", "SD", "CORR", "DF", "CF", "BF", "AF", "AN");
image3 <- image3[image3$x >= 70,]
image3 <- image3[image3$y <= 368,]
image3 <- image3[image3$x <= 368,]
jpeg('1-1_Image3.jpg')
image(70:368, 2:368, matrix(image3$expert[order(-image3$y, image3$x)], 368-70+1, 368-2+1), col=heat.colors(3), main="Expert Labels for Image 3", xlab="x", ylab="y");
dev.off()

####----------------------------------
## Function that can be used to produce printed correlations in
## lower-half panels of pairwise scatterplot diagrams.
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * r)
}
####----------------------------------
## put histograms on the diagonal
panel.hist <- function(x, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}

panel.user <- function(x, y, ...)
{
    points(x, y, col="#00000020", ...)
}

#### Image1
jpeg('1-1_Pair_1.jpg')
pairs(image1[,7:11], col="#00000027", lower.panel=panel.cor)
dev.off()
jpeg('1-1_Pair_Radiance_Cloud.jpg')
pairs(image1[image1$expert==1,7:11], upper.panel=panel.user, lower.panel=panel.cor, diag.panel=panel.hist)
dev.off()
jpeg('1-1_Pair_Radiance_noCloud.jpg')
pairs(image1[image1$expert==-1,7:11], upper.panel=panel.user, lower.panel=panel.cor, diag.panel=panel.hist)
dev.off()
jpeg('1-1_Pair_ND_Cloud.jpg')
pairs(image1[image1$expert==1,4:6], upper.panel=panel.user, lower.panel=panel.cor, diag.panel=panel.hist)
dev.off()
jpeg('1-1_Pair_ND_noCloud.jpg')
pairs(image1[image1$expert==-1,4:6], upper.panel=panel.user, lower.panel=panel.cor, diag.panel=panel.hist)
dev.off()


### Image2
jpeg('1-1_Pair_Image2_Radiance_Cloud.jpg')
pairs(image2[image2$expert==1,7:11], upper.panel=panel.user, lower.panel=panel.cor, diag.panel=panel.hist)
dev.off()
jpeg('1-1_Pair_Image2_Radiance_noCloud.jpg')
pairs(image2[image2$expert==-1,7:11], upper.panel=panel.user, lower.panel=panel.cor, diag.panel=panel.hist)
dev.off()
jpeg('1-1_Pair_Image2_ND_Cloud.jpg')
pairs(image2[image2$expert==1,4:6], upper.panel=panel.user, lower.panel=panel.cor, diag.panel=panel.hist)
dev.off()
jpeg('1-1_Pair_Image2_ND_noCloud.jpg')
pairs(image2[image2$expert==-1,4:6], upper.panel=panel.user, lower.panel=panel.cor, diag.panel=panel.hist)
dev.off()

### Image3
jpeg('1-1_Pair_Image3_Radiance_Cloud.jpg')
pairs(image3[image3$expert==1,7:11], upper.panel=panel.user, lower.panel=panel.cor, diag.panel=panel.hist)
dev.off()
jpeg('1-1_Pair_Image3_Radiance_noCloud.jpg')
pairs(image3[image3$expert==-1,7:11], upper.panel=panel.user, lower.panel=panel.cor, diag.panel=panel.hist)
dev.off()
jpeg('1-1_Pair_Image3_ND_Cloud.jpg')
pairs(image3[image3$expert==1,4:6], upper.panel=panel.user, lower.panel=panel.cor, diag.panel=panel.hist)
dev.off()
jpeg('1-1_Pair_Image3_ND_noCloud.jpg')
pairs(image3[image3$expert==-1,4:6], upper.panel=panel.user, lower.panel=panel.cor, diag.panel=panel.hist)
dev.off()

##### 2-1 Features that is important for indicating Cloud or No Cloud
##### Image1
image.list <- c("y", "x", "expert", "NDAI", "SD", "CORR", "DF", "CF", "BF", "AF", "AN");
jpeg('2-1_Hist_Image1.jpg')
image1.score <- rep(0, 8)
par(mfrow=c(2,4), mar=c(2,2,1,0)+.5, mgp=c(1.6,.6,0))
for(i in 4:11){
	x <- hist(image1[image1$expert==1,i], breaks=20, plot=FALSE)
	y <- hist(image1[image1$expert==-1,i], breaks=20, plot=FALSE)
	plot(x, col="red", ylim=c(0,1.1*max(max(x$density),max(y$density))), freq=FALSE, main=NULL, xlab=NULL)
	plot(y, col=rgb(0,0,1,.5), add=T, freq=FALSE, main=NULL, xlab=NULL)
	title(paste("The Histogram of",image.list[i],"in Image1",sep=" "), cex.main =1.5)
	a <- image1[image1$expert==1,i]
	b <- image1[image1$expert==-1,i]
	w <- t.test(a,b)
	image1.score[i-3] <- w$statistic
}
par(mfrow=c(1,1))
dev.off()

##### Image2
jpeg('2-1_Hist_Image2.jpg')
image2.score <- rep(0, 8)
par(mfrow=c(2,4), mar=c(2,2,1,0)+.5, mgp=c(1.6,.6,0))
for(i in 4:11){
	x <- hist(image2[image2$expert==1,i], breaks=20, plot=FALSE)
	y <- hist(image2[image2$expert==-1,i], breaks=20, plot=FALSE)
	plot(x, col="red", ylim=c(0,1.1*max(max(x$density),max(y$density))), freq=FALSE, main=NULL, xlab=NULL)
	plot(y, col=rgb(0,0,1,.5), add=T, freq=FALSE, main=NULL, xlab=NULL)
	title(paste("The Histogram of",image.list[i],"in Image2",sep=" "), cex.main =1.5)
	a <- image2[image2$expert==1,i]
	b <- image2[image2$expert==-1,i]
	w <- t.test(a,b)
	image2.score[i-3] <- w$statistic
}
par(mfrow=c(1,1))
dev.off()

##### Image3
jpeg('2-1_Hist_Image3.jpg')
image3.score <- rep(0, 8)
par(mfrow=c(2,4), mar=c(2,2,1,0)+.5, mgp=c(1.6,.6,0))
for(i in 4:11){
	x <- hist(image3[image3$expert==1,i], breaks=20, plot=FALSE)
	y <- hist(image3[image3$expert==-1,i], breaks=20, plot=FALSE)
	plot(x, col="red", ylim=c(0,1.1*max(max(x$density),max(y$density))), freq=FALSE, main=NULL, xlab=NULL)
	plot(y, col=rgb(0,0,1,.5), add=T, freq=FALSE, main=NULL, xlab=NULL)
	title(paste("The Histogram of",image.list[i],"in Image3",sep=" "), cex.main =1.5)
	a <- image3[image3$expert==1,i]
	b <- image3[image3$expert==-1,]
	w <- t.test(a,b)
	image3.score[i-3] <- w$statistic
}
par(mfrow=c(1,1))
dev.off()

##### Measure of Correlation as a simple Criterion
for (i in 4:11){
	image1.score[i-3] <- abs(cor(image1$expert, image1[,i]))
	image2.score[i-3] <- abs(cor(image2$expert, image2[,i]))
	image3.score[i-3] <- abs(cor(image3$expert, image3[,i]))
}
min.pt <- min(min(min(image1.score),min(image2.score)), min(image3.score))
max.pt <- max(max(max(image1.score),max(image2.score)), max(image3.score))
plot(image1.score, xaxt="n", xlab=NULL, ylim=c(min.pt, max.pt), type="l", ylab="Absolute Correlation")
axis(1, at=1:8,labels=image.list[4:11])
points(image1.score)
lines(image2.score, col=2)
points(image2.score, col=2)
lines(image3.score, col=4)
points(image3.score, col=4)


###### Develop 1-0 Classifiers
##### Image1
image1.full <- image1;
image1 <- subset(image1, expert !=0);
image2.full <- image2;
image2 <- subset(image2, expert !=0);
image3.full <- image3;
image3 <- subset(image3, expert !=0);
library(MASS)


###### Different 1-0 Classifiers

# Type 1: LOGISTIC REGRESSION
logit.img1 <- glm(as.factor(expert)~NDAI+DF+CF+AF+AN+SD+CORR, data = image1, family=binomial(link="logit"))
summary(logit.img1)
logit.img1.fit <- fitted(logit.img1);


#### Model Selection
stepAIC(glm(as.factor(expert)~NDAI+SD+CORR+DF+CF+BF+AF+AN, data=image1, subset=sample(1:nrow(image1), 5000), family=binomial(link="logit")))

#### Model Diagnostics
# break this up into bins (factor) by 10% quantile increments
c(0, quantile(logit.img1.fit, p = seq(0.1, 0.9, 0.1)), 1)	#### Check
logit.img1.bin <- cut(logit.img1.fit, br = c(0, quantile(logit.img1.fit, p = seq(0.1, 0.9, 0.1)), 1));
levels(logit.img1.bin)
logit.img1.bin <- cut(logit.img1.fit, br = c(0, quantile(logit.img1.fit, p = seq(0.1, 0.9, 0.1)), 1), labels=F);
table(logit.img1.bin);

# now calculate expected/observed values to perform chi-sq. test
logit.img1.exp <- matrix(0, nrow=10, ncol=2);
colnames(logit.img1.exp) <- c("1-pi", "pi");
logit.img1.obs <- matrix(0, nrow=10, ncol=2);
colnames(logit.img1.obs) <- c("1-y", "y");
for(j in 1:10){
	logit.img1.exp[j, 2] = sum(logit.img1.fit[logit.img1.bin==j]);	#### Ej: Expected Number of Cases in jth bin
	logit.img1.exp[j, 1] = sum((1 - logit.img1.fit)[logit.img1.bin==j]);	#### nj-Ej
	logit.img1.obs[j, 2] = with(image1, sum(((1+expert)/2)[logit.img1.bin==j]));	#### Oj: Observed
	logit.img1.obs[j, 1] = with(image1, sum(((1-expert)/2)[logit.img1.bin==j]));	#### nj-Oj
}	
# calculate chi-sq. statistic
sum((logit.img1.obs-logit.img1.exp)^2/logit.img1.exp)
img1.stat <- sum((logit.img1.obs-logit.img1.exp)^2/logit.img1.exp)	#### Hosmer-Lemeshow Statistic
1 - pchisq(img1.stat, 8);	#### df=n-2=8


## OTHER KINDS OF DIAGNOSTICS
## deviance residuals
logit.img1.dev <- residuals(logit.img1, type="deviance")
plot(logit.img1.fit, logit.img1.dev, pch=46, xlab="Posterior probabilities", ylab="Deviance residuals", ylim=c(-4, 4));
abline(h=0, col="grey");
abline(h=c(-3,3), col="green");

## standardized residuals should have roughly a N(0,1) distribution
logit.img1.std <- rstandard(logit.img1);
qqnorm(logit.img1.std, pch=46);


##### Check of Residual Deviance
##### Relationship between Residual Deviance vs. Different Features
par(mfrow=c(2,4), mar=c(2,2,1,0)+.5, mgp=c(1.6,.6,0))
for(i in 4:11){
	plot(image1[,i], logit.img1.dev, type="p", col="light blue", main=NULL, xlab="Feature")
	title(paste(image.list[i],"vs. Residual Deviance",sep=" "), cex.main =1.5)
}
par(mfrow=c(1,1))

library(epiR)
epi.cp(logit.img1.dev)




#### Type 2: LDA
lda.img1 <- lda(expert~NDAI+DF+CF+AF+AN+SD+CORR, data=image1)
summary(lda.img1)
#### Can be used to construct Boundaries
prior.img1 <- lda.img1$prior
mu.img1 <- lda.img1$means
A.img1 <- lda.img1$scaling
sigma.img1 <- A.img1 %*% t(A.img1)

##### Model Selection: Cross-Validation
##### vector of thresholds to try
##### Threshold for Dividing the Group
cutoffs <- seq(0, 1, 0.1);
# number of folds, v
v <- 5

permutation <- sample(1:nrow(image1), nrow(image1), replace=FALSE)
groupsize <- floor(nrow(image1)/v)
groups <- lapply(as.data.frame(matrix(permutation[1:(groupsize*v)], ncol=v)), function(x){x})
groups[[v]] <- c(groups[[v]], tail(permutation, length(permutation)-(groupsize*v)))


#### cross-validation functions
cpvalidate <- function(thiscp){
  fit.lda <- function(agroup){lda(expert~NDAI+CORR+AF+AN+BF+SD, data = image1[-agroup,])}
  lda.cv <- lapply(groups, fit.lda)
  pred.lda <- function(mod, test){
    2*(as.numeric(predict(mod, image1[test,])$posterior[,2] > thiscp))-1;}
  lda.pred <- mapply(pred.lda, lda.cv, groups, SIMPLIFY=FALSE)
  calcerror <- function(pred, test){
    1 - sum(diag( table(actual=image1$expert[test], predicted=pred)))/length(test)
  }  
  mean(mapply(calcerror, lda.pred, groups))
}

image1.5foldcv <- sapply(cutoffs, cpvalidate)
plot(cutoffs,  image1.5foldcv, pch=19, xlab="Threshold Setting", ylab="Prediction Error")




##### Model Diagnostics


#### Type 3: QDA
qda.img1 <- qda(expert~NDAI+DF+CF+AF+AN+SD+CORR, data=image1)
summary(qda.img1)

## Sample-based estimates similar as 'lda', except now for each
## class different covariance-matrices:
A = qda.img1$scaling[,,1]
sigma.cloud.img1 = A%*%t(A)
sigma.cloud.img1


##### Model Selection: Cross-Validation
cutoffs <- seq(0, 1, 0.1);
v <- 5

permutation <- sample(1:nrow(image1), nrow(image1), replace=FALSE)
groupsize <- floor(nrow(image1)/v)
groups <- lapply(as.data.frame(matrix(permutation[1:(groupsize*v)], ncol=v)), function(x){x})
groups[[v]] <- c(groups[[v]], tail(permutation, length(permutation)-(groupsize*v)))


#### cross-validation functions
cpvalidate <- function(thiscp){
  fit.qda <- function(agroup){qda(expert~NDAI+CORR+AF+AN+BF+SD, data = image1[-agroup,])}
  qda.cv <- lapply(groups, fit.qda)
  pred.qda <- function(mod, test){
    2*(as.numeric(predict(mod, image1[test,])$posterior[,2] > thiscp))-1;}
  qda.pred <- mapply(pred.qda, qda.cv, groups, SIMPLIFY=FALSE)
  calcerror <- function(pred, test){
    1 - sum(diag( table(actual=image1$expert[test], predicted=pred)))/length(test)
  }  
  mean(mapply(calcerror, qda.pred, groups))
}

image1.qda.4foldcv <- sapply(cutoffs, cpvalidate)
plot(cutoffs,  image1.qda.4foldcv, pch=19, xlab="Threshold Setting", ylab="Prediction Error")



##### Model Diagnostics






