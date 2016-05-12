# STAT 215A Project
rm(list=ls())

###############################EDA
# Load RData File
load("./FinalProj/fMRIclass.RData")
ls()
dim(fit_feat)
N <- dim(fit_feat)[1]
P <- dim(fit_feat)[2]

# Naive Data Cleaning
# Remove the zero items: P=10409
head(fit_feat)
ind.zero <- c()
for (j in 1:P){
	score <- sum(fit_feat[,j]^2)
	if (score==0){
		ind.zero <- c(ind.zero, j)
	}
}
fit_feat_sc <- fit_feat[,-ind.zero]
dim(fit_feat_sc)
N <- dim(fit_feat_sc)[1]
P <- dim(fit_feat_sc)[2]

# Outlier Removal
samp.norm <- c()
ind.out <- c()
for (i in 1:N){
	nm <- sum(fit_feat[i,]^2)
	samp.norm <- c(samp.norm, nm)
	if (nm>800 || nm<100){
		ind.out <- c(ind.out, i)
	}
}
hist(samp.norm, main = "Histogram of Norm of Image")
boxplot(samp.norm, main = "Boxplot of Norm of Image")
length(ind.out)
fit_feat_ou <- fit_feat_sc[-ind.out,]
dim(fit_feat_ou)
N <- dim(fit_feat_ou)[1]
P <- dim(fit_feat_ou)[2]

# Data Partitioning
ind <- sample(1:N, N)
ind.train <- ind[1:(N*0.6)]
ind.valid <- ind[(N*0.6+1):(N*0.8)]
ind.test <- ind[(N*0.8+1):N]

fit_data_sc <- fit_data[,-ind.zero]
fit_data_ou <- fit_data_sc[-ind.out,]
dim(fit_data_ou)

##############################Prediction
# LASSO Regression
library(glmnet)
s.list <- seq(from=0.01, to=0.5, by=0.02)
cor.lasso <- vector("list", length=15)
max.cor <- c()
max.s <- c()
for (n in 1:length(cor.lasso)){
	lasso.D1 <- glmnet(x=fit_feat_ou[ind.train, ], y=fit_data_ou[ind.train, n], family="gaussian", alpha=1);
	for (k in 1:length(s.list)){
		predict.D1 <- predict(lasso.D1, fit_feat_ou[ind.valid, ], s=s.list[k])
		cor.lasso[[n]] <- c(cor.lasso[[n]], cor(predict.D1, fit_data_ou[ind.valid, n]))
	}
	max.cor <- c(max.cor, max(cor.lasso[[n]]))
	max.s <- c(max.s, s.list[which.max(cor.lasso[[n]])])	
}
max.cor <- c()
max.s <- c()
for (n in 1:length(cor.lasso)){
	max.cor <- c(max.cor, max(cor.lasso[[n]], na.rm=T))
	max.s <- c(max.s, s.list[which.max(cor.lasso[[n]])])	
}
par(mfrow=c(5,3), mar=c(2,2,1,0)+.5, mgp=c(1.6,.6,0))
for (n in 1:length(cor.lasso)){
	plot(s.list, cor.lasso[[n]], xlab="Penalty Coefficient", ylab="Correlation", main=paste("Voxel", " # ",as.character(n), sep=""), type="b", col="Dark Blue", lwd=2)
}
max(max.cor)

# Diagnostic
lasso.D1V1 <- glmnet(x=fit_feat_ou[ind.train, ], y=fit_data_ou[ind.train, 1], family="gaussian", alpha=1);
par(mfrow=c(1,2))
plot(s.list, cor.lasso[[1]], xlab="Penalty Coefficient", ylab="Correlation", main="Voxel #1", type="b", col="Dark Blue", lwd=2)
plot(fit_data_ou[ind.test,1],predict(lasso.D1V1, fit_feat_ou[ind.test, ], s=max.s[1]), xlab="Observed data", ylab="Lasso Fitted value", main="Voxel #1 chosen model")

cor(predict(lasso.D1V1, fit_feat_ou[ind.test, ], s=max.s[1]), fit_data_ou[ind.test, 1])
max(cor.lasso[[1]], na.rm=T)



# Partial Least Square Regression (PLS)
library(pls)
item.list <- c(1:9, seq(from=10, to=80, by=5))
cor.pls <- vector("list", length=15)
max.cor <- c()
max.comp <- c()
for (n in 1:length(cor.pls)){
	input <- scale(fit_feat_ou[ind.train, ])
	pls.D1 <- plsr(fit_data_ou[ind.train, n]~input, ncomp=max(item.list))
	for (k in 1:length(item.list)){
		pred.pls <- predict(pls.D1, fit_feat_ou[ind.valid, ], ncomp=item.list[k])
		cor.pls[[n]] <- c(cor.pls[[n]], cor(pred.pls, fit_data_ou[ind.valid, n]))
	}
	max.cor <- c(max.cor, max(cor.pls[[n]]))
	max.comp <- c(max.comp, item.list[which.max(cor.pls[[n]])])
}
par(mfrow=c(5,3), mar=c(2,2,1,0)+.5, mgp=c(1.6,.6,0))
for (n in 1:length(cor.pls)){
	plot(item.list, cor.pls[[n]], xlab="Included Parameters M", ylab="Correlation", main=paste("Voxel", " # ",as.character(n), sep=""), type="b", col="Dark Blue", lwd=2)
}
max(max.cor)

# Diagnostic
pls.D1V1 <- plsr(fit_data_ou[ind.train, 1]~input, ncomp=max(item.list))

par(mfrow=c(1,2))
plot(item.list, cor.pls[[1]], xlab="Included Parameters M", ylab="Correlation", main="Voxel #1 ", type="b", col="Dark Blue", lwd=2)
plot(fit_data_ou[ind.test,1],predict(pls.D1V1, fit_feat_ou[ind.test, ], ncomp=item.list[1]), xlab="Observed data", ylab="Lasso Fitted value", main="Voxel #1 chosen model")

cor(predict(pls.D1V1, fit_feat_ou[ind.test, ], ncomp=item.list[1]), fit_data_ou[ind.test, 1])
max(cor.pls[[1]], na.rm=T)



# L2-Epsilon Boosting Algorithm
epsilon <- 0.05
X <- fit_feat_ou[ind.train, ]
X <- scale(X, center=TRUE, scale=FALSE)

cor.L2 <- vector("list", length=15)
STEP <- 50
max.step <- c()
max.cor <- c()
for (m in 1:length(cor.L2)){
	colum <- m
	U <- fit_data_ou[ind.train, colum]
	Beta <- rep(0, dim(X)[2])
	for (n in 1:STEP){
		U <- U-(X%*%Beta)[,1]
		tp <- c()
		for (j in 1:length(Beta)){
			tp <- c(tp, abs(cor(U, X[,j])))
		}
		jmax <- which.max(tp)
		Beta[jmax] <- Beta[jmax] + epsilon*sign(cor(U, X[,jmax]))
		pred.L2 <- fit_feat_ou[ind.valid, ]%*%Beta
		cor.L2[[m]] <- c(cor.L2[[m]], cor(pred.L2, fit_data_ou[ind.valid, colum]))
	}
	max.step <- c(max.step, which.max(cor.L2[[m]]))
	max.cor <- c(max.cor, max(cor.L2[[m]]))
}
par(mfrow=c(5,3), mar=c(2,2,1,0)+.5, mgp=c(1.6,.6,0))
for (n in 1:length(cor.L2)){
	plot(1:STEP, cor.L2[[n]],xlab="STEPS", ylab="Correlation", main=paste("Voxel", " # ",as.character(n), sep=""), type="b", col="Dark Blue", lwd=2)
}
max(max.cor)


# Diagnostic
epsilon <- 0.05
X <- fit_feat_ou[ind.train, ]
X <- scale(X, center=TRUE, scale=FALSE)
colum <- 1
U <- fit_data_ou[ind.train, colum]
Beta <- rep(0, dim(X)[2])
cor.L2 <- c()
j.list <- c()
STEP <- 50
for (n in 1:STEP){
	U <- U-(X%*%Beta)[,1]
	tp <- c()
	for (j in 1:length(Beta)){
		tp <- c(tp, abs(cor(U, X[,j])))
	}
	jmax <- which.max(tp)
	j.list <- c(j.list, jmax)
	Beta[jmax] <- Beta[jmax] + epsilon*sign(cor(U, X[,jmax]))
	pred.L2 <- fit_feat_ou[ind.valid, ]%*%Beta
	cor.L2 <- c(cor.L2, cor(pred.L2, fit_data_ou[ind.valid, colum]))
}
which.max(cor.L2)
max(cor.L2)


U <- fit_data_ou[ind.train, colum]
Beta <- rep(0, dim(X)[2])
cor.L2V1 <- c()
j.listV1 <- c()
STEP <- which.max(cor.L2)
for (n in 1:STEP){
	U <- U-(X%*%Beta)[,1]
	tp <- c()
	for (j in 1:length(Beta)){
		tp <- c(tp, abs(cor(U, X[,j])))
	}
	jmax <- which.max(tp)
	j.list <- c(j.list, jmax)
	Beta[jmax] <- Beta[jmax] + epsilon*sign(cor(U, X[,jmax]))
	pred.L2V1 <- fit_feat_ou[ind.test, ]%*%Beta
	cor.L2V1 <- c(cor.L2V1, cor(pred.L2V1, fit_data_ou[ind.test, colum]))
}
which.max(cor.L2V1)
max(cor.L2V1)

par(mfrow=c(1,2))
plot(1:50, cor.L2, xlab="STEPS", ylab="Correlation", main="Voxel #1", type="b", col="Dark Blue", lwd=2)
plot(fit_data_ou[ind.test,1],pred.L2V1, xlab="Observed data", ylab="Lasso Fitted value", main="Voxel #1 chosen model")

cor(pred.L2V1, fit_data_ou[ind.test, 1])




#############################Validation
# Prediction for val_feat
val_feat_sc <- val_feat[,-ind.zero]
dim(val_feat_sc)
M <- dim(val_feat_sc)[1]
P <- dim(val_feat_sc)[2]
s.sel <- max.s
predv1 <- c()
for (n in 1:length(s.sel)){
	lasso.D1 <- glmnet(x=fit_feat_ou[ind.train, ], y=fit_data_ou[ind.train, n], family="gaussian", alpha=1);
	predict.D1 <- predict(lasso.D1, val_feat_sc, s=s.sel[n])
	predv1 <- cbind(predv1, predict.D1)
}
write(t(predv1), file="predv1_walnut.txt", ncolumns=15)



############################## Interpretation
lasso.D1 <- glmnet(x=fit_feat_ou[ind.train, ], y=fit_data_ou[ind.train, 1], family="gaussian", alpha=1);
cor.list <- c()
for (i in 1:length(lasso.D1$lambda)){
	target <- cor((fit_feat_ou[ind.valid, ]%*%(lasso.D1$beta[,i]))[,1], fit_data_ou[ind.valid, 1])
	cor.list <- c(cor.list, target)
}
cor.fin.max=which.max(cor.list)
which.max(lasso.D1$beta[,cor.fin.max])
head(order(lasso.D1$beta[,cor.fin.max], decreasing=TRUE))
head(sort(lasso.D1$beta[,cor.fin.max], decreasing=TRUE))
















