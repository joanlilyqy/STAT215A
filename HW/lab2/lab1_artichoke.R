##### Identifier: artichoke
##### LAB #1 STAT 215A
##### A Macroscope In the Redwoods

rm(list=ls())

data.all <- read.csv("sonoma-data-all.csv", header=T, sep=",", row.names=NULL)



##### Data Cleaning
##### Step # 1
##### Work on the whole dataset

data.sc <- data.all[,c(2,3,5,7,8,10:11)]
attach(data.sc)

##### Network and Log have different scale in Voltage
list.net <- 1:114980
list.log <- 114981:length(voltage)

data.sc[list.log,][((voltage[list.log]>3)|(voltage[list.log]<2.4)), ] <- NA
attach(data.sc)
data.sc[((humidity>100)|(humidity<16.4))&(is.na(humidity)==F), ] <- NA
attach(data.sc)
data.sc[((humid_temp>32.6)|(humid_temp<6.6))&(is.na(humid_temp)==F), ] <- NA
attach(data.sc)
data.sc[(is.na(humid_temp)==TRUE), ] <- NA
attach(data.sc)



###### Removal of Duplications
###### There's no missing value in "epoch" and "nodeid"

data.sc[(duplicated(data.sc[,1:2])==TRUE), ] <- NA
attach(data.sc)


length(epoch[is.na(epoch)==TRUE])
length(voltage[is.na(voltage)==TRUE])
length(humid_temp[is.na(humid_temp)==TRUE])
length(humidity[is.na(humidity)==TRUE])
length(hamatop[is.na(hamatop)==TRUE])
length(hamabot[is.na(hamabot)==TRUE])


###### Whole Group Plots
###### Repeat Figure 3(a) in the paper

par(mfrow=c(2,2))
hist(humid_temp, freq=FALSE, main="Histogram of Temperature")
hist(humidity, freq=FALSE, main="Histogram of Humidity")
hist(hamatop, freq=FALSE, main="Histogram of Incident PAR")
hist(hamabot, freq=FALSE, main="Histogram of Reflected PAR")
par(mfrow=c(1,1))


###### Kernel Density Estimation

hist(humid_temp, freq=FALSE, ylim=c(0,.09), xlab="Temperature [Celsius]")
lines(density(humid_temp, bw=.5, kernel="gaussian", na.rm=TRUE), lty=2, col=1, lwd=2)
lines(density(humid_temp, bw=.5, kernel="cosine", na.rm=TRUE), lty=2, col=2, lwd=2)
lines(density(humid_temp, bw=.5, kernel="rectangular", na.rm=TRUE), lty=2, col=3, lwd=2)
lines(density(humid_temp, bw=.5, kernel="epanechnikov", na.rm=TRUE), lty=2, col=4, lwd=2)
lines(density(humid_temp, bw=.5, kernel="triangular", na.rm=TRUE), lty=2, col=5, lwd=2)
legend("topright", paste("kernel=", c("Gaussian","Cosine","Rectangular","Epanechnikov","Triangular"), sep=""), col=1:5, lty=2, lwd=2)


hist(humid_temp, freq=FALSE, ylim=c(0,.09), xlab="Temperature [Celsius]")
lines(density(humid_temp, bw=.5, kernel="gaussian", na.rm=TRUE), lty=2, col=1, lwd=2)
lines(density(humid_temp, bw=.1, kernel="gaussian", na.rm=TRUE), lty=2, col=2, lwd=2)
lines(density(humid_temp, bw=1, kernel="gaussian", na.rm=TRUE), lty=2, col=3, lwd=2)
lines(density(humid_temp, bw=2, kernel="gaussian", na.rm=TRUE), lty=2, col=4, lwd=2)
legend("topright", paste("bandwidth=", c("0.1","0.5","1","2"), sep=""), col=c(2,1,3,4), lty=2, lwd=2)






list.log.sample <- sample(list.log, size=20000, replace=FALSE)
list.net.sample <- sample(list.net, size=10000, replace=FALSE)

jpeg("Step_1_2.jpg")
par(mfrow=c(2,2))
plot(humid_temp[list.log.sample], humidity[list.log.sample], xlab="temperature", ylab="humidity", type="p", col="#00000050")
plot(humid_temp[list.net.sample], humidity[list.net.sample], xlab="temperature", ylab="humidity", type="p", col="#00000050")
plot(humid_temp[list.log.sample], voltage[list.log.sample], xlab="temperature", ylab="voltage", type="p", col="#00000050")
plot(humid_temp[list.net.sample], voltage[list.net.sample], xlab="temperature", ylab="voltage", ylim=c(200,250), type="p", col="#00000050")
par(mfrow=c(1,1))
dev.off()




###### Step # 2
###### Day and Night Seperate
######

## First of ALL, Break up result_time column into columns for month, day, and time

dates <- scan("sonoma_dates.txt", what="character", sep=" ");
epoch <- scan("sonoma_epochs.txt", what="numeric", sep=" ");
epochdates <- data.frame(epoch = epoch, dates = dates);
rm(list = c("dates", "epoch"));
epochdates$epoch <- as.numeric(as.character(epochdates$epoch));

## Parse time information
result_time <- epochdates$dates;

## Parse time information
result_time <- epochdates$dates;
result_time <- as.character(result_time)
result_time <- data.frame(result_time)

month <- c()
get.month <- function(x){ return(substr(x,5,7)) }
month <- apply(result_time, MARGIN=1, FUN=get.month)
month[month=="Apr"] <- 4
month[month=="May"] <- 5
month[month=="Jun"] <- 6
month <- as.numeric(month)

day <- c()
get.day <- function(x){ return(as.numeric(substr(x,9,10))) }
day <- apply(result_time, MARGIN=1, FUN=get.day)

hour <- c()
get.hour <- function(x){ return(as.numeric(substr(x,12,13))) }
hour <- apply(result_time, MARGIN=1, FUN=get.hour)

minute <- c()
get.minu <- function(x){ return(as.numeric(substr(x,15,16))) }
minute <- apply(result_time, MARGIN=1, FUN=get.minu)



#### Day & Night Separation

Std.epoch <- 1:length(epoch)
list.hour <- epoch
list.hour[is.na(list.hour)==FALSE] <- hour[na.omit(epoch)]
list.day <- Std.epoch[((list.hour<=19)&(list.hour>=7))&(is.na(list.hour)==FALSE)]
list.nig <- Std.epoch[((list.hour>19)|(list.hour<7))&(is.na(list.hour)==FALSE)]


list.day.sample <- sample(list.day, size=20000, replace=FALSE)
list.nig.sample <- sample(list.nig, size=20000, replace=FALSE)

png("Step_2.png")
pairs(data.sc[list.day.sample, 4:7])
dev.off()

png("Step_2_nig.png")
pairs(data.sc[list.nig.sample, 4:7])
dev.off()


##### By Pair Graph, find that InPAR>150000 corresponds to global outlier
##### For the local outliers in Night InPAR, probably due to special sunrise (b.c. not globally outlying)

data.sc[(hamatop>150000)&(is.na(hamatop)==FALSE), ] <- NA
attach(data.sc)


##### Relationship between Temperature and Humidity
##### By Bootstrap each time 10000 points

B <- 500
N <- 1000
x.pred <- seq(from=min(humid_temp, na.rm=T), to=max(humid_temp, na.rm=T), length.out=1000)
M.quad = matrix(0, nrow=length(x.pred), ncol=B)
a.quad = matrix(0, nrow=3, ncol=B)

plot(humid_temp[sample], humidity[sample], type="p", col=4)
for (i in 1:B){
	sample <- sample(list.day, size=N, replace=FALSE)
	formula.quad = humidity[sample] ~ humid_temp[sample] + I(humid_temp[sample]^2)
	quad.fit = glm(formula.quad, data=data.sc)

	a0.star = quad.fit$coef[1]
	a1.star = quad.fit$coef[2]
	a2.star = quad.fit$coef[3]
	curve(a0.star + a1.star*x + a2.star*x^2, add=T, col="gray", lwd=1)
	## To get CI-bounds, get predictions for each bootstrap-sample on the grid.
	M.quad[,i] = a0.star + a1.star*x.pred + a2.star*x.pred^2
	a.quad[,i] = c(a0.star, a1.star, a2.star)
}

a.mean <- apply(a.quad, 1, FUN=function(x){return(mean(x,na.rm=T))})
MEAN.quad <- apply(M.quad, 1, FUN=function(x){return(mean(x,na.rm=T))})
SD.quad <-  apply(M.quad, 1, FUN=function(x){return(sd(x,na.rm=T))}) 
lines(x.pred, MEAN.quad, col=2, lwd=2)
lines(x.pred,(MEAN.quad + 3*SD.quad), col="green", lwd=1)
lines(x.pred,(MEAN.quad - 3*SD.quad), col="green", lwd=1)



B <- 500
N <- 1000
x.pred <- seq(from=min(humid_temp, na.rm=T), to=max(humid_temp, na.rm=T), length.out=1000)
M.quad = matrix(0, nrow=length(x.pred), ncol=B)
a.quad = matrix(0, nrow=3, ncol=B)

sample <- sample(list.nig, size=N*10, replace=FALSE)
plot(humid_temp[sample], humidity[sample], type="p", col=4)
for (i in 1:B){
	sample <- sample(list.nig, size=N, replace=FALSE)
	formula.quad = humidity[sample] ~ humid_temp[sample] + I(humid_temp[sample]^2)
	quad.fit = glm(formula.quad, data=data.sc)

	a0.star = quad.fit$coef[1]
	a1.star = quad.fit$coef[2]
	a2.star = quad.fit$coef[3]
	curve(a0.star + a1.star*x + a2.star*x^2, add=T, col="gray", lwd=1)
	## To get CI-bounds, get predictions for each bootstrap-sample on the grid.
	M.quad[,i] = a0.star + a1.star*x.pred + a2.star*x.pred^2
	a.quad[,i] = c(a0.star, a1.star, a2.star)
}

a.mean <- apply(a.quad, 1, FUN=function(x){return(mean(x,na.rm=T))})
MEAN.quad <- apply(M.quad, 1, FUN=function(x){return(mean(x,na.rm=T))})
SD.quad <-  apply(M.quad, 1, FUN=function(x){return(sd(x,na.rm=T))}) 
lines(x.pred, MEAN.quad, col=2, lwd=2)
lines(x.pred,(MEAN.quad + 3*SD.quad), col="green", lwd=1)
lines(x.pred,(MEAN.quad - 3*SD.quad), col="green", lwd=1)



##### The final histogram, the Daytime histograms are more sensible. 

par(mfrow=c(2,4))
hist(humid_temp[list.day], main="Histogram of Temperature")
hist(humidity[list.day], main="Histogram of humidity")
hist(hamatop[list.day], main="Histogram of InPAR")
hist(hamabot[list.day], main="Histogram of RePAR")
hist(humid_temp[list.nig], main="Histogram of Temperature")
hist(humidity[list.nig], main="Histogram of humidity")
hist(hamatop[list.nig], main="Histogram of InPAR")
hist(hamabot[list.nig], main="Histogram of RePAR")
par(mfrow=c(1,1))






##### Step # 3

##### Check the "dip"

Std.epoch <- 1:length(epoch)
list.date <- epoch
list.date[is.na(list.date)==FALSE] <- day[na.omit(epoch)]

list.month <- epoch
list.month[is.na(list.month)==FALSE] <- month[na.omit(epoch)]

data.May1 <- c()
data.May1 <- data.sc[((list.month==5)&(list.date==1))&(is.na(list.month)==FALSE), ]

id <- unique(na.omit(data.May1[,2]))

##### Plot the dips

plot(data.May1[(data.May1[,2]==id[1])&(is.na(data.May1[,2])==F), 1], data.May1[(data.May1[,2]==id[1])&(is.na(data.May1[,2])==F), 4], xlim=c(1001, 1100), type="l", ylim=c(15,100), col=4)
for (i in 2:length(id)){
	lines(data.May1[(data.May1[,2]==id[i])&(is.na(data.May1[,2])==F), 1], data.May1[(data.May1[,2]==id[i])&(is.na(data.May1[,2])==F), 4], col=4)
}

list.hour <- epoch
list.hour[is.na(list.hour)==FALSE] <- hour[na.omit(epoch)]

list.minute <- epoch
list.minute[is.na(list.minute)==FALSE] <- minute[na.omit(epoch)]

dip <- c()
dip <- data.sc[(((list.minute>=30)&(list.minute<=40))&(list.hour==9))&(is.na(list.hour)==FALSE), ]
dip.else <- data.sc[(!((((list.minute>=30)&(list.minute<=40))&(list.hour==9))))&(is.na(list.hour)==FALSE), ]

sample.else <- sample(1:dim(dip.else)[1], size=10000, replace=FALSE)

plot(dip.else[sample.else, 4], dip.else[sample.else, 5], xlab="humidity", ylab="temperature", type="p", col=4)
plot(dip[,5], dip[,4], type="p", xlab="Temperature [Celcius]", ylab="Humidity [%]", main="Humidity vs. Temperature LOESS Fitting", col=2)
legend("topright", pch=1, col=c(4,2), c("Non-dip points", "Dip points"))


#### LOESS Fitting for Dip periods

## Fit LOESS-prediction-rule.
alpha = 0.25     # the % of data used to fit prediction rule at each x
degree = 0
loess.fit = loess(dip[,4]~dip[,5], data=dip, span=alpha, degree=degree)

## Plot the LOESS-prediction-rule (make grid, and get predictions along grid)
tem.pred = seq(from=min(dip[,5]), to=max(dip[,5]), by=0.01)
hum.pred = predict(loess.fit, tem.pred)
plot(dip[,5], dip[,4], type="p", xlab="Temperature [Celcius]", ylab="Humidity [%]", main="Humidity vs. Temperature LOESS Fitting", col=2)
lines(tem.pred, hum.pred, col=4, lwd=2)

alpha=0.25
tem.pred = seq(from=min(dip[,5]), to=max(dip[,5]), by=0.01)
jpeg("loess_degree.jpg")
plot(dip[,5], dip[,4], type="p", xlab="Temperature [Celcius]", ylab="Humidity [%]", main="Humidity vs. Temperature LOESS Fitting", col=8)
for (i in 0:2){
	degree=i
	loess.fit = loess(dip[,4]~dip[,5], data=dip, span=alpha, degree=degree)

	hum.pred = predict(loess.fit, tem.pred)
	lines(tem.pred, hum.pred, col=(i+2), lty=2, lwd=2)
}
legend("topright", lty=2, col=2:4, paste("Degree=", c("0","1","2"), sep=""))
dev.off()

degree=1
tem.pred = seq(from=min(dip[,5]), to=max(dip[,5]), by=0.01)
jpeg("loess_bandwidth.jpg")
plot(dip[,5], dip[,4], type="p", xlab="Temperature [Celsius]", ylab="Humidity [%]", main="Humidity vs. Temperature LOESS Fitting", col=8)
test <- c(2, 10, 20, 50, 100)
for (i in 1:5){
	alpha=test[i]*0.01
	loess.fit = loess(dip[,4]~dip[,5], data=dip, span=alpha, degree=degree)

	hum.pred = predict(loess.fit, tem.pred)
	lines(tem.pred, hum.pred, col=(i+1), lty=1, lwd=1)
}
legend("topright", lty=2, col=2:6, paste("Bandwidth=", c("2%","10%","20%","50%","100%"), sep=""))
dev.off()

