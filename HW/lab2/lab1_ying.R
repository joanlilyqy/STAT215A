### Lab1 redwood data
# referred to Discussion section code for parsing dates and loading data

# import data
data.net <- read.csv("lab1Data/sonoma-data-net.csv", header=T);
data.log <- read.csv("lab1Data/sonoma-data-log.csv", header=T);
data.all <- read.csv("lab1Data/sonoma-data-all.csv", header=T);
head(data.all)


### Data Cleaning
# extract useful columns
sensor <- data.all[,c(2,3,5,7,8,10:11)];
index.net <- 1:length(data.net$voltage);
index.log <- (length(data.net$voltage)+1):length(data.all$voltage);

# clean according to range analysis in the original paper
# treat data points out of range as outliers
sensor[index.log, ][((sensor$voltage[list.log]>3)|(sensor$voltage[list.log]<2.4)), ]<-NA;
sensor[((sensor$humidity>100)|(sensor$humidity<16.4))&(is.na(sensor$humidity)==F), ]<-NA;
sensor[((sensor$humid_temp>32.6)|(sensor$humid_temp<6.6))&(is.na(sensor$humid_temp)==F), ]<-NA;
sensor[(sensor$hamatop>150000)&(is.na(sensor$hamatop)==F), ] <- NA

# clear duplicates
sensor[(duplicated(sensor[,1:2])==T), ]<-NA;


### Basic plots for dataset feature
attach(sensor)

# regenerate figure 3(a) in the ref paper with only temp
hist(humid_temp, freq=F, ylim=c(0,0.1), main="Distribution of Temperature in Redwood data", xlab="Temperature (^C)");


###### Kernel Density Plots (Lab2)
#(1) experiment with different bandwidths
lines(density(humid_temp, bw=.2, kernel="gaussian", na.rm=TRUE), lty=2, col=1, lwd=1.5)
lines(density(humid_temp, bw=.4, kernel="gaussian", na.rm=TRUE), lty=3, col=2, lwd=1.5)
lines(density(humid_temp, bw=.8, kernel="gaussian", na.rm=TRUE), lty=4, col=3, lwd=1.5)
lines(density(humid_temp, bw=1.6, kernel="gaussian", na.rm=TRUE), lty=5, col=4, lwd=1.5)
legend("topright", paste("bandwidth=", c("0.2","0.4","0.8","1.6"), sep=""), col=1:4, lty=2:5, lwd=1.5)

#(2) experiment with different kernels
hist(humid_temp, freq=F, ylim=c(0,0.1), main="Distribution of Temperature in Redwood data", xlab="Temperature (^C)");
lines(density(humid_temp, bw=.8, kernel="gaussian", na.rm=TRUE), lty=2, col=1, lwd=1.5)
lines(density(humid_temp, bw=.8, kernel="cosine", na.rm=TRUE), lty=3, col=2, lwd=1.5)
lines(density(humid_temp, bw=.8, kernel="rectangular", na.rm=TRUE), lty=4, col=3, lwd=1.5)
legend("topright", paste("kernel=", c("Gaussian","Cosine","Rectangular"), sep=""), col=1:3, lty=2:4, lwd=1.5)
legend("topleft", "bandwidth=0.8")



### Time parsing
# code from artichoke for reference
dates <- scan("sonoma_dates.txt", what="character", sep=" ");
epoch <- scan("sonoma_epochs.txt", what="numeric", sep=" ");
epochdates <- data.frame(epoch = epoch, dates = dates);
rm(list = c("dates", "epoch"));
epochdates$epoch <- as.numeric(as.character(epochdates$epoch));

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

time <- c()
get.time <- function(x){ return(as.numeric(substr(x,12,13))*100+as.numeric(substr(x,15,16))) }
time <- apply(result_time, MARGIN=1, FUN=get.time)

# plot the "dip"
index.date <- epoch
index.date[is.na(index.date)==F] <- day[na.omit(epoch)]
index.month <- epoch
index.month[is.na(index.month)==F] <- month[na.omit(epoch)]

data.May1 <- c()
data.May1 <- sensor[((index.month==5)&(index.date==1))&(is.na(index.month)==F), ]

id <- unique(na.omit(data.May1[,2]))

plot(time[data.May1[(data.May1[,2]==id[1])&(is.na(data.May1[,2])==F), 1]], data.May1[(data.May1[,2]==id[1])&(is.na(data.May1[,2])==F), 4], type="l", xlim=c(900,1300),ylim=c(15,100), col=4, xlab="Time", ylab="Temperature (^C)", main="Temperature vs Time Plot")
for (i in 2:length(id)){
	lines(time[data.May1[(data.May1[,2]==id[i])&(is.na(data.May1[,2])==F), 1]], data.May1[(data.May1[,2]==id[i])&(is.na(data.May1[,2])==F), 4], col=4)
}
legend("topright", "all nodes, 9:00-13:00")


# get the "dip" period data

index.hour <- epoch
index.hour[is.na(index.hour)==F] <- hour[na.omit(epoch)]
index.minute <- epoch
index.minute[is.na(index.minute)==F] <- minute[na.omit(epoch)]

# time 9:30-9:40
sensor.dip <- c()
sensor.dip <- sensor[(((index.minute>=30)&(index.minute<=40))&(index.hour==9))&(is.na(index.hour)==F), ]
dip.else <- sensor[(!((((index.minute>=30)&(index.minute<=40))&(index.hour==9))))&(is.na(index.hour)==F), ]


##### LOESS Fitting for Dip periods

#(1) degree

span=0.25 		# the persent of data used to fit at each point
temp.pred = seq(from=min(sensor.dip[,5],na.rm=T), to=max(sensor.dip[,5],na.rm=T), by=0.01)
plot(sensor.dip[,5], sensor.dip[,4], type="p", xlab="Temperature [^C]", ylab="Humidity [%RH]", main="Humidity vs. Temperature LOESS Fitting", col="#00000030" )
for (i in 0:2){
	degree=i
	loess.fit = loess(sensor.dip[,4]~sensor.dip[,5], data=sensor.dip, span=span, degree=degree)
	humid.pred = predict(loess.fit, temp.pred)
	lines(temp.pred, humid.pred, col=(i+2), lty=(i+2), lwd=2)
}
legend("topright", lty=2:4, col=2:4, lwd=2, paste("Degree=", c("0-NW","1","2"), sep=""))
legend("bottomleft", "Bandwidth=25%")

#(2) span/bandwidth
degree=2		# the highest polynomial degree
plot(sensor.dip[,5], sensor.dip[,4], type="p", xlab="Temperature [^C]", ylab="Humidity [%RH]", main="Humidity vs. Temperature LOESS Fitting", col="#00000030" )
test <- c(5, 25, 50, 100)
for (i in 1:4){
	span=test[i]*0.01
	loess.fit = loess(sensor.dip[,4]~sensor.dip[,5], data=sensor.dip, span=span, degree=degree)
	humid.pred = predict(loess.fit, temp.pred)
	lines(temp.pred, humid.pred, col=(i+1), lty=(i+1), lwd=2)
}
legend("topright", lty=2:5, col=2:5, paste("Bandwidth=", c("5%","25%","50%","100%"), sep=""))
legend("bottomleft", "Degree=2")







