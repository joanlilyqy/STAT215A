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

# regenerate figure 3(a) in the ref paper with only temp and humidity
par(mfrow=c(2,1))
hist(humid_temp, freq=F, main="Distribution of Temperature in Redwoods data", xlab="Temperature (^C)");
hist(humidity, freq=F, main="Distribution of Temperature in Redwoods data", xlab="Humidity (%RH)");

# scatter plot with sample points
sample.log <- sample(index.log, size=2000, replace=FALSE);
sample.net <- sample(index.net, size=1000, replace=FALSE);
sample.all <- sample(length(data.all$voltage), size=5000, replace=FALSE);

# correlation between temperature and humidity
plot(humid_temp[sample.all], humidity[sample.all], type="p", col="#00000050", xlab="Temperature (^C)", 
ylab="Humidity (%RH)")
title("Temperature vs. Humidity in Redwood data");

# strong correlation between temp and voltage (different trends in net vs. log)
par(mfrow=c(1,2))
plot(humid_temp[sample.log], voltage[sample.log], ylim=c(2.4,3), type="p", col="#00000050", 
xlab="Temperature (^C)", ylab="Battery Voltage (V)")
title("Logged Temperature vs Battery Voltage")
plot(humid_temp[sample.net], voltage[sample.net], ylim=c(200,250), type="p", col="#00000050",
xlab="Temperature (^C)", ylab="Battery Voltage (V)")
title("Net Temperature vs Battery Voltage")


### Split data into day/night using time parsing
# code from bSpace by TA
# Break up result_time column into columns for month, day, and time
dates <- scan("lab1Data/sonoma_dates.txt", what="character", sep=" ");
epoch <- scan("lab1Data/sonoma_epochs.txt", what="numeric", sep=" ");
epochdates <- data.frame(epoch = epoch, dates = dates);
rm(list = c("dates", "epoch"));
epochdates$epoch <- as.numeric(as.character(epochdates$epoch));

# Parse time information
result_time <- epochdates$dates;

# Creating month vector
"([A-Za-z]{3} )([A-Za-z]{3} *[1-3]*[0-9] [0-9]{2}:[0-9]{2})(:[0-9]{2})"
month <- c(); 
month[grep("([A-Za-z]{3} )(Apr *[1-3]*[0-9] [0-9]{2}:[0-9]{2})(:[0-9]{2})", result_time)] <- 4;
month[grep("([A-Za-z]{3} )(May *[1-3]*[0-9] [0-9]{2}:[0-9]{2})(:[0-9]{2})", result_time)] <- 5;
month[grep("([A-Za-z]{3} )(Jun *[1-3]*[0-9] [0-9]{2}:[0-9]{2})(:[0-9]{2})", result_time)] <- 6;

# Creating day vector
day <- c(); 
for(i in 1:30){day_expr <- sprintf("([A-Za-z]{3} )([A-Za-z]{3} *%s [0-9]{2}:[0-9]{2})(:[0-9]{2})", i); day[grep(day_expr, result_time)] <- i;}

# Creating time vectors
hour <- c();
for(i in 0:24){hour_expr <- sprintf("([A-Za-z]{3} )([A-Za-z]{3} *[1-3]*[0-9] %02s:[0-9]{2})(:[0-9]{2})", i); hour[grep(hour_expr, result_time)] <- i;}
minute <- rep(c(0,5), length(hour)/2);
for(i in 0:59){min_expr <- sprintf("([A-Za-z]{3} )([A-Za-z]{3} *[1-3]*[0-9] [0-9]{2}:%02s)(:[0-9]{2})", i); minute[grep(min_expr, result_time)] <- i;}

# Combine the time parsing result
epochtime <- cbind(month,day,hour,minute) #epochtime[epoch] gives the time of data point

# split index list and sample day/night separately
index.epoch <- 1:length(epoch);
index.hour <- hour[epoch];
index.day <- index.epoch[((index.hour< 18)&(index.hour> 10))&(is.na(index.hour)==F)]
index.nit <- index.epoch[((index.hour>=18)|(index.hour<=10))&(is.na(index.hour)==F)]

sample.day <- sample(index.day, size=2000, replace=FALSE)
sample.nit <- sample(index.nit, size=2000, replace=FALSE)

# regenerate figure 3(a) in the ref paper with PAR daytime
par(mfrow=c(2,1))
hist(hamatop[index.day], freq=F, main="Distribution of Incident PAR", xlab="PAR (umol/m^2/s)");
hist(hamabot[index.day], freq=F, main="Distribution of Reflected PAR", xlab="PAR (umol/m^2/s)");






