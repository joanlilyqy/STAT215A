# below, sonoma_dates.txt is the part of the sonoma-dates text file with just the dates
# sonoma_epochs.txt is the part of the sonoma-dates text file with just the epochs

## Break up result_time column into columns for month, day, and time
dates <- scan("sonoma_dates.txt", what="character", sep=",");
epoch <- scan("sonoma_epochs.txt", what="numeric", sep=",");
epochdates <- data.frame(epoch = epoch, dates = dates);
rm(list = c("dates", "epoch"));
epochdates$epoch <- as.numeric(as.character(epochdates$epoch));

## Parse time information
result_time <- epochdates$dates;


## Parse time information
result_time <- epochdates$dates;

## Creating month vector

"([A-Za-z]{3} )([A-Za-z]{3} *[1-3]*[0-9] [0-9]{2}:[0-9]{2})(:[0-9]{2})"

month <- c(); 

month[grep("([A-Za-z]{3} )(Apr *[1-3]*[0-9] [0-9]{2}:[0-9]{2})(:[0-9]{2})", result_time)] <- 4;

month[grep("([A-Za-z]{3} )(May *[1-3]*[0-9] [0-9]{2}:[0-9]{2})(:[0-9]{2})", result_time)] <- 5;

month[grep("([A-Za-z]{3} )(Jun *[1-3]*[0-9] [0-9]{2}:[0-9]{2})(:[0-9]{2})", result_time)] <- 6;

## Creating day vector
day <- c(); 
for(i in 1:30){day_expr <- sprintf("([A-Za-z]{3} )([A-Za-z]{3} *%s [0-9]{2}:[0-9]{2})(:[0-9]{2})", i); day[grep(day_expr, result_time)] <- i;}

## Creating time vectors
hour <- c();
for(i in 0:24){hour_expr <- sprintf("([A-Za-z]{3} )([A-Za-z]{3} *[1-3]*[0-9] %02s:[0-9]{2})(:[0-9]{2})", i); hour[grep(hour_expr, result_time)] <- i;}

minute <- c();
for(i in 0:59){min_expr <- sprintf("([A-Za-z]{3} )([A-Za-z]{3} *[1-3]*[0-9] [0-9]{2}:%02s)(:[0-9]{2})", i); minute[grep(min_expr, result_time)] <- i;}