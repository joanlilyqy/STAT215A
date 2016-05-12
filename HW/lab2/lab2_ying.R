##### LAB2 Linguistic Data
library(maps)
library(fields)
library(ca)
library(MASS)
library("Acinonyx")

##### EDA
# refer to TA code

rm(list=ls())
lingData <- read.table("lingData.txt", header=T);
lingLoc <- read.table("lingLocation.txt", header=T);

dim(lingData);
# contains data for 47,471 respondents and (73-6)=67 questions
length(unique(lingData$ZIP));
# there are 11,712 unique zip codes it seems
sum(is.na(lingData))
# there are some missing values, may want to look into these
unique(lingData$STATE)
# also some weird state values, for instance no such state as C) or 94... or XX


#####Data Cleaning

# zip codes, in case you find NA values and would rather trust the zip code (if provided)
zipcodes <- read.csv("zips.txt", header=F);
head(zipcodes);

# some clunky cleanup efforts so my plotting code works
lingData <- lingData[!is.na(lingData$STATE),]
lingData <- lingData[!is.na(lingData$lat),];
lingData <- lingData[!is.na(lingData$long),];
sum(is.na(lingData))

# want to remove hawaii and AK because they're off the main map
HI_and_AK <- lingData$long < -130 | lingData$long > -65
lingData <- lingData[!(HI_and_AK),];

##### 1. Relationships between questions 
# preparation for plot (refer to TA code)
# here's just some annotation stuff to make sure that the states are labeled correctly
state_names <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY");

map_state_names <- map("state")$names;
state_names_nonabbrev <- c("alabama", "alaska", "arizona", "arkansas", "california", "colorado", "connecticut", "district of columbia", "delaware", "florida", "georgia", "hawaii", "idaho", "illinois", "indiana", "iowa", "kansas", "kentucky", "louisiana", "maine", "maryland", "massachusetts", "michigan", "minnesota", "mississippi", "missouri", "montana", "nebraska", "nevada", "new hampshire", "new jersey", "new mexico", "new york", "north carolina", "north dakota", "ohio", "oklahoma", "oregon", "pennsylvania", "rhode island", "south carolina", "south dakota", "tennessee", "texas", "utah", "vermont", "virginia", "washington", "west virginia", "wisconsin", "wyoming")

# getting the map names in the right order
map_state_order <- c();
for(i in 1:51){
	slength <- nchar(state_names_nonabbrev[i])
	ind <- grep(state_names_nonabbrev[i], substr(map_state_names, 1, slength));
	map_state_order[ind] <- i
}


# analysis for Q68(maternal grandmother),69(parternal grandmother),70(maternal grandfather),71(parternal grandfather)
par(mfrow=c(2,2))
#68
image = as.image( Z = c(1, lingData$Q068[(lingData$Q068!=7)&(lingData$Q068!=0)], 1), ind=rbind(c(-130, 51), lingData[(lingData$Q068!=7)&(lingData$Q068!=0), c(73,72)], c(-65, 24)), nrow=150, ncol=150 );
image.plot( image, col=rainbow(max(unique(lingData$Q068[(lingData$Q068!=7)&(lingData$Q068!=0)])), alpha=1));
mtext("Answer to Question 68: Maternal Grandmother", 3, line = -1, cex=0.75);
map("world", add=T)
map.text("state", labels=state_names[map_state_order], add = T);
map("state", add=T)
#69
image = as.image( Z = c(1, lingData$Q069[(lingData$Q069!=6)&(lingData$Q069!=0)], 1), ind=rbind(c(-130, 51), lingData[(lingData$Q069!=6)&(lingData$Q069!=0), c(73,72)], c(-65, 24)), nrow=150, ncol=150 );
image.plot( image, col=rainbow(max(unique(lingData$Q069[(lingData$Q069!=6)&(lingData$Q069!=0)])), alpha=1));
mtext("Answer to Question 69: Paternal Grandmother", 3, line = -1, cex=0.75);
map("world", add=T)
map.text("state", labels=state_names[map_state_order], add = T);
map("state", add=T)
#70
image = as.image( Z = c(1, lingData$Q070[(lingData$Q070!=7)&(lingData$Q070!=0)], 1), ind=rbind(c(-130, 51), lingData[(lingData$Q070!=7)&(lingData$Q070!=0), c(73,72)], c(-65, 24)), nrow=150, ncol=150 );
image.plot( image, col=rainbow(max(unique(lingData$Q070[(lingData$Q070!=7)&(lingData$Q070!=0)])), alpha=1));
mtext("Answer to Question 70: Maternal Grandfather", 3, line = -1, cex=0.75);
map("world", add=T)
map.text("state", labels=state_names[map_state_order], add = T);
map("state", add=T)
#71
image = as.image( Z = c(1, lingData$Q071[(lingData$Q071!=5)&(lingData$Q071!=0)], 1), ind=rbind(c(-130, 51), lingData[(lingData$Q071!=5)&(lingData$Q071!=0), c(73,72)], c(-65, 24)), nrow=150, ncol=150 );
image.plot( image, col=rainbow(max(unique(lingData$Q071[(lingData$Q071!=5)&(lingData$Q071!=0)])), alpha=1));
mtext("Answer to Question 71: Paternal Grandfather", 3, line = -1, cex=0.75);
map("world", add=T)
map.text("state", labels=state_names[map_state_order], add = T);
map("state", add=T)

# linked plots: like interactive coplots
# 68
q68=lingData$Q068[(lingData$Q068!=0)&(lingData$Q069!=0)&(lingData$Q070!=0)&(lingData$Q071!=0)]
ibar(as.factor(q68));
# 69
q69=lingData$Q069[(lingData$Q068!=0)&(lingData$Q069!=0)&(lingData$Q070!=0)&(lingData$Q071!=0)]
ibar(as.factor(q69));
# 70
q70=lingData$Q070[(lingData$Q068!=0)&(lingData$Q069!=0)&(lingData$Q070!=0)&(lingData$Q071!=0)]
ibar(as.factor(q70));
# 71
q71=lingData$Q071[(lingData$Q068!=0)&(lingData$Q069!=0)&(lingData$Q070!=0)&(lingData$Q071!=0)]
ibar(as.factor(q71));


# this is for Q050: how do you address a group of people
image = as.image( Z = c(1, lingData$Q050[(lingData$Q050!=0)], 1), ind=rbind(c(-130, 51), lingData[(lingData$Q050!=0), c(73,72)], c(-65, 24)), nrow=150, ncol=150 );
image.plot( image, col=rainbow(max(unique(lingData$Q050[lingData$Q050!=0])), alpha=1));
mtext("Answer to Question 50", 3, line = -1, cex=0.75);
map("world", add=T)
map.text("state", labels=state_names[map_state_order], add = T);
map("state", add=T)

# this is for Q105: how do you call sweetened carbonated beverage
image = as.image( Z = c(1, lingData$Q105[(lingData$Q105!=0)], 1), ind=rbind(c(-130, 51), lingData[(lingData$Q105!=0), c(73,72)], c(-65, 24)), nrow=150, ncol=150 );
image.plot( image, col=rainbow(max(unique(lingData$Q105[lingData$Q105!=0])), alpha=1));
mtext("Answer to Question 105", 3, line = -1, cex=0.75);
map("world", add=T)
map.text("state", labels=state_names[map_state_order], add = T);
map("state", add=T)

# linked plots: like interactive coplots
# the "you all" question
q50 <- (lingData$Q050[(lingData$Q050!=0)&(lingData$Q105!=0)])
ibar(as.factor(q50));
# the "pop/coke" question
q105 <-(lingData$Q105[(lingData$Q050!=0)&(lingData$Q105!=0)])
ibar(as.factor(q105));


#####2.MCA 

# Select some problems

Q.list <- c(65,66,73,74,76,84,85,94,96,97,99,106)
compare <- as.numeric(substr(names(lingData),2,4))
Data.list <- c()
for (i in 1:length(Q.list)){
	num <- grep(Q.list[i], compare)
	Data.list <- c(Data.list, num)
}

sel.Data <- lingData[,Data.list]
names(sel.Data) <- as.character(1:12)
mca.1 <- mjca(sel.Data);
plot(mca.1, what=c("none","all"))


# data without 0

sel.Data <- lingData[,Data.list]
zero.list <- apply(sel.Data, MARGIN=1, FUN=function(x){sum(x==0, na.rm=T)})
sel.Data.no <- sel.Data[which(zero.list==0), ]
names(sel.Data.no) <- as.character(1:12)
mca.2 <- mjca(sel.Data.no);
plot(mca.2, what=c("none","all"))


# Exploring more: 74/99/106
image = as.image( Z = c(1, lingData$Q074[(lingData$Q074==2)], 1), ind=rbind(c(-130, 51), lingData[(lingData$Q074==2), c(73,72)], c(-65, 24)), nrow=150, ncol=150 );
image.plot( image, col=2);
mtext("Answer to Question 74 Choice=2", 3, line = -1, cex=0.75);
map("world", add=T)
map.text("state", labels=state_names[map_state_order], add = T);
map("state", add=T)

image = as.image( Z = c(1, lingData$Q099[(lingData$Q099==4))], 1), ind=rbind(c(-130, 51), lingData[(lingData$Q099==4)), c(73,72)], c(-65, 24)), nrow=150, ncol=150 );
image.plot( image, col=2);
mtext("Answer to Question 99 Choice=4", 3, line = -1, cex=0.75);
map("world", add=T)
map.text("state", labels=state_names[map_state_order], add = T);
map("state", add=T)

image = as.image( Z = c(1, lingData$Q106[(lingData$Q106==4))], 1), ind=rbind(c(-130, 51), lingData[(lingData$Q106==4)), c(73,72)], c(-65, 24)), nrow=150, ncol=150 );
image.plot( image, col=2);
mtext("Answer to Question 106 Choice=4", 3, line = -1, cex=0.75);
map("world", add=T)
map.text("state", labels=state_names[map_state_order], add = T);
map("state", add=T)

# Robustness
QR.list <- c(50,65,66,73,74,76,84,85,94,96,97,99,105,106)
compare <- as.numeric(substr(names(lingData),2,4))
DataR.list <- c()
for (i in 1:length(QR.list)){
	num <- grep(QR.list[i], compare)
	DataR.list <- c(DataR.list, num)
}
sel.DataR <- lingData[,DataR.list]
zero.list <- apply(sel.DataR, MARGIN=1, FUN=function(x){sum(x==0, na.rm=T)})
sel.DataR.no <- sel.DataR[which(zero.list==0), ]
names(sel.DataR.no) <- as.character(1:14)
mca.3 <- mjca(sel.DataR.no);
plot(mca.3, what=c("none","all"))



#####3. Clustering

sel.Data.mca <- mca.2$rowcoord

# perform k-means clustering
sel.Data.k5 <- kmeans(sel.Data.mca, centers=5);
# compare results
plot(sel.Data.mca[,1], sel.Data.mca[,2], xlab="MCA 1st component", ylab="MCA 2nd component", col=c(1:5)[sel.Data.k5$cluster])


# Clustering on only two questions
Q2.list <- c(50,105)
compare <- as.numeric(substr(names(lingData),2,4))
Data2.list <- c()
for (i in 1:length(Q2.list)){
	num <- grep(Q2.list[i], compare)
	Data2.list <- c(Data2.list, num)
}

list.Q <- c(Data2.list[grep(50, Q2.list)],Data2.list[grep(105, Q2.list)])
ling.Q <- lingData[which(zero.list==0), c(list.Q, 72, 73)]

lingQ.k <- kmeans(ling.Q[,1:2], centers=3);

plot(ling.Q[,4], ling.Q[,3], xlab="longitude", ylab="latitude", xlim=c(-130, -65), ylim=c(24, 51), col=c(1:3)[lingQ.k$cluster], main="K-means Clustering on Q50 and Q105")
legend("bottomleft", "Clusters: k=3")
map("world", add=T)
map("state", add=T)


