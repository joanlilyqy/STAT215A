##### LAB2 STAT 215A
##### Part-2. Linguistic Data


#########################################
# explore the linguistic data a little bit

rm(list=ls())
lingData <- read.table("lingData.txt", header=T);
lingLoc <- read.table("lingLocation.txt", header=T);

# contains data for 47,471 respondents
# and 68 questions
dim(lingData);
length(unique(lingData$ZIP));
# there are 11,712 unique zip codes it seems

# there are some missing values, may want to look into these
sum(is.na(lingData))

# also some weird state values, for instance
# no such state as C) or 94... or XX
unique(lingData$STATE)

# so there is some cleaning involved, but this data is much cleaner than the tree data. after all, the point of the tree lab was to clean data...that's not the point of this one

# zip codes, in case you find NA values and would rather trust the zip code (if provided)
zipcodes <- read.csv("zips.txt", header=F);
head(zipcodes);



# let's take a look at how they got from lingData to lingLocation
questionlevels <- 
apply(lingData[,-c(1:4,72,73)], 2, function(x){length(unique(x))})
# check that this matches up with ncol of lingLoc
# each question can be encoded by levels-1 binary variables (remove the zero)
sum(questionlevels-1);
dim(lingLoc);


########################################
# making pretty plots!!!
# yaaaaay.
# these two packages allow you to make plots on maps
library(maps)
library(fields)
library("Acinonyx")



########################################
# Plot Setup

# here's just some annotation stuff to make sure that the states are labeled correctly
state_names <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY");

map_state_names <- map("state")$names;
state_names_nonabbrev <- c("alabama", "alaska", "arizona", "arkansas", "california", "colorado", "connecticut", "district of columbia", "delaware", "florida", "georgia", "hawaii", "idaho", "illinois", "indiana", "iowa", "kansas", "kentucky", "louisiana", "maine", "maryland", "massachusetts", "michigan", "minnesota", "mississippi", "missouri", "montana", "nebraska", "nevada", "new hampshire", "new jersey", "new mexico", "new york", "north carolina", "north dakota", "ohio", "oklahoma", "oregon", "pennsylvania", "rhode island", "south carolina", "south dakota", "tennessee", "texas", "utah", "vermont", "virginia", "washington", "west virginia", "wisconsin", "wyoming")

# getting the map names in the right order
#### A bit of Modification

map_state_order <- c();
for(i in 1:51){
	slength <- nchar(state_names_nonabbrev[i])
	ind <- grep(state_names_nonabbrev[i], substr(map_state_names, 1, slength));
	map_state_order[ind] <- i
}


########################################
## Simulation Start From Here
##
## 1. Relationships between questions
##    Data Cleaning

# some clunky cleanup efforts so my plotting code works
lingData <- lingData[!is.na(lingData$STATE),]
lingData <- lingData[!is.na(lingData$lat),];
lingData <- lingData[!is.na(lingData$long),];


# want to remove hawaii and AK because they're off the main map
HI_and_AK <- lingData$long < -130 | lingData$long > -65


####################################################
###### Select # 50, 51, 52, 70, 71, 83, 96, 111, 

# actual plotting code
# this is for Q050: how do you address a group of people
image = as.image( Z = c(1, lingData$Q050[!((HI_and_AK)|(lingData$Q050==0))], 1), ind=rbind(c(-130, 50), lingData[!((HI_and_AK)|(lingData$Q050==0)), c(73,72)], c(-65, 25)), nrow=150, ncol=150 );
image.plot( image, col=rainbow(max(unique(lingData$Q050[lingData$Q050!=0])), alpha=0.50, s=1, v=0.9));
mtext("Answer to Question 50", 3, line = -0.7, cex=0.75);
map("world", add=T)
map.text("state", labels=state_names[map_state_order], add = T);
map("state", add=T)


##### Questions 68-71
jpeg("Que105.jpg")
map("state")
image = as.image( Z = c(1, lingData$Q099[!((HI_and_AK)|(lingData$Q099!=4))], 1), ind=rbind(c(-130, 50), lingData[!((HI_and_AK)|(lingData$Q099!=4)), c(73,72)], c(-65, 25)), nrow=150, ncol=150 );
image.plot( image, col=2);
mtext("Answer to Question 99 Choice=2", 3, line = -0.7, cex=0.75);
map("world", add=T)
map.text("state", labels=state_names[map_state_order], add = T);
map("state", add=T)
dev.off()




##################################################
# linked plots: like interactive coplots
library("Acinonyx")
# the "city" question
ibar(as.factor(lingData$Q050));
# the paternal grandpa question
ibar(as.factor(lingData$Q105));
# drive-through liquor stores
ibar(as.factor(lingData$Q068));
ibar(as.factor(lingData$Q069));
ibar(as.factor(lingData$Q070));
ibar(as.factor(lingData$Q071));


################################################
##### Correspondence Analysis

lingData.sc <- lingData[,5:71]
lingData.sc <- lingData.sc[1:10000,]
zero.list <- apply(lingData.sc, MARGIN=1, FUN=function(x){sum(x==0, na.rm=T)})
lingData.sc.an <- lingData.sc[which(zero.list==0), ]
x.sc <- mjca(lingData.sc.an);

plot(x.sc, what=c("none","all"))


#############################
# Select some problems

que.list <- c(50,58,61,65,66, 68:71, 73,74,76,84,85,94,96,97,99,105,106)

compare <- as.numeric(substr(names(lingData),2,4))
sel.list <- c()
for (i in 1:length(que.list)){
	num <- grep(que.list[i], compare)
	sel.list <- c(sel.list, num)
}

####################################################
######## Multiple Joint Correspondence Analysis

###### Full dat with 0's
lingData.sc <- lingData[,sel.list]
names(lingData.sc) <- as.character(1:20)
ca.1 <- mjca(lingData.sc);
plot(ca.1, what=c("none","all"))


###### Full data without 0's

lingData.sc <- lingData[,sel.list]
zero.list <- apply(lingData.sc, MARGIN=1, FUN=function(x){sum(x==0, na.rm=T)})
lingData.sc.an <- lingData.sc[which(zero.list==0), ]
names(lingData.sc.an) <- as.character(1:20)
ca.2 <- mjca(lingData.sc.an);

plot(ca.2, what=c("none","all"))


############# Question # 74/99/106
image = as.image( Z = c(1, lingData$Q074[!((HI_and_AK)|(lingData$Q074!=2))], 1), ind=rbind(c(-130, 50), lingData[!((HI_and_AK)|(lingData$Q074!=2)), c(73,72)], c(-65, 25)), nrow=150, ncol=150 );
image.plot( image, col=2);
mtext("Answer to Question 74 Choice=2", 3, line = -0.7, cex=0.75);
map("world", add=T)
map.text("state", labels=state_names[map_state_order], add = T);
map("state", add=T)

image = as.image( Z = c(1, lingData$Q099[!((HI_and_AK)|(lingData$Q099!=4))], 1), ind=rbind(c(-130, 50), lingData[!((HI_and_AK)|(lingData$Q099!=4)), c(73,72)], c(-65, 25)), nrow=150, ncol=150 );
image.plot( image, col=2);
mtext("Answer to Question 99 Choice=4", 3, line = -0.7, cex=0.75);
map("world", add=T)
map.text("state", labels=state_names[map_state_order], add = T);
map("state", add=T)

image = as.image( Z = c(1, lingData$Q106[!((HI_and_AK)|(lingData$Q106!=4))], 1), ind=rbind(c(-130, 50), lingData[!((HI_and_AK)|(lingData$Q106!=4)), c(73,72)], c(-65, 25)), nrow=150, ncol=150 );
image.plot( image, col=2);
mtext("Answer to Question 106 Choice=4", 3, line = -0.7, cex=0.75);
map("world", add=T)
map.text("state", labels=state_names[map_state_order], add = T);
map("state", add=T)


####### Group #2 50/61/105/106
image = as.image(Z=c(1, lingData$Q050[!((HI_and_AK)|(lingData$Q050!=9))], 1), ind=rbind(c(-130, 50), lingData[!((HI_and_AK)|(lingData$Q050!=9)), c(73,72)], c(-65, 25)), nrow=150, ncol=150 );
image.plot( image, col=2);
mtext("Answer to Question 50 Choice=9", 3, line = -0.7, cex=0.75);
map("world", add=T)
map.text("state", labels=state_names[map_state_order], add = T);
map("state", add=T)

image = as.image(Z=c(1, lingData$Q105[!((HI_and_AK)|(lingData$Q105!=3))], 1), ind=rbind(c(-130, 50), lingData[!((HI_and_AK)|(lingData$Q105!=3)), c(73,72)], c(-65, 25)), nrow=150, ncol=150 );
image.plot( image, col=2);
mtext("Answer to Question 105 Choice=3", 3, line = -0.7, cex=0.75);
map("world", add=T)
map.text("state", labels=state_names[map_state_order], add = T);
map("state", add=T)

image = as.image(Z=c(1, lingData$Q106[!((HI_and_AK)|(lingData$Q106!=2))], 1), ind=rbind(c(-130, 50), lingData[!((HI_and_AK)|(lingData$Q106!=2)), c(73,72)], c(-65, 25)), nrow=150, ncol=150 );
image.plot( image, col=2);
mtext("Answer to Question 106 Choice=2", 3, line = -0.7, cex=0.75);
map("world", add=T)
map.text("state", labels=state_names[map_state_order], add = T);
map("state", add=T)



###### Clustering Analysis

ling.val <- ca.2$rowcoord

# perform k-means clustering
ling.k2 <- kmeans(ling.val, centers=5);
ling.k3 <- kmeans(iris, centers=3);
ling.k4 <- kmeans(iris[,c(4,3)], centers=4);

# compare results
plot(ling.val[,1], ling.val[,2], xlab="1st component", ylab="2nd component", col=c(1:5)[ling.k2$cluster])



##### Real Clustering
list.realistic <- c(sel.list[grep(50, que.list)],sel.list[grep(105, que.list)])
ling.realistic <- lingData[which(zero.list==0), c(list.realistic, 72, 73)]

ling.kreal <- kmeans(ling.realistic[, 1:2], centers=3);

plot(ling.realistic[,4], ling.realistic[,3], xlab="longitude", ylab="latitude", xlim=c(-125, -65), ylim=c(25, 52), col=c(1:3)[ling.kreal$cluster])
map("world", add=T)
map("state", add=T)

geo.1 <- ling.realistic[which(ling.kreal$cluster==1),3:4]
geo.2 <- ling.realistic[which(ling.kreal$cluster==2),3:4]
geo.3 <- ling.realistic[which(ling.kreal$cluster==3),3:4]

max(sqrt(apply((geo.1-mean(geo.1))^2, MARGIN=1, FUN=sum)))
max(sqrt(apply((geo.1-mean(geo.1))^2, MARGIN=1, FUN=sum)))
max(sqrt(apply((geo.1-mean(geo.1))^2, MARGIN=1, FUN=sum)))

sqrt(sum((mean(geo.1)-mean(geo.2))^2))
sqrt(sum((mean(geo.1)-mean(geo.3))^2))
mean(geo.2)-mean(geo.3)


















