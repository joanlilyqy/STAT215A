setwd("~/Desktop/fall215a/lab1");

library(MASS); data(Cars93);
attach(Cars93);

# jitter example
plot(MPG.city, MPG.highway);
plot(jitter(MPG.city), jitter(MPG.highway));

# transparency example
plot(MPG.city, MPG.highway, pch=19);
plot(MPG.city, MPG.highway, col="#00000030", pch=19);

# better transparency example
x <- rnorm(50000);
y = rnorm(50000);
plot(x, y, pch=46);
plot(x, y, pch=46, col="#00000050")

# iplots
library("Acinonyx")
ibar(Type)
iplot(MPG.city, MPG.highway)
iplot(Length, Width)




# onto the redwood data!!!
net <- read.csv("lab1Data/sonoma-data-net.csv", header=T);
datalog <- read.csv("lab1Data/sonoma-data-log.csv", header=T);

head(datalog);
head(net);

# some issues present themselves immediately:
# what's up with result_time?
sum(datalog$result_time == "2004-11-10 14:25:00")
nrow(datalog)

# also, notice that the different nodes have very different numbers of observations
node.numobs <- table(datalog$nodeid);
node.numobs
# make this into a matrix
node.numobs <- matrix(c(as.numeric(rownames(node.numobs)), node.numobs), nrow=length(node.numobs), ncol=2)
colnames(node.numobs) <- c("nodeid", "num.obs");

# consider this boxplot of node voltage by node id
# you can see that some nodes have very very very low voltage! total outliers.
with(datalog[sample(1:nrow(datalog), size=15000),], boxplot(voltage ~ nodeid))

node.voltages <- aggregate(voltage ~ nodeid, data = datalog, FUN=mean)
colnames(node.voltages) <- c("nodeid", "voltage")
node.voltages <- node.voltages[order(node.voltages[,2]), ]
head(node.voltages, 20);

# merge this voltage and # of observations data
node.stats <- merge(node.voltages, node.numobs, by ="nodeid")


