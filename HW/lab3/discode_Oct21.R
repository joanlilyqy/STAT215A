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
image(70:368, 2:368, matrix(image1$expert[order(-image1$y, image1$x)], 368-70+1, 368-2+1), col=heat.colors(3), main="Expert Labels for Image 1", xlab="x", ylab="y");
# cloud: yellow
# no cloud: red
# unlabeled: orange

filled.contour(70:368, 2:368, matrix(image1$SD[order(-image1$y, image1$x)], 368-70+1, 368-2+1), col=heat.colors(20));

filled.contour(70:368, 2:368, matrix(image1$CORR[order(-image1$y, image1$x)], 368-70+1, 368-2+1), col=heat.colors(20));

## KERNEL DENSITY PLOTS
## see difference in distributions between cloud/no cloud for the three features
dens.NDAI.1 <- with(subset(image1, expert==1), density(NDAI))
dens.NDAI.0 <- with(subset(image1, expert==-1), density(NDAI))
plot(dens.NDAI.1, col="red", main="NDAI density for cloud/no cloud", xlab="NDAI", ylim=c(min(dens.NDAI.1$y, dens.NDAI.0$y), max(dens.NDAI.1$y, dens.NDAI.0$y)), xlim=c(min(dens.NDAI.1$x, dens.NDAI.0$x), max(dens.NDAI.1$x, dens.NDAI.0$x)));
lines(dens.NDAI.0, col="blue");
legend("topright", bty="n", legend=c("cloud", "no cloud"), lty=1, col=c("red", "blue"))

dens.CORR.1 <- with(subset(image1, expert==1), density(CORR))
dens.CORR.0 <- with(subset(image1, expert==-1), density(CORR))
plot(dens.CORR.1, col="red", main="CORR density for cloud/no cloud", xlab="CORR", ylim=c(min(dens.CORR.1$y, dens.CORR.0$y), max(dens.CORR.1$y, dens.CORR.0$y)), xlim=c(min(dens.CORR.1$x, dens.CORR.0$x), max(dens.CORR.1$x, dens.CORR.0$x)));
lines(dens.CORR.0, col="blue");
legend("topright", bty="n", legend=c("cloud", "no cloud"), lty=1, col=c("red", "blue"))

dens.SD.1 <- with(subset(image1, expert==1), density(SD))
dens.SD.0 <- with(subset(image1, expert==-1), density(SD))
plot(dens.SD.1, col="red", main="SD density for cloud/no cloud", xlab="SD", ylim=c(min(dens.SD.1$y, dens.SD.0$y), max(dens.SD.1$y, dens.SD.0$y)), xlim=c(min(dens.SD.1$x, dens.SD.0$x), max(dens.SD.1$x, dens.SD.0$x)));
lines(dens.SD.0, col="blue");
legend("topright", bty="n", legend=c("cloud", "no cloud"), lty=1, col=c("red", "blue"))