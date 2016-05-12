# code for Lab 0, Fall 2011

# load data in
data(USArrests)
stateCoord <- read.table("stateCoord.txt", header=T);

# merge the two datasets; new data set is my.arrests
my.arrests <- cbind(USArrests, stateCoord);

# write the new datasets to a new text file
write.table(my.arrests, file="USArrests_withCoord.txt", quote=F, col.names = T, row.names=F);

# plotting Murder vs. Assault
with(my.arrests, plot(Murder, Assault)) # notice linear trend

# plotting Rape vs. urban population
with(my.arrests, plot(Rape, UrbanPop))
# which one's the outlier?
with(my.arrests, which(Rape > 40 & UrbanPop < 60))
# add a point in red for the outlier
with(my.arrests, points(Rape[2], UrbanPop[2], col=2, pch=19))

# the non-lazy way:
# i call this the non-lazy way because in the previous method, i plotted ALL the points, and then i plotted OVER the outlier with a red point. unnecessary work! plus, annoying if you're anal like me and you notice that the point is a little thicker than the others. 
# the non-lazy way would be to plot all of the points SANS the outlier, then add the outlier in. like so:
with(my.arrests[-2,], plot(Rape, UrbanPop));
with(my.arrests, points(Rape[2], UrbanPop[2], col=2, pch=19));

# note, instead of using with, you could also try using attach
# i don't use it myself because i often forget to detach things...
# the problem with this is: when you attach a dataset, you attach only that current version of the dataset. so if you make changes to the dataset, then those are not reflected in the attached dataset. 
# however, if you make changes to the variables like Murder, Rape with syntax like Murder[2] <- blah blah, then you end up changing *what's attached* (but not necessarily the actual dataset). uncool!
# example using attach:
attach(my.arrests)
plot(Rape, UrbanPop);
which(Rape > 40 & UrbanPop > 60);
points(Rape[2], UrbanPop[2], col=2, pch=19)
detach(my.arrests);


# side-by-side plots
par(mfrow=c(1,2));
# plotting Murder vs. Assault
with(my.arrests, plot(Murder, Assault)) # notice linear trend
# plotting Rape vs. urban population
with(my.arrests, plot(UrbanPop, Rape))
# add a point in red for the outlier
with(my.arrests, points(UrbanPop[2], Rape[2], col=2, pch=19))

# side-by-side plots with text labels, also saved as PNG
# saving as PNG called "stateEDAplots.png"
png("stateEDAplots.png", width=500, height=300);
par(mfrow=c(1,2));
# make an empty plot with room for Murder vs. Assault
with(my.arrests, plot(1, 1, xlim=range(Murder), ylim=range(Assault), type="n", xlab="Murder", ylab="Assault"));
# plotting Murder vs. Assault
with(my.arrests, text(Murder, Assault, labels=row.names(my.arrests), cex=0.50))
# make an empty plot with room for UrbanPop vs. Rape
with(my.arrests, plot(1, 1, xlim=range(UrbanPop), ylim=range(Rape), type="n", xlab="UrbanPop", ylab="Rape"));
# plotting Rape vs. urban population
with(my.arrests[-2,], text(UrbanPop, Rape, labels=row.names(my.arrests), cex=0.50))
# add a point in red for the outlier, Alaska
with(my.arrests, text(UrbanPop[2], Rape[2], labels=row.names(my.arrests)[2], col=2, cex=0.50))
# exit the plotting device
dev.off();


# linear regression: rape on urbanpop
UrbanPop.Rape.OLS <- lm(Rape ~ UrbanPop, data = my.arrests);
plot(UrbanPop.Rape.OLS$fitted.values, UrbanPop.Rape.OLS$residuals); # notice the heteroscedasticity

# replot rape vs. urban pop
with(my.arrests, plot(UrbanPop, Rape));
# add regression line in blue
abline(UrbanPop.Rape.OLS, col=4);

# refit the model with the outlier removed
UrbanPop.Rape.nooutlier.OLS <- lm(Rape ~ UrbanPop, data = my.arrests[-2,]);
# plot this new regression line in red
abline(UrbanPop.Rape.nooutlier.OLS, col=2);

# the red line fits slightly better than the blue line, but both linear models are not a good description of the data due to the heteroscedasticity

title("Urban Population vs. Rape");
legend("topright", legend = c("Reg. line w/ outlier", "Reg. line w/o outlier"), col=c(4, 2), lty=1, bg = "white");


# replot with saving as PDF:
pdf("statesregression.pdf", width=5, height=5);
with(my.arrests, plot(UrbanPop, Rape));
abline(UrbanPop.Rape.OLS, col=4);
abline(UrbanPop.Rape.nooutlier.OLS, col=2);
title("Urban Population vs. Rape");
legend("topright", legend = c("Reg. line w/ outlier", "Reg. line w/o outlier"), col=c(4, 2), lty=1, bg = "white");
dev.off();














