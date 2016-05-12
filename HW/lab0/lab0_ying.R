# Lab 0 Basic R Skills

#onto the data
#(1)
data(USArrests)
#(2)
stateCoord <- read.table("stateCoord.txt")
head(stateCoord)
#(3)
#mydata <- merge(USArrests,stateCoord)
#Merge the two datasets into a single data frame
mydata <- cbind(USArrests,stateCoord)
head(mydata)
#(4)
#write.table(mydata, file="mydata.txt", sep="\t")
write.table(mydata, file="mydata.txt", sep="\t", quote=FALSE, col.names=TRUE, row.names=FALSE)

#comparing two variables
#(1)
#plot(mydata$Murder,mydata$Assault) #linear
with(mydata, plot(Murder, Assault))
#(2)
plot(mydata$UrbanPop,mydata$Rape)
#outlier
index=which(mydata$UrbanPop<50 & mydata$Rape>40)
#points(mydata$UrbanPop[index],mydata$Rape[index],col=2)
points(mydata$UrbanPop[index],mydata$Rape[index],col=2,pch=19) #red solid circle
#(3)
par(mfcol=c(1,2))
with(mydata, plot(Murder, Assault))
with(mydata, plot(UrbanPop, Rape))
points(mydata$UrbanPop[index],mydata$Rape[index],col=2,pch=19)
#(4) (refer to solution)
png("mydata.png")
par(mfcol=c(1,2))
#with(mydata, plot(Murder, Assault))
with(mydata, plot(0,0,xlim=range(Murder),ylim=range(Assault),type="n",xlab="Murder",ylab="Assault"))
with(mydata,text(Murder,Assault,labels=row.names(mydata),cex=0.5))
#with(mydata, plot(UrbanPop, Rape))
with(mydata[-index,], plot(0,0,xlim=range(UrbanPop),ylim=range(Rape),type="n",xlab="UrbanPop",ylab="Rape"))
with(mydata[-index,],text(UrbanPop,Rape,labels=row.names(mydata),cex=0.5))
text(mydata$UrbanPop[index],mydata$Rape[index],labels=row.names(mydata)[index],col=2,cex=0.5)
dev.off()
#(5) see above

#regression
#(1)
popRape.LS<-lm(Rape~UrbanPop,data=mydata)
summary(popRape.LS)
#(2)
names(popRape.LS)
with(popRape.LS,plot(fitted.values,residuals)) # not completely random
#(3)
with(mydata, plot(UrbanPop, Rape))
abline(popRape.LS,col="blue") #the use of lm
#(4)
pdf("mydata_regression.pdf")
popRape.LS.new<-lm(Rape~UrbanPop,data=mydata[-index,]) 
with(mydata, plot(UrbanPop, Rape))
abline(popRape.LS,col="blue")
abline(popRape.LS.new,col="red")
#(5)
# not good due to the heteroscedasticity
#(6)
title("UrbanPop vs. Rape")
leg.txt <- c("Regression Line with outlier", "Regression Line without outlier")
legend("topleft",legend=leg.txt,col=c("red","blue"),lty=1)
dev.off()
#(7) see above




