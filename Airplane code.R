#
#
# Derek Jedamski
#
# Design your own project: Paper-Airplane
#
# Capstone (792)
#
#

# Read data in

plane<-read.csv('paperplane.csv',header=TRUE)

# Plot run order vs response

plot(plane[order(plane$Throw.Order),]$Distance,ylab='Distance',xlab='Run Order',main='Plotting Run Order vs Distance')

# Histogram for the response

hist(plane$Distance, density=40,main='Histogram of Flight Distances', xlab='Distance',ylab='Frequency')

# The potential of a transformation is something to keep in mind moving forward

# Plotting each factor against the response (wasn't used in report)

par(mfrow=c(2,3),mar=c(2,2,2,.2))
for(j in 1:6)
{
plot(plane$Distance,plane[,j],main=colnames(plane)[j])
}

#
# Formal Analysis
#

install.packages('FrF2')
library(FrF2)

#
# Including all factors for the half-normal plot
#

# Removing the order variables and isolating those used in the experiment

plane.new<-plane[,c(1:7)]

planes<-lm(Distance~(.)^6,data=plane.new)
planes$effects

# Generating the half-normal plot

DanielPlot(planes,half=TRUE,alpha=0.05)

# Running the reduced model based on 
# significant factors from half-normal
# Ommitted the Plane.Type:Paper.Size:Taped.Top 3fi

planes2<-lm(Distance~Plane.Type+Wing.Tips+Paper.Size+Plane.Type:Paper.Size+Plane.Type:Wing.Tips+Paper.Size:Wing.Tips+Plane.Type:Paper.Size:Wing.Tips,data=plane.new)

# Taking a look at effects and half-normal plot (not really necessary)

planes2$effects
DanielPlot(planes2,half=TRUE,alpha=0.05)

# Checking residuals
# Add back in the throw order to check the independence assumption

planecheck<-cbind(plane[order(plane$Throw.Order),],c(1:64))

par(mfrow=c(2,2))
plot(planes2, which=c(1,2,3))
plot(planecheck[,9], resid(planes2),ylab='Residuals',xlab='Run Order',main='Plotting Run Order vs Residuals')

# To get residuals vs factors I needed to convert
# the factors to numeric variables

plane$Plane.Type1<-as.numeric(plane$Plane.Type)
plane$Paper.Size1<-as.numeric(plane$Paper.Size)
plane$Wing.Tips1<-as.numeric(plane$Wing.Tips)
plane$Taped.Top1<-as.numeric(plane$Taped.Top)
plane$Paper.Clip1<-as.numeric(plane$Paper.Clip)
plane$Thrower1<-as.numeric(plane$Thrower)
par(mfrow=c(2,3), mar=c(2,4,2,0.5))
plot(plane$Plane.Type1,resid(planes2),xaxt='n',ylab='Residuals',main='Plane.Type'); axis(1,2:1,labels=c('Little Boy', 'Big Boy'))
plot(plane$Paper.Size1,resid(planes2),xaxt='n',ylab='Residuals',main='Paper.Size'); axis(1,2:1,labels=c('Small', 'Large'))
plot(plane$Wing.Tips1,resid(planes2),xaxt='n',ylab='Residuals',main='Wing.Tips'); axis(1,1:2,labels=c('no', 'yes'))
plot(plane$Taped.Top1,resid(planes2),xaxt='n',ylab='Residuals',main='Taped.Top'); axis(1,1:2,labels=c('no', 'yes'))
plot(plane$Paper.Clip1,resid(planes2),xaxt='n',ylab='Residuals',main='Paper.Clip'); axis(1,1:2,labels=c('no', 'yes'))
plot(plane$Thrower1,resid(planes2),xaxt='n',ylab='Residuals',main='Thrower'); axis(1,1:2,labels=c('jim', 'john'))

# A transformation may be necessary here

#
# Trying a transformation
#

# Using Box-Cox to determine best transformation
# Using the model using interactions up to size 5.
# Interactions of size 6 are rarely significant

library(MASS)
box<-boxcox(Distance~(.)^5, data=plane.new, lambda=seq(-1,1.5,length=30),plotit=TRUE)
lambda<-box$x[which.max(box$y)]

# Many different transformations were tested out
# including exponents of 0.4, 0.5, 0.6, 0.7, 0.8
# and the log transformation. They were evaluated 
# based on model size, adjusted R-squared, and residuals
# It was determined square-root is best for this model

# Using a square root transformation
# Looking at histogram of transformed response

hist((plane$Distance)^0.5, breaks=8,density=40,main='Histogram of Flight Distance with Square-Root Transformation', xlab='Distance',ylab='Frequency')

plane.new$Distance<-(plane.new$Distance)^0.5
planes<-lm(Distance~(.)^6,data=plane.new)
planes$effects

# Generating the half-normal plot

DanielPlot(planes,half=TRUE,alpha=0.05)

# Running the reduced model based on 
# significant factors from half-normal
# Ommitted the Plane.Type:Paper.Size:Taped.Top 3fi

# This potential outlier was removed just to check
# the impact it may be having. Once it was determined
# that it wasn't really having an impact, it was added
# back in and the analysis was completed with all observations.
# plane.new<-plane.new[-c(39),]

planes2<-lm(Distance~Plane.Type+Wing.Tips+Paper.Size+Plane.Type:Paper.Size+Plane.Type:Wing.Tips+Paper.Size:Wing.Tips+Plane.Type:Paper.Size:Wing.Tips,data=plane.new)

# Taking a look at effects and half-normal plot (not really necessary)

planes2$effects
DanielPlot(planes2,half=TRUE,alpha=0.05)

# Checking residuals

# Adding back in the throw order to check the independence assumption

planecheck<-cbind(plane[order(plane$Throw.Order),],c(1:64))

par(mfrow=c(2,2))
plot(planes2, which=c(1,2,3))
plot(planecheck[,8], resid(planes2),ylab='Residuals',xlab='Run Order',main='Plotting Run Order vs Residuals')

# To get residuals vs factors I needed to convert
# the factors to numeric variables

plane.new$Plane.Type1<-as.numeric(plane.new$Plane.Type)
plane.new$Paper.Size1<-as.numeric(plane.new$Paper.Size)
plane.new$Wing.Tips1<-as.numeric(plane.new$Wing.Tips)
plane.new$Taped.Top1<-as.numeric(plane.new$Taped.Top)
plane.new$Paper.Clip1<-as.numeric(plane.new$Paper.Clip)
plane.new$Thrower1<-as.numeric(plane.new$Thrower)
par(mfrow=c(2,3), mar=c(2,4,2,0.5))
plot(plane.new$Plane.Type1,resid(planes2),xaxt='n',ylab='Residuals',main='Plane.Type'); axis(1,2:1,labels=c('Little Boy', 'Big Boy'))
plot(plane.new$Paper.Size1,resid(planes2),xaxt='n',ylab='Residuals',main='Paper.Size'); axis(1,2:1,labels=c('Small', 'Large'))
plot(plane.new$Wing.Tips1,resid(planes2),xaxt='n',ylab='Residuals',main='Wing.Tips'); axis(1,1:2,labels=c('no', 'yes'))
plot(plane.new$Taped.Top1,resid(planes2),xaxt='n',ylab='Residuals',main='Taped.Top'); axis(1,1:2,labels=c('no', 'yes'))
plot(plane.new$Paper.Clip1,resid(planes2),xaxt='n',ylab='Residuals',main='Paper.Clip'); axis(1,1:2,labels=c('no', 'yes'))
plot(plane.new$Thrower1,resid(planes2),xaxt='n',ylab='Residuals',main='Thrower'); axis(1,1:2,labels=c('jim', 'john'))

# Re-doing normality for each level of wing tips

plane.new<-cbind(plane.new,resid(planes2))
library(car)

par(mfrow=c(1,2))
qqPlot(plane.new[which(plane.new$Wing.Tips=='yes'),14], ylab='Residuals',main='Wing Tips Folded')
qqPlot(plane.new[which(plane.new$Wing.Tips=='no'),14], ylab='Residuals',main='Wing Tips Not Folded')

# Checking for outliers

plot(planes2, which=4)

# Send results to text file

write.table(round((planes2$fit^2),1), '/Volumes/PUBLIC/RIT Year 2/Spring/Capstone/Paper Airplane/Distance_Pred.txt', col.names=FALSE, row.names=FALSE, sep=" ")

# Finding optimal levels for each factor

IAPlot(planes2,select=c(1,2))

# Plotting the raw data on a 3-dimensional plot

install.packages('scatterplot3d')
library(scatterplot3d)

s3d<-scatterplot3d(plane[,3],plane[,2],plane[,7],pch=ifelse(plane[,1]=='Big Boy',2,1),lab=c(1,1,20),x.ticklabs=c('Not Folded','Folded'),y.ticklabs=c('Large','Small'),color=ifelse(plane[,1]=='Big Boy','red','blue'),xlab='Wing Tips',ylab='Paper Size',zlab='Distance', main='Plotting the 3-Dimensional Interaction')
legend(s3d$xyz.convert(1.3, 2, 49),title='Plane Type',pch = c(2,1),col=c('red','blue'),legend = c("Big Boy", "Little Boy"))

# Generating the 2-way interaction plot
# for no wing tips

install.packages('stats')
library(stats)

plane.new$Distance=(plane.new$Distance)^2

interaction.plot(plane.new[which(plane.new$Wing.Tips=='no'),]$Paper.Size,plane.new[which(plane.new$Wing.Tips=='no'),]$Plane.Type,plane.new[which(plane.new$Wing.Tips=='no'),]$Distance, ylim=range(0,40), trace.label='Plane Type',xlab='Size of Paper',ylab='Distance',col=c(2,1),pch=c(1,5),type='b', main='No Wing Tips',lwd=2,fixed=T)

# Seeing where optimal settings lie in actual and predicted distance

plot((planes2$fit)^2,plane$Distance, xlab='Predicted Distance',xlim=c(0,50),ylab='Actual Distance',ylim=c(0,50),col=ifelse(plane[,1]=='Big Boy' & plane[,2]=='large' & plane[,3]=='no','red','black'),pch=ifelse(plane[,1]=='Big Boy' & plane[,2]=='large' & plane[,3]=='no',3,1),main='Predicted vs Actual Distance \n with optimal settings highlighted')

# Calculating the mean and standard deviation for optimal settings

optimal<-plane[plane[,1]=='Big Boy' & plane[,2]=='large' & plane[,3]=='no',c(1,2,3,7)]
mean(optimal[,4])
sd(optimal[,4])

# Calculating the probability any of these paper airplanes
# exceed 30, 35, 40, 45, 50, and 55 feet.

# These were used just to transform the response
# back and forth just for interpretation checks
# plane.new$Distance <- plane.new$Distance^2
# plane.new$Distance <- sqrt(plane.new$Distance)

summary<- data.frame(matrix(nrow=8, ncol=8))
colnames(summary)<-c('Distance_Mean', 'Distance_StDev', '30 feet', '35 feet','40 feet','45 feet','50 feet', '55 feet')

summary[1,1]<-round(mean(plane.new[which(plane.new$Plane.Type=='Big Boy' & plane.new$Wing.Tips=='no' & plane.new$Paper.Size=='large'),7]),2)
summary[1,2]<-round(sd(plane.new[which(plane.new$Plane.Type=='Big Boy' & plane.new$Wing.Tips=='no'& plane.new$Paper.Size=='large'),7]),2)
summary[2,1]<-round(mean(plane.new[which(plane.new$Plane.Type=='Big Boy' & plane.new$Wing.Tips=='no' & plane.new$Paper.Size=='small'),7]),2)
summary[2,2]<-round(sd(plane.new[which(plane.new$Plane.Type=='Big Boy' & plane.new$Wing.Tips=='no'& plane.new$Paper.Size=='small'),7]),2)
summary[3,1]<-round(mean(plane.new[which(plane.new$Plane.Type=='Big Boy' & plane.new$Wing.Tips=='yes' & plane.new$Paper.Size=='large'),7]),2)
summary[3,2]<-round(sd(plane.new[which(plane.new$Plane.Type=='Big Boy' & plane.new$Wing.Tips=='yes'& plane.new$Paper.Size=='large'),7]),2)
summary[4,1]<-round(mean(plane.new[which(plane.new$Plane.Type=='Big Boy' & plane.new$Wing.Tips=='yes' & plane.new$Paper.Size=='small'),7]),2)
summary[4,2]<-round(sd(plane.new[which(plane.new$Plane.Type=='Big Boy' & plane.new$Wing.Tips=='yes'& plane.new$Paper.Size=='small'),7]),2)
summary[5,1]<-round(mean(plane.new[which(plane.new$Plane.Type=='Little Boy' & plane.new$Wing.Tips=='no' & plane.new$Paper.Size=='large'),7]),2)
summary[5,2]<-round(sd(plane.new[which(plane.new$Plane.Type=='Little Boy' & plane.new$Wing.Tips=='no'& plane.new$Paper.Size=='large'),7]),2)
summary[6,1]<-round(mean(plane.new[which(plane.new$Plane.Type=='Little Boy' & plane.new$Wing.Tips=='no' & plane.new$Paper.Size=='small'),7]),2)
summary[6,2]<-round(sd(plane.new[which(plane.new$Plane.Type=='Little Boy' & plane.new$Wing.Tips=='no'& plane.new$Paper.Size=='small'),7]),2)
summary[7,1]<-round(mean(plane.new[which(plane.new$Plane.Type=='Little Boy' & plane.new$Wing.Tips=='yes' & plane.new$Paper.Size=='large'),7]),2)
summary[7,2]<-round(sd(plane.new[which(plane.new$Plane.Type=='Little Boy' & plane.new$Wing.Tips=='yes'& plane.new$Paper.Size=='large'),7]),2)
summary[8,1]<-round(mean(plane.new[which(plane.new$Plane.Type=='Little Boy' & plane.new$Wing.Tips=='yes' & plane.new$Paper.Size=='small'),7]),2)
summary[8,2]<-round(sd(plane.new[which(plane.new$Plane.Type=='Little Boy' & plane.new$Wing.Tips=='yes'& plane.new$Paper.Size=='small'),7]),2)

for (i in c(30,35,40,45,50,55))
{
summary[1,(i/5-3)]<-1-pnorm(sqrt(i),mean=summary[1,1],sd=summary[1,2])
summary[2,(i/5-3)]<-1-pnorm(sqrt(i),mean=summary[2,1],sd=summary[2,2])
summary[3,(i/5-3)]<-1-pnorm(sqrt(i),mean=summary[3,1],sd=summary[3,2])
summary[4,(i/5-3)]<-1-pnorm(sqrt(i),mean=summary[4,1],sd=summary[4,2])
summary[5,(i/5-3)]<-1-pnorm(sqrt(i),mean=summary[5,1],sd=summary[5,2])
summary[6,(i/5-3)]<-1-pnorm(sqrt(i),mean=summary[6,1],sd=summary[6,2])
summary[7,(i/5-3)]<-1-pnorm(sqrt(i),mean=summary[7,1],sd=summary[7,2])
summary[8,(i/5-3)]<-1-pnorm(sqrt(i),mean=summary[8,1],sd=summary[8,2])
}

round(summary,4)

# Generating the mean and standard deviation when
# adding the taped top factor.

round(mean(plane.new[which(plane.new$Plane.Type=='Big Boy' & plane.new$Wing.Tips=='no' & plane.new$Paper.Size=='large' & plane.new$Taped.Top=='yes'),7]),2)
round(sd(plane.new[which(plane.new$Plane.Type=='Big Boy' & plane.new$Wing.Tips=='no'& plane.new$Paper.Size=='large' & plane.new$Taped.Top=='yes'),7]),2)

# Looking at the optimal settings vs the alternate solutions

plot((planes2$fit)^2,plane$Distance, xlab='Predicted Distance',xlim=c(0,50),ylab='Actual Distance',ylim=c(0,50),col=ifelse(plane[,1]=='Big Boy' & plane[,2]=='large' & plane[,3]=='no','red',ifelse(plane[,1]=='Big Boy' & plane[,2]=='large' & plane[,3]=='yes','blue','black')),pch=ifelse(plane[,1]=='Big Boy' & plane[,2]=='large' & plane[,3]=='no',3,ifelse(plane[,1]=='Big Boy' & plane[,2]=='large' & plane[,3]=='yes',2,1)),main='Predicted vs Actual Distance \n with levels of wing tips highlighted')
legend('topleft',c('Wing Tips','No Tips'), pch=c(2,3), col=c('blue','red'))

# Looking at the two different levels of the taped top factor

plot((planes2$fit)^2,plane$Distance, xlab='Predicted Distance',xlim=c(0,50),ylab='Actual Distance',ylim=c(0,50),col=ifelse(plane[,1]=='Big Boy' & plane[,2]=='large' & plane[,3]=='no' & plane[,4]=='yes','red',ifelse(plane[,1]=='Big Boy' & plane[,2]=='large' & plane[,3]=='no' & plane[,4]=='no','blue','black')),pch=ifelse(plane[,1]=='Big Boy' & plane[,2]=='large' & plane[,3]=='no' & plane[,4]=='yes',3,ifelse(plane[,1]=='Big Boy' & plane[,2]=='large' & plane[,3]=='no' & plane[,4]=='no',2,1)),main='Predicted vs Actual Distance \n with levels of taped top highlighted')
legend('topleft',c('No Taped Top','Taped Top'), pch=c(2,3), col=c('blue','red'))
