#
#
# Derek Jedamski
#
# Six Cavity Mold
#
# Capstone (792)
#
#

# Creating a dataframe with the data

cavity<-c(1,2,3,4,5,6)
mean<-c(49.50,49.54,49.95,50.05,50.46,50.50)
sd<-c(0.08,0.12,0.12,0.14,0.10,0.06)

cavity.mold<-data.frame(cavity,mean,sd)

# Setting the x-axis

x<-seq(from=49,to=51,length.out=500)

# Plotting all of the conditional distributions

plot(x,dnorm(x,mean=49.5,sd=0.08),main='Conditional Distributions for each Cavity',ylab='Density', xlab='Diameter (mm)',type='l',lty=2, ylim=c(0,7))
abline(v=c(49.35,50.65), col=3, lty=2, lwd=2)
abline(v=c(49.3,50.7), col=14, lty=3, lwd=2)
abline(v=50, col=18, lty=2, lwd=1)
abline(h=0)
for (i in 2:6)
{
lines(x,dnorm(x,mean=cavity.mold[i,2],cavity.mold[i,3]),lty=2)
}
# Add unconditional distribution as sum of pdfs
lines(x,(1/6)*dnorm(x,mean=49.5,sd=0.08)+(1/6)*dnorm(x,mean=49.54,sd=0.12)+(1/6)*dnorm(x,mean=49.95,sd=0.12)+(1/6)*dnorm(x,mean=50.05,sd=0.14)+(1/6)*dnorm(x,mean=50.46,sd=0.1)+(1/6)*dnorm(x,mean=50.5,sd=0.06),col='red',lwd=3)

# Calculating exact total fraction out of specification
# individually by cavity

summary<-data.frame(matrix(nrow=6, ncol=2))
colnames(summary)<-c('50+/-0.7', '50+/-0.65')
row.names(summary)<-row.names(cavity.mold)

for (i in 1:6)
{
summary[i,1]<-1-(pnorm(50.7,mean=cavity.mold[i,2],sd=cavity.mold[i,3])-pnorm(49.3,cavity.mold[i,2],sd=cavity.mold[i,3]))
summary[i,2]<-1-(pnorm(50.65,mean=cavity.mold[i,2],sd=cavity.mold[i,3])-pnorm(49.35,cavity.mold[i,2],sd=cavity.mold[i,3]))
}
round(summary,5)

# The exact total fraction out of specification is simply
# the mean of fraction out of spec of the individuals
# Calculating the total fraction out of spec for each
# of the spec limits
colMeans(summary)

# Adding the fraction out of spec to the main data frame
new.mold<-cbind(cavity.mold,round(summary,5))

# Adding the normal approximation to the unconditional dist
# with exact mean and sd (calculated in report)
lines(x,dnorm(x,mean=50,sd=0.4076), col='blue', lwd=3)

#Calculating fraction out of spec for the normal approximation
1-(pnorm(50.7,mean=50,sd=0.4076)-pnorm(49.3,mean=50,sd=0.4076))
1-(pnorm(50.65,mean=50,sd=0.4076)-pnorm(49.35,mean=50,sd=0.4076))

#
# Shifting means
#

# First set of spec limits (tighter)
# Calculating fraction out of spec for 14 different shifts
shift1<-data.frame(matrix(nrow=6, ncol=14))
colnames(shift1)<-c('-0.020','-0.015', '-0.010','-0.005','0','+0.005','+0.010','+0.015','+0.020','+0.025','+0.030','+0.035','+0.040','+0.045')
for (i in 1:14)
{
shift1[1,i]<-1-(pnorm(50.65,mean=49.5+0.005*(i-5),sd=0.08)-pnorm(49.35,mean=49.5+0.005*(i-5),sd=0.08))
shift1[2,i]<-1-(pnorm(50.65,mean=49.54+0.005*(i-5),sd=0.12)-pnorm(49.35,mean=49.54+0.005*(i-5),sd=0.12))
shift1[3,i]<-1-(pnorm(50.65,mean=49.95+0.005*(i-5),sd=0.12)-pnorm(49.35,mean=49.95+0.005*(i-5),sd=0.12))
shift1[4,i]<-1-(pnorm(50.65,mean=50.05+0.005*(i-5),sd=0.14)-pnorm(49.35,mean=50.05+0.005*(i-5),sd=0.14))
shift1[5,i]<-1-(pnorm(50.65,mean=50.46+0.005*(i-5),sd=0.10)-pnorm(49.35,mean=50.46+0.005*(i-5),sd=0.10))
shift1[6,i]<-1-(pnorm(50.65,mean=50.5+0.005*(i-5),sd=0.06)-pnorm(49.35,mean=50.5+0.005*(i-5),sd=0.06))
}

# Calculating the total fraction out of specification by
# taking the average of the out of spec for individuals
# Then plotting the fraction out of spec for each shift

colMeans(shift1)
plot(colMeans(shift1), xlab='Shift',ylab='Fraction out of Spec', type='o',xaxt='n', main='Fraction out of Specification by Mean Shifts')
axis(1,1:14,labels=c('-0.020','-0.015', '-0.010','-0.005','0','+0.005','+0.010','+0.015','+0.020','+0.025','+0.030','+0.035','+0.040','+0.045'))

# Adding new means to the dataframe

new.mold$newMean<-new.mold$mean+0.015

# Plotting the updated conditional distributions

plot(x,dnorm(x,mean=49.515,sd=0.08),main='Conditional Distributions for each Cavity \n after Mean Shifts',ylab='Density', xlab='Diameter',type='l',lty=2, ylim=c(0,7))
abline(v=c(49.35,50.65), col=3, lty=2, lwd=2)
abline(v=50, col=18, lty=2, lwd=1)
abline(h=0)
for (i in 2:6)
{
lines(x,dnorm(x,mean=new.mold[i,6],new.mold[i,3]),lty=2)
}

# Second set of spec limits (wider)
# Calculating fraction out of spec for 14 different shifts
shift2<-data.frame(matrix(nrow=6, ncol=14))
colnames(shift2)<-c('-0.015', '-0.010','-0.005','0','+0.005','+0.010','+0.015','+0.020','+0.025','+0.030','+0.035','+0.040', '+0.045','+0.50')
for (i in 1:14)
{
shift2[1,i]<-1-(pnorm(50.7,mean=49.5+0.005*(i-4),sd=0.08)-pnorm(49.3,mean=49.5+0.005*(i-4),sd=0.08))
shift2[2,i]<-1-(pnorm(50.7,mean=49.54+0.005*(i-4),sd=0.12)-pnorm(49.3,mean=49.54+0.005*(i-4),sd=0.12))
shift2[3,i]<-1-(pnorm(50.7,mean=49.95+0.005*(i-4),sd=0.12)-pnorm(49.3,mean=49.95+0.005*(i-4),sd=0.12))
shift2[4,i]<-1-(pnorm(50.7,mean=50.05+0.005*(i-4),sd=0.14)-pnorm(49.3,mean=50.05+0.005*(i-4),sd=0.14))
shift2[5,i]<-1-(pnorm(50.7,mean=50.46+0.005*(i-4),sd=0.10)-pnorm(49.3,mean=50.46+0.005*(i-4),sd=0.10))
shift2[6,i]<-1-(pnorm(50.7,mean=50.5+0.005*(i-4),sd=0.06)-pnorm(49.3,mean=50.5+0.005*(i-4),sd=0.06))
}

# Calculating the total fraction out of specification by
# taking the average of the out of spec for individuals
# Then plotting the fraction out of spec for each shift

colMeans(shift2)
plot(colMeans(shift2))
plot(colMeans(shift2), xlab='Shift',ylab='Fraction out of Spec', type='o',xaxt='n', main='Fraction out of Specification by Mean Shifts')
axis(1,1:14,labels=c('-0.015', '-0.010','-0.005','0','+0.005','+0.010','+0.015','+0.020','+0.025','+0.030','+0.035','+0.040','+0.045','+0.050'))

# Adding the new means to the dataframe

new.mold$newMean2<-new.mold$mean+0.02

# Plotting the updated conditional distributions

plot(x,dnorm(x,mean=49.52,sd=0.08),main='Conditional Distributions for each Cavity \n after Mean Shifts',ylab='Density', xlab='Diameter',type='l',lty=2, ylim=c(0,7))
abline(v=c(49.3,50.7), col=14, lty=3, lwd=2)
abline(v=50, col=18, lty=2, lwd=1)
abline(h=0)
for (i in 2:6)
{
lines(x,dnorm(x,mean=new.mold[i,7],new.mold[i,3]),lty=2)
}