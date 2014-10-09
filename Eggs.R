# read in data # 

eggs<-read.csv('Eggs.csv',header=T)

##### validity check #####
table(eggs$S)


#### informal analysis ####
# Generating mean for each level  of size
aggregate(M~S,eggs,mean)
aggregate(M~S,eggs,sd)


# 95% Confidence Interval


eggs.S1<-eggs[which(eggs$S=='1'),]
eggs.S2<-eggs[which(eggs$S=='2'),]
eggs.S3<-eggs[which(eggs$S=='3'),]
eggs.S4<-eggs[which(eggs$S=='4'),]
eggs.S5<-eggs[which(eggs$S=='5'),]


 CI<- data.frame(matrix(nrow=5, ncol=3))
 names(CI)<-c("mean", "lower","upper")
 
 bounds<-qnorm(.95)*(sd(eggs.S1$M)/sqrt(nrow(eggs.S1)))
 CI[1,1]<-round(mean(eggs.S1$M),2)
 CI[1,3]<- round(CI[1,1]+bounds,2)
 CI[1,2]<- round(CI[1,1]-bounds,2)
 
 bounds<-qnorm(.95)*(sd(eggs.S2$M)/sqrt(nrow(eggs.S2)))
 CI[2,1]<-round(mean(eggs.S2$M),2)
 CI[2,3]<- round(CI[2,1]+bounds,2)
 CI[2,2]<- round(CI[2,1]-bounds,2)
 
 bounds<-qnorm(.95)*(sd(eggs.S3$M)/sqrt(nrow(eggs.S3)))
 CI[3,1]<-round(mean(eggs.S3$M),2)
 CI[3,3]<- round(CI[3,1]+bounds,2)
 CI[3,2]<- round(CI[3,1]-bounds,2)
 
 bounds<-qnorm(.95)*(sd(eggs.S4$M)/sqrt(nrow(eggs.S4)))
 CI[4,1]<-round(mean(eggs.S4$M),2)
 CI[4,3]<- round(CI[4,1]+bounds,2)
 CI[4,2]<- round(CI[4,1]-bounds,2)
 
 bounds<-qnorm(.95)*(sd(eggs.S5$M)/sqrt(nrow(eggs.S5)))
 CI[5,1]<-round(mean(eggs.S5$M),2)
 CI[5,3]<- round(CI[5,1]+bounds,2)
 CI[5,2]<- round(CI[5,1]-bounds,2)


 CI


# Generating plots for width, length, and their interaction vs weight, each with colored groups for each size egg


eggs$S<-as.factor(eggs$S)


S.name<-c("small","medium","large","extra large","jumbo")


par(mfrow=c(1,2))
with(eggs,plot(L,M,col=S,xlab="Length",ylab="Weight",main="Scatterplot for Weight vs Length"))
legend("topleft",title="Size", S.name,text.width=strwidth('1100'),fill=levels(eggs$S))
with(eggs,plot(W,M,col=S,xlab="Width",ylab="Weight",main="Scatterplot for Weight vs Width"))
legend("topleft",title="Size",S.name,text.width=strwidth('1100'),fill=levels(eggs$S))


with(eggs,plot(L*W,M,col=S,xlab="Length*Width",ylab="Weight",main="Scatterplot for Weight vs Interaction of Length and Width"))
legend("topleft",title="Size",S.name,fill=levels(eggs$S))



# Generating a 3D plot to see the two main inputs vs the response with colored groups for size of egg
install.packages('scatterplot3d')
library(scatterplot3d)
x11()
attach(eggs)
s3d<-with(eggs,scatterplot3d(L,W,M, pch=19, color=as.numeric(eggs$S), xlab='Length',ylab='Width',zlab='Weight',main='3D plot of Length vs Width vs Weight'))
legend(s3d$xyz.convert(2,2,90),title='Size',legend=S.name,pch=19,col=seq_along(levels(eggs$S)))



# density plot for all factors


install.packages("sm")
library(sm)


# Weight by Size #


x11()
with(eggs,sm.density.compare(M, S, xlab="Weight",lwd=2))
title("Density Plot for Weight")
colfill<-c(2:(2+length(levels(eggs$S)))) 
legend("topright",title="Size",S.name,fill=colfill)


# Length by Size #


x11()
with(eggs,sm.density.compare(L, S, xlab="Length",lwd=2))
title("Density Plot for Length")
legend("topright",title="Size",S.name,fill=colfill)


# Width by Size #


x11()
with(eggs,sm.density.compare(W, S, xlab="Width",lwd=2))
title("Density Plot for Width")
legend("topright",title="Size",S.name,fill=colfill)



######### Formal Analysis #######
# Multicollinearity check
 # Correlation Matrix 
eggs.check<-eggs[,2:4]
 eggs.check$S<-as.numeric(eggs$S)
 round(cor(eggs.check),2)


 #Compute the new matrix X'X.
 eggs.check<- as.matrix(eggs.check)
 M <- t(eggs.check)%*%eggs.check
 print(round(M,2))


#Compute the determinant of X'X
 det(M)


 #Compute the eigenvalues of X'X
 lambda <-eigen(M)$values
 print(round(lambda,2))


#Compute the Condition Number
 k <- max(lambda)/min(lambda)
 k 


#Show on a table a row with lambda(i) where i = 1,2,...,p
 #and a second row with sqrt(lambda(mx)/lambda(j))
 #Condition Indexes
 Z <- max(lambda)/lambda
 print(round(sqrt(Z),2))


# Making ratio length and width
 eggs<-as.data.frame(eggs)
 attach(eggs)


# a.using the shape index
eggs$sindex<-((eggs$W)^2)*eggs$L
summary(eggs$sindex)
plot(M~sindex,data=eggs,xlab="Shape Index",ylab="Weight",
     main="Scatter Plot for Weight vs Shape Index",col=S)
legend("topleft",cex=0.7,title="Size",S.name,fill=c(1,2,3,4,5))
abline(v=c(72,76),col="grey",lty=2) 


# b.build a model with all three terms and model check
library(MASS)
lm0<-lm(M~1,data=eggs)
lmfull<-lm(M~L+W+sindex,data=eggs)
lmstep<-stepAIC(lmfull, scope=list(upper=lmfull, lower=lm0), direction="both")

lmstep<-lm(M~L+W+sindex,data=eggs)
summary(lmstep)

library(car)
par(mfrow=c(1,2))
qqPlot(lmstep)
plot(lmstep$fitted,rstandard(lmstep),xlab="Fitted Values",ylab="Standardized Residuals")
abline(h=0,col="grey",lty=2)
par(mfrow=c(2,2))
plot(eggs$L,rstandard(lmstep),xlab="Length",ylab="Standardized Residuals")
abline(h=0,col="grey",lty=2)
plot(eggs$W,rstandard(lmstep),xlab="Width",ylab="Standardized Residuals")  # non-constant variance #
abline(h=0,col="grey",lty=2)
plot(eggs$L*eggs$W,rstandard(lmstep),xlab="Length*Width",ylab="Standardized Residuals")
abline(h=0,col="grey",lty=2)
plot(eggs$sindex,rstandard(lmstep),xlab="Shape Index",ylab="Standardized Residuals")
abline(h=0,col="grey",lty=2)

install.packages('car')
library(car)
round(vif(lmstep),2)

# Build a model with just the shape index and model check

lm1<-lm(M~sindex,data=eggs)
summary(lm1)

library(car)
par(mfrow=c(1,2))
qqPlot(lm1)
plot(lm1$fitted,rstandard(lm1),xlab="Fitted Values",ylab="Standardized Residuals")
abline(h=0,col="grey",lty=2)
par(mfrow=c(2,2))
plot(eggs$L,rstandard(lm1),xlab="Length",ylab="Standardized Residuals")
abline(h=0,col="grey",lty=2)
plot(eggs$W,rstandard(lm1),xlab="Width",ylab="Standardized Residuals")  # non-constant variance #
abline(h=0,col="grey",lty=2)
plot(eggs$L*eggs$W,rstandard(lm1),xlab="Length*Width",ylab="Standardized Residuals")
abline(h=0,col="grey",lty=2)
plot(eggs$sindex,rstandard(lm1),xlab="Shape Index",ylab="Standardized Residuals")
abline(h=0,col="grey",lty=2)


# d.naive regression not constant
naive<-lm(M~L+W, data=eggs)
summary(naive)


par(mfrow=c(2,2))
qqPlot(naive)
plot(naive$fitted,rstandard(naive),xlab="Fitted Values",ylab="Standardized Residuals")
abline(h=0,col="grey",lty=2)
plot(eggs$L,rstandard(naive),xlab="Length",ylab="Standardized Residuals")
abline(h=0,col="grey",lty=2)
plot(eggs$W,rstandard(naive),xlab="Width",ylab="Standardized Residuals")
abline(h=0,col="grey",lty=2)

# Altering the naive model

new.naive<-lm(sqrt(M)~L+W, data=eggs)
summary(new.naive)

par(mfrow=c(2,2))
qqPlot(new.naive)
plot(new.naive$fitted,rstandard(new.naive),xlab="Fitted Values",ylab="Standardized Residuals")
abline(h=0,col="grey",lty=2)
plot(eggs$L,rstandard(new.naive),xlab="Length",ylab="Standardized Residuals")
abline(h=0,col="grey",lty=2)
plot(eggs$W,rstandard(new.naive),xlab="Width",ylab="Standardized Residuals")
abline(h=0,col="grey",lty=2)

# e.S impact M and L
eggs$S<-as.factor(eggs$S)
lm2<-lm(M~sindex+S,data=eggs)
lm2aov<-anova(lm2)
lm2aov


with(eggs,pairwise.t.test(M,S))


install.packages('TukeyC')
library(TukeyC)


x11()
lm3aov<-aov(M~S,data=eggs)
plot(TukeyHSD(lm3aov))
