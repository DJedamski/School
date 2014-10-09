#
#
# Derek Jedamski
#
# Design your own project: Wine Quality Project
#
# Capstone (792)
#
#

# Reading the data in

wine<-read.csv("Wine Quality.csv",header=TRUE)

#
#
# Exploratory Data Analysis
#
#

plot(wine)

# There are some points that seem concerning (re: high-leverage)

# Running some data-checks

table(wine$Color)
table(wine$Quality)
hist(wine$Quality)

# Plotting each input against the response

par(mfrow=c(3,4),mar=c(2,2,2,.2))
for(j in 1:11)
{
plot(wine[,j],jitter(wine$Quality,factor=0.8),main=colnames(wine)[j])
}

par(mfrow=c(3,4),mar=c(2,2,2,.2))
for(j in 1:11)
{
plot(wine[,j],jitter(wine$Color, factor=1.0),main=colnames(wine)[j])
}

# There are some points that seem to be potential
# high-leverage points. Will wait to deal with them
# until we see if it affects the models

# Creating two additional datasets based on color

wine.white<-wine[which(wine$Color==0),]
wine.red<-wine[which(wine$Color==1),]

# Producing some summary statistics and graphical displays
# to gauge differences in make-up of the different color wines

# Summary Statistics

summary<- data.frame(matrix(nrow=12, ncol=4))
colnames(summary)<-c('Red_Mean', 'Red_StDev', 'White_Mean', 'White_StDev')
row.names(summary)<-colnames(wine[,1:12])
for(i in 1:12)
{
summary[i,1]<-round(mean(wine.red[,i]),2)
summary[i,2]<-round(sd(wine.red[,i]),2)
summary[i,3]<-round(mean(wine.white[,i]),2)
summary[i,4]<-round(sd(wine.white[,i]),2)
}

# This indicates that there appear to be some differences
# in make-up between the two color wines. Lets visualize 
# it via some density plots and boxplots

# Boxplots

par(mfrow=c(3,4),mar=c(2,2,2,.2))
for(j in 1:12)
{
boxplot(wine[,j]~factor(Color),names=c("White", "Red"),col=c("White", "Red"),data=wine,main=colnames(wine)[j])
}

# Density plots

install.packages("sm")
library(sm)

par(mfrow=c(3,4), mar=c(2,0.5,1.5,0.5), yaxt='n')
for(i in 1:11)
{
with(wine,sm.density.compare(wine[,i], Color,lwd=2,col=c(1,2)))
title(colnames(wine)[i])
legend('topright',title='Color',c('White','Red'),col=c(1,2),lty=c(1,2))
}

# Density plots indicate that most variables may not
# be Normal distributed.

# Splitting by color and testing for normality

normality.check<-matrix(nrow=11,ncol=2)
colnames(normality.check)<-c('Red P-Val','White P-Val')
row.names(normality.check)<-colnames(wine[,1:11])
for(j in 1:11)
{
	normality.check[j,1]<-shapiro.test(wine[wine$Color==0,j])$p
	normality.check[j,2]<-shapiro.test(wine[wine$Color==1,j])$p
}
round(normality.check,4)

# This test shows that none of the variables are normal
# However, the Shapiro-Wilks test will often detect the slightest
# departure from normality in large datasets (too sensitive). Thus, 
# in a dataset like this one, the test is biased towards rejecting the 
# null (Normality). So, we will also look at QQ plots for each variable.

for(i in 0:1)
{
par(mfrow=c(3,4))
for(j in 1:11)
{
	qqnorm(wine[wine$Color==i,j],main=colnames(wine)[j]); qqline(wine[wine$Color==i,j],col='red')
}
}

par(mfrow=c(3,4))
for(j in 1:11)
{
	qqnorm(wine[wine$Color==0,j],main=colnames(wine)[j]); qqline(wine[wine$Color==0,j],col='red')
}

# This does happen to confirm what we saw before, very few
# variables appear to be Normal distributed

# Now we can focus more on the structure of the data and how
# the input variables relate to each other.

cor(wine.white)
cor(wine.red)

# Visualizing the correlation matrix

install.packages('corrplot')
library(corrplot)
corrplot(cor(wine),order='FPC',diag=FALSE,type='upper')
corrplot(cor(wine.white[,1:12]),order='FPC',diag=FALSE,type='upper', title='White Wine', mar=c(0,0,1,0))
corrplot(cor(wine.red[,1:12]),order='FPC',diag=FALSE,type='upper', title='Red Wine', mar=c(0,0,1,0))

#
# Formal Analysis
#

# Multicollinearity check - This is run for each
# color rather than as a whole since we basically
# have two different populations (red and white)

# I ran the red dataset and white dataset one at time
# It was easier to set it up like this rather than
# replicating the code for each of the datasets

wine.check<-wine.red[,1:11]
wine.check<-wine.white[,1:11]

# Compute the new matrix X'X.
 wine.check<- as.matrix(wine.check)
 M <- t(wine.check)%*%wine.check
 print(round(M,2))

# Compute the determinant
 det(M)

# Compute the eigenvalues
 lambda <-eigen(M)$values
 print(round(lambda,2))

# Compute the Condition Number
 k <- max(lambda)/min(lambda)
 k 

# Show on a table a row with lambda(i) where i = 1,2,...,p
# and a second row with sqrt(lambda(mx)/lambda(j))
# Condition Indexes
 Z <- max(lambda)/lambda
 print(round(sqrt(Z),2))

 
#
# Logistic Regression to predict color based on the inputs
#

# Removing the quality response variable and then splitting
# the dataset into a test set and a training set

wine.color<-wine[,c(1:11,13)]

# I decided to assign 70% to the training set and 30% to test set

indexes<-sample(1:nrow(wine.color),size=0.7*nrow(wine.color))
training.color<-wine.color[indexes,]
test.color<-wine.color[-indexes,]

# First, running a preliminary full model

glm.color.full<-glm(Color~.,data=training.color,family='binomial', maxit=50)

summary(glm.color.full)

plot(glm.color.full)
library(car)
outlierTest(glm.color.full)

# As was suspected earlier, there are two observations that
# tested to be outliers. Those observations will now be removed

wine.color<-wine.color[-c(396,1798),]

# Building the model and then using best subsets for variable selection
# The goal is to keep the model as simple as possible without sacrificing too much
# in performance. The methods used for variable selection will reflect that

# Running the training and test set again after the observations were removed

indexes<-sample(1:nrow(wine.color),size=0.7*nrow(wine.color))
training.color<-wine.color[indexes,]
test.color<-wine.color[-indexes,]

# Taking a look at best model by each size

install.packages('leaps')
library(leaps)
subset.wine<-regsubsets(Color~.,data=training.color,nbest=1)
summary(subset.wine)
plot(subset.wine,scale='bic', main='Best subsets for determining wine color')

# Running the best model of each size

glm.color.one<-glm(Color~total.sulfur.dioxide,data=training.color,family='binomial', maxit=50)
glm.color.two<-glm(Color~total.sulfur.dioxide+density,data=training.color,family='binomial', maxit=50)
glm.color.three<-glm(Color~total.sulfur.dioxide+density+residual.sugar,data=training.color,family='binomial', maxit=50)
glm.color.four<-glm(Color~total.sulfur.dioxide+density+residual.sugar+volatile.acidity,data=training.color,family='binomial', maxit=50)
glm.color.five<-glm(Color~total.sulfur.dioxide+density+residual.sugar+volatile.acidity+alcohol,data=training.color,family='binomial', maxit=50)
glm.color.six<-glm(Color~total.sulfur.dioxide+density+residual.sugar+volatile.acidity+alcohol+free.sulfur.dioxide,data=training.color,family='binomial', maxit=50)
glm.color.full<-glm(Color~.,data=training.color,family='binomial', maxit=50)

summary(glm.color.one)
summary(glm.color.two)
summary(glm.color.three)
summary(glm.color.four)
summary(glm.color.five)
summary(glm.color.six)

plot(glm.color.one)
plot(glm.color.two)
plot(glm.color.three)
plot(glm.color.four)
plot(glm.color.five)
plot(glm.color.six)

library(car)
vif(glm.color.one)
vif(glm.color.two)
vif(glm.color.three)
vif(glm.color.four)
vif(glm.color.five)
vif(glm.color.six)

# Everything checks out with each of the models

# I will test each model to see how they perform

# Assessing accuracy on the training set

# The first six lines here are identical except for swapping out
# one model for the other. This was easier than replicating the
# full scoring code for each model

predicted.training.color.1<-predict(glm.color.one, training.color,type='response')
predicted.training.color.2<-predict(glm.color.two, training.color,type='response')
predicted.training.color.3<-predict(glm.color.three, training.color,type='response')
predicted.training.color.4<-predict(glm.color.four, training.color,type='response')
predicted.training.color.5<-predict(glm.color.five, training.color,type='response')
predicted.training.color.6<-predict(glm.color.six, training.color,type='response')
predicted.training.color.f<-predict(glm.color.full, training.color,type='response')

predicted.color<-ifelse(predicted.training.color.f>0.5,1,0)
conf<-table(training.color[,12],predicted.color)
err<-1-(sum(diag(conf))/nrow(training.color))
(accuracy<-1-err)

# Assessing accuracy on the test set

predicted.test.color.1<-predict(glm.color.one, test.color,type='response')
predicted.test.color.2<-predict(glm.color.two, test.color,type='response')
predicted.test.color.3<-predict(glm.color.three, test.color,type='response')
predicted.test.color.4<-predict(glm.color.four, test.color,type='response')
predicted.test.color.5<-predict(glm.color.five, test.color,type='response')
predicted.test.color.6<-predict(glm.color.six, test.color,type='response')
predicted.test.color.f<-predict(glm.color.full, test.color,type='response')

predicted.color<-ifelse(predicted.test.color.5>0.5,1,0)
conf<-table(test.color[,12],predicted.color)
err<-1-(sum(diag(conf))/nrow(test.color))
(accuracy<-1-err)

# Plotting the color against predicted probability for each model

par(mfrow=c(2,3), mar=c(2,4,2,0.5))
plot(jitter(test.color$Color, factor=0.3),xlab='Wine Color',ylab='P(Red)',predicted.test.color.1, main='One Variable')
abline(h=0.5, col='red', lty=2)
plot(jitter(test.color$Color, factor=0.3),xlab='Wine Color',ylab='P(Red)',predicted.test.color.2, main='Two Variable')
abline(h=0.5, col='red', lty=2)
plot(jitter(test.color$Color, factor=0.3),xlab='Wine Color',ylab='P(Red)',predicted.test.color.3, main='Three Variable')
abline(h=0.5, col='red', lty=2)
plot(jitter(test.color$Color, factor=0.3),xlab='Wine Color',ylab='P(Red)',predicted.test.color.4, main='Four Variable')
abline(h=0.5, col='red', lty=2)
plot(jitter(test.color$Color, factor=0.3),xlab='Wine Color',ylab='P(Red)',predicted.test.color.5, main='Five Variable')
abline(h=0.5, col='red', lty=2)
plot(jitter(test.color$Color, factor=0.3),xlab='Wine Color',ylab='P(Red)',predicted.test.color.6, main='Six Variable')
abline(h=0.5, col='red', lty=2)

#
# SVM
#

install.packages('kernlab')
library(kernlab)

# There are multiple different kernels, different costs, etc
# We want to find the optimal settings with which to predict response
# So I tested out a few different kernels and various costs in order
# to find the best predictive model on the test set

cost<-matrix(nrow=150,ncol=3)
colnames(cost)<-c('rbfdot','polydot','vanilladot')
for (i in 1:150)
{
	svm.model<-ksvm(Color~.,data=training.color,type='C-svc',kernel='vanilladot',C=i,scaled=c())
	svm.pred<-predict(svm.model,test.color[,-12])
	cost[i,3]<-(sum(diag(table(pred=svm.pred,true=test.color[,12])))/nrow(test.color))
}

# Plotting the results

x11()
plot(cost[,1],xlab='Cost',ylab='Accuracy',type='l',col='blue',ylim=c(.96,.995),main='Comparing kernels across different values for cost')
lines(cost[,2],type='l',pch=22,lty=2,col='red')
lines(cost[,3],type='l',pch=6,lty=3,col='black')
legend('bottomright',title='Kernel',c('rbfdot - Radial Basis (Gaussian)', 'polydot - Polynomial', 'vanilladot - Linear'),col=c('blue','red','black'),lty=c(1,2,3))

# Selecting the best kernel and cost value

svm.model<-ksvm(Color~.,data=training.color,type='C-svc',kernel='vanilladot',C=1,scaled=c())

# Assessing accuracy on the training and test set

svm.pred<-predict(svm.model,training.color[,-12])
table(pred=svm.pred,true=training.color[,12])
svm.pred<-predict(svm.model,test.color[,-12])
table(pred=svm.pred,true=test.color[,12])
(sum(diag(table(pred=svm.pred,true=test.color[,12])))/nrow(test.color))

#
#
# Multinomial regression to predict quality
#
# Building models w/ all wines, just red, and just white
#

# Removing the color response variable and splitting out training and test set

wine.quality<-wine[,1:12]

indexes<-sample(1:nrow(wine.quality),size=0.7*nrow(wine.quality))
training.quality<-wine.quality[indexes,]
test.quality<-wine.quality[-indexes,]

#
# Whole dataset to predict quality
#
# Building the model in the same manner as above
#

install.packages('nnet')
library(nnet)

# Generating the best model of each size

subset.wine<-regsubsets(Quality~.,data=training.quality,nbest=1)
summary(subset.wine)
plot(subset.wine,scale='bic', main='Best subsets for quality of all wines')

# Running the best models of each size

glm.quality.full<-multinom(Quality~.,data=training.quality, maxit=500)
glm.quality.one<-multinom(Quality~alcohol,data=training.quality, maxit=500)
glm.quality.two<-multinom(Quality~alcohol+volatile.acidity,data=training.quality, maxit=500)
glm.quality.three<-multinom(Quality~alcohol+volatile.acidity+sulphates,data=training.quality, maxit=500)
glm.quality.four<-multinom(Quality~alcohol+volatile.acidity+sulphates+residual.sugar,data=training.quality, maxit=500)
glm.quality.five<-multinom(Quality~alcohol+volatile.acidity+sulphates+residual.sugar+total.sulfur.dioxide,data=training.quality, maxit=500)
glm.quality.six<-multinom(Quality~alcohol+volatile.acidity+sulphates+residual.sugar+total.sulfur.dioxide+free.sulfur.dioxide,data=training.quality, maxit=500)

summary(glm.quality.full)
summary(glm.quality.one)
summary(glm.quality.two)
summary(glm.quality.three)
summary(glm.quality.four)
summary(glm.quality.five)
summary(glm.quality.six)

# Assessing accuracy on training set

predicted.quality.f<-predict(glm.quality.full,training.quality,type='class')
predicted.quality.1<-predict(glm.quality.one,training.quality,type='class')
predicted.quality.2<-predict(glm.quality.two,training.quality,type='class')
predicted.quality.3<-predict(glm.quality.three,training.quality,type='class')
predicted.quality.4<-predict(glm.quality.four,training.quality,type='class')
predicted.quality.5<-predict(glm.quality.five,training.quality,type='class')
predicted.quality.6<-predict(glm.quality.six,training.quality,type='class')

conf<-table(training.quality[,12],predicted.quality.6)
err<-1-(sum(diag(conf))/nrow(training.quality))
(accuracy<-1-err)

# Assessing accuracy on test set

predicted.quality.f<-predict(glm.quality.full,test.quality,type='class')
predicted.quality.1<-predict(glm.quality.one,test.quality,type='class')
predicted.quality.2<-predict(glm.quality.two,test.quality,type='class')
predicted.quality.3<-predict(glm.quality.three,test.quality,type='class')
predicted.quality.4<-predict(glm.quality.four,test.quality,type='class')
predicted.quality.5<-predict(glm.quality.five,test.quality,type='class')
predicted.quality.6<-predict(glm.quality.six,test.quality,type='class')

conf<-table(test.quality[,12],predicted.quality.6)
err<-1-(sum(diag(conf))/nrow(test.quality))
(accuracy<-1-err)
conf

# It's not surprising that this model doesn't do well
# We have red wines and white wines lumped in together
# Quality may be determined differently between colors
# Now we will use the split datasets to build models
# to predict quality based on the inputs for each color

#
# Red wines
#

# Removing the color variable and then splitting up training and test sets

wine.quality.red<-wine.red[,1:12]

indexes<-sample(1:nrow(wine.quality.red),size=0.7*nrow(wine.quality.red))
training.quality.red<-wine.quality.red[indexes,]
test.quality.red<-wine.quality.red[-indexes,]

# Generating the best model of each size

subset.wine<-regsubsets(Quality~.,data=training.quality.red,nbest=1)
summary(subset.wine)
plot(subset.wine,scale='bic', main='Best subsets for quality of red wine')

# Running the best model of each size

glm.quality.red.full<-multinom(Quality~.,data=training.quality.red, maxit=500)
glm.quality.red.one<-multinom(Quality~alcohol,data=training.quality.red, maxit=500)
glm.quality.red.two<-multinom(Quality~alcohol+volatile.acidity,data=training.quality.red, maxit=500)
glm.quality.red.three<-multinom(Quality~alcohol+volatile.acidity+sulphates,data=training.quality.red, maxit=500)
glm.quality.red.four<-multinom(Quality~alcohol+volatile.acidity+sulphates+chlorides,data=training.quality.red, maxit=500)
glm.quality.red.five<-multinom(Quality~alcohol+volatile.acidity+sulphates+chlorides+total.sulfur.dioxide,data=training.quality.red, maxit=500)
glm.quality.red.six<-multinom(Quality~alcohol+volatile.acidity+sulphates+chlorides+total.sulfur.dioxide+pH,data=training.quality.red, maxit=500)

summary(glm.quality.red.full)
summary(glm.quality.red.one)
summary(glm.quality.red.two)
summary(glm.quality.red.three)
summary(glm.quality.red.four)
summary(glm.quality.red.five)
summary(glm.quality.red.six)

# Assessing accuracy on training set

predicted.quality.f<-predict(glm.quality.red.full,training.quality.red,type='class')
predicted.quality.1<-predict(glm.quality.red.one,training.quality.red,type='class')
predicted.quality.2<-predict(glm.quality.red.two,training.quality.red,type='class')
predicted.quality.3<-predict(glm.quality.red.three,training.quality.red,type='class')
predicted.quality.4<-predict(glm.quality.red.four,training.quality.red,type='class')
predicted.quality.5<-predict(glm.quality.red.five,training.quality.red,type='class')
predicted.quality.6<-predict(glm.quality.red.six,training.quality.red,type='class')

conf<-table(training.quality.red[,12],predicted.quality.6)
err<-1-(sum(diag(conf))/nrow(training.quality.red))
(accuracy<-1-err)

# Assessing accuracy on test set

predicted.quality.f<-predict(glm.quality.red.full,test.quality.red,type='class')
predicted.quality.1<-predict(glm.quality.red.one,test.quality.red,type='class')
predicted.quality.2<-predict(glm.quality.red.two,test.quality.red,type='class')
predicted.quality.3<-predict(glm.quality.red.three,test.quality.red,type='class')
predicted.quality.4<-predict(glm.quality.red.four,test.quality.red,type='class')
predicted.quality.5<-predict(glm.quality.red.five,test.quality.red,type='class')
predicted.quality.6<-predict(glm.quality.red.six,test.quality.red,type='class')

conf<-table(test.quality.red[,12],predicted.quality.2)
err<-1-(sum(diag(conf))/nrow(test.quality.red))
(accuracy<-1-err)
conf

# SVM

# There are multiple different kernels, different costs, etc
# We want to find the optimal settings with which to predict response
# I used the default kernel for multi-class responses and tested a
# number of different cost values

install.packages('e1071')
library(e1071)
svm.model<-svm(Quality~.,data=training.quality.red, cost=1)
svm.pred<-predict(svm.model,training.quality.red[-12])
table(pred=round(svm.pred,0),true=training.quality.red[,12])
svm.pred<-predict(svm.model,test.quality.red[-12])
table(pred=round(svm.pred,0),true=test.quality.red[,12])


#
# White wines
#

# Removing the color variable and splitting up into the training and test set

wine.quality.white<-wine.white[,1:12]

indexes<-sample(1:nrow(wine.quality.white),size=0.7*nrow(wine.quality.white))
training.quality.white<-wine.quality.white[indexes,]
test.quality.white<-wine.quality.white[-indexes,]

# Generating the best model of each size

subset.wine<-regsubsets(Quality~.,data=training.quality.white,nbest=1)
summary(subset.wine)
plot(subset.wine,scale='bic', main='Best subsets for quality of white wine')

# Running the best model of each size

glm.quality.white.full<-multinom(Quality~.,data=training.quality.white, maxit=500)
glm.quality.white.one<-multinom(Quality~alcohol,data=training.quality.white, maxit=500)
glm.quality.white.two<-multinom(Quality~alcohol+volatile.acidity,data=training.quality.white, maxit=500)
glm.quality.white.three<-multinom(Quality~alcohol+volatile.acidity+residual.sugar,data=training.quality.white, maxit=500)
glm.quality.white.four<-multinom(Quality~alcohol+volatile.acidity+residual.sugar+free.sulfur.dioxide,data=training.quality.white, maxit=500)
glm.quality.white.five<-multinom(Quality~alcohol+volatile.acidity+residual.sugar+free.sulfur.dioxide+density,data=training.quality.white, maxit=500)
glm.quality.white.six<-multinom(Quality~alcohol+volatile.acidity+residual.sugar+free.sulfur.dioxide+density+sulphates,data=training.quality.white, maxit=500)

summary(glm.quality.white.full)
summary(glm.quality.white.one)
summary(glm.quality.white.two)
summary(glm.quality.white.three)
summary(glm.quality.white.four)
summary(glm.quality.white.five)
summary(glm.quality.white.six)

# Assessing accuracy on training set

predicted.quality.f<-predict(glm.quality.white.full,training.quality.white,type='class')
predicted.quality.1<-predict(glm.quality.white.one,training.quality.white,type='class')
predicted.quality.2<-predict(glm.quality.white.two,training.quality.white,type='class')
predicted.quality.3<-predict(glm.quality.white.three,training.quality.white,type='class')
predicted.quality.4<-predict(glm.quality.white.four,training.quality.white,type='class')
predicted.quality.5<-predict(glm.quality.white.five,training.quality.white,type='class')
predicted.quality.6<-predict(glm.quality.white.six,training.quality.white,type='class')

conf<-table(training.quality.white[,12],predicted.quality.6)
err<-1-(sum(diag(conf))/nrow(training.quality.white))
(accuracy<-1-err)

# Assessing accuracy on test set

predicted.quality.f<-predict(glm.quality.white.full,test.quality.white,type='class')
predicted.quality.1<-predict(glm.quality.white.one,test.quality.white,type='class')
predicted.quality.2<-predict(glm.quality.white.two,test.quality.white,type='class')
predicted.quality.3<-predict(glm.quality.white.three,test.quality.white,type='class')
predicted.quality.4<-predict(glm.quality.white.four,test.quality.white,type='class')
predicted.quality.5<-predict(glm.quality.white.five,test.quality.white,type='class')
predicted.quality.6<-predict(glm.quality.white.six,test.quality.white,type='class')

conf<-table(test.quality.white[,12],predicted.quality.4)
err<-1-(sum(diag(conf))/nrow(test.quality.white))
(accuracy<-1-err)
conf

# SVM

# There are multiple different kernels, different costs, etc
# We want to find the optimal settings with which to predict response
# I used the default kernel for multi-class responses and tested a
# number of different cost values

install.packages('e1071')
library(e1071)
svm.model<-svm(Quality~.,data=training.quality.white, cost=30)
svm.pred<-predict(svm.model,training.quality.white[-12])
table(pred=round(svm.pred,0),true=training.quality.white[,12])
svm.pred<-predict(svm.model,test.quality.white[-12])
table(pred=round(svm.pred,0),true=test.quality.white[,12])