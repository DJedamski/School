#
# Final Project for Regression II
# 

# Reading data in and checking for multicollinearity

hockey<-read.csv('Hockey.csv',header=T)
plot(hockey) 							
hockey1<-hockey[,2:9]
cor(hockey1)

# Running the linear model

lm.hockey<-lm(P~.,data=hockey)
print(summary(lm.hockey))
residuals(lm.hockey)
par(mfrow=c(2,2))
plot(lm.hockey)

# Performing the eigenanalysis

source('unitlength.R')
B<-as.matrix(hockey1)
Y<-unitlength(B)
M<-t(Y)%*%Y
print(M)
det(M)
lambda<-eigen(M)$values
print(lambda)

# Conditioning Numbers

VK<-sqrt(max(lambda)/lambda)
print(lambda)
print(VK)

# VIF

install.packages('car')
library(car)
vif.hockey<-vif(lm.hockey)
print(sqrt(vif.hockey))

# Using BIC for model selection

lm.hockey.bic<-step(lm.hockey,P~.,data=hockey,family='binomial',direction='both',trace=0,k=log(nrow(hockey)))
summary(lm.hockey.bic)
par(mfrow=c(2,2))
plot(lm.hockey.bic)

# Using AIC for model selection

lm.hockey.aic<-step(lm.hockey,P~.,data=hockey,family='binomial',direction='both',trace=0,k=log(2))
summary(lm.hockey.aic)
par(mfrow=c(2,2))
plot(lm.hockey.aic)

# Finding the best models of each size

install.packages('leaps')
library(leaps)
subset.hockey1<-regsubsets(P~.,data=hockey,nbest=2)
summary(subset.hockey1)
plot(subset.hockey1,scale='bic')

# Logistic Regression/Accuracy

hockey.new<-read.csv('Hockey2.csv',header=T)
glm.hockey<-glm(Playoffs~.,data=hockey.new,family='binomial')
glm.hockey.f<-step(glm.hockey,Playoffs~.,data=hockey.new,family='binomial',direction='both',trace=0,k=log(2))
summary(glm.hockey)
prob.playoffs<-predict(glm.hockey.f,hockey.new[,-1],type='response')
predicted.playoffs<-(prob.playoffs>0.5)
conf.glm.playoffs<-table(hockey.new$Playoffs,predicted.playoffs)
conf.glm.playoffs
accuracy.glm.playoffs<-sum(diag(conf.glm.playoffs))/nrow(hockey.new)
accuracy.glm.playoffs

# Finding the best model of each size

install.packages('leaps')
library(leaps)
subset.hockey<-regsubsets(Playoffs~.,data=hockey.new,nbest=2)
summary(subset.hockey)
plot(subset.hockey,scale='bic')

# Boxplots for all statistics

par(mfrow=c(3,3))
boxplot(PK.~Playoffs,data=hockey.new, main='Penalty Kill Percentage')
boxplot(Shot.Diff~Playoffs,data=hockey.new, main='Shot Differential')
boxplot(FEN.CLOSE~Playoffs,data=hockey.new, main='Fenwick Close')
boxplot(PP.~Playoffs,data=hockey.new, main='Power Play Percentage')
boxplot(PDO~Playoffs,data=hockey.new, main='PDO')
boxplot(X5.5.F.A~Playoffs,data=hockey.new, main='5-on-5 Goal Ratio')
boxplot(FO.~Playoffs,data=hockey.new, main='Faceoff Percentage')
boxplot(PPO~Playoffs,data=hockey.new, main='Power Play Opportunities')

# Boxplots for playoff teams for significant variables

par(mfrow=c(2,3))
boxplot(PK.~Playoffs,data=hockey.new, main='Penalty Kill Percentage')
boxplot(Shot.Diff~Playoffs,data=hockey.new, main='Shot Differential')
boxplot(FEN.CLOSE~Playoffs,data=hockey.new, main='Fenwick Close')
boxplot(PP.~Playoffs,data=hockey.new, main='Power Play Percentage')
boxplot(PDO~Playoffs,data=hockey.new, main='PDO')



