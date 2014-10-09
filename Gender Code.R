#
#
# Capstone Project
# Gender Measurements
# Group 1
# Ben, Derek, and Subrina
#
#

#   source('Gender.R')

# Change directory to where these data are

setwd('F:/RIT Year 2/Spring/Capstone/')

#Read in these data

gender <- read.table("GenderMeas1.txt", header=T, quote="\"")


# 1(a)
# Convert variables

gender.in<-apply(gender[,c(1:5, 10:21, 24)],2, function(cm.in) cm.in*.393701)
Wgt<-gender$Wgt*2.20462
gender.avg.in<-apply(gender[,c(6:9)],2, function(avgin) (avgin*.393701)/2)

gender.conv.temp<-cbind(gender.in,Wgt,gender.avg.in,gender[,c(22,25:27)])
head(gender.conv.temp)

# Reorder these data

gender.conv<-gender.conv.temp[,c(1:5,20:23, 6:17,24,19,18,25:27)]

# 1(b)
# Round necessary decimals and print first 6 observations

gender.round<-round(gender.conv[1:6,c(1:21,23:24)],2)
gender.other<-gender.conv[1:6,c(22,25:27)]
rounded.values<-cbind(gender.round,gender.other)
print(rounded.values)


# 2
# Find the overall mean values and %CV

summary.stats<-matrix(nrow=24,ncol=4)
colnames(summary.stats)<-c('Male_Mean', 'Male_%CV', 'Female_Mean', 'Female_%CV')
row.names(summary.stats)<-colnames(gender.conv[,1:24])

for(j in 1:24)
{
summary.stats[j,1]<-mean(gender.conv[gender.conv$Gend==1,][,j])
summary.stats[j,2]<-100*(sd(gender.conv[gender.conv$Gend==1,][,j])/mean(gender.conv[gender.conv$Gend==1,][,j]))
summary.stats[j,3]<-mean(gender.conv[gender.conv$Gend==0,][,j])
summary.stats[j,4]<-100*(sd(gender.conv[gender.conv$Gend==0,][,j])/mean(gender.conv[gender.conv$Gend==0,][,j]))
}
round(summary.stats,1)

# 3
# This is where we want to split the dataset into a training set and test set

training.set<-gender.conv[gender.conv$train==1,]
test.set<-gender.conv[gender.conv$train==0,]

# Then generating visualizations
# We decided on using boxplots to visualize the distribution of each variable by gender

par(mfrow=c(4,6),mar=c(2,2,2,.2))
for(j in 1:24)
{
boxplot(training.set[,j]~factor(Gend),names=c("Female", "Male"),col=c("pink", "blue"),data=training.set,main=colnames(training.set)[j])
}

# An additional plot in Appendix A showing the distribution of variables by gender

install.packages('rattle')
library(rattle)
rattle()

# Generating a correlation plot to see relationships among variables

install.packages('corrplot')
library(corrplot)
corrplot(cor(gender.conv[1:24]),type='upper')

# 4
# Build a model using the training set

# First, test for normality
# We need to split each variable up by Gender to test for normality

normality.check<-matrix(nrow=24,ncol=2)
colnames(normality.check)<-c('Female P-Val','Male P-Val')
row.names(normality.check)<-colnames(training.set[,1:24])
for(j in 1:24)
{
	normality.check[j,1]<-shapiro.test(training.set[training.set$Gend==0,j])$p
	normality.check[j,2]<-shapiro.test(training.set[training.set$Gend==1,j])$p
}
round(normality.check,4)

# We failed to reject the null hypothesis of Normality if p-val>0.1 for both genders
# We determined that there was significant evidence against Normality in all but 9 variables

# Running QQ plots for each variable as a double-check
# For the sake of deciphering plots, needed to break into 2 plots

for(i in 0:1)
{
x11()
par(mfrow=c(3,4))
for(j in 1:12)
{
	qqnorm(training.set[training.set$Gend==i,j],main=colnames(training.set)[j]); qqline(training.set[training.set$Gend==i,j],col='red')
}
x11()
par(mfrow=c(3,4))
for(j in 13:24)
{
	qqnorm(training.set[training.set$Gend==i,j],main=colnames(training.set)[j]); qqline(training.set[training.set$Gend==i,j],col='red')
}
}

# This backs up what we saw in the Shapiro-Wilk Normality tests
# Running LDA for just those 9 variables

library(MASS)
training.set$Gend<-as.factor(training.set$Gend)
lda.train<-lda(Gend~Biac+Bitr+Elb_D+Wrst_D+Shld_G+Frrm_G+Clf_GM+Wrst_Gm+Hgt,training.set)

# 5(a)
# Assessing the performance of the model on the training set

predicted.gend.train<-predict(lda.train, training.set[,c(1,3,6,7,10,17,19,21,24)],type='response')$class
conf<-table(training.set[,25],predicted.gend.train)
err<-1-(sum(diag(conf))/nrow(training.set))
(accuracy<-1-err)

# This returns an accuracy of 98.2% (or an error rate of just 1.8%)
# We were able to reduce the model to just 5 terms
# while not effecting the model's performance

cor(training.set[,c(1,3,6,7,10,17,19,21,24)])

lda.train<-lda(Gend~Biac+Bitr+Elb_D+Frrm_G+Clf_GM,training.set)

# Scoring this reduced model

predicted.gend.train<-predict(lda.train, training.set[,c(1,3,6,17,19)],type='response')$class
conf<-table(training.set[,25],predicted.gend.train)
err<-1-(sum(diag(conf))/nrow(training.set))
(accuracy<-1-err)

# This returns an accuracy of 97.0% (or an error rate of just 3.0%)


# 5(b)
# Graphs showing the effect of model on training set

plot(lda.train)

palette(c("blue", "pink"))
plot(predict(lda.train)$x, col=training.set$Gend, ylab="LD Scores", main="Discriminant Analysis Predictions", xlab="ID number")

# 6
# Assessing the quality of the model on the test set

predicted.gend.test<-predict(lda.train, test.set[,c(1,3,6,17,19)],type='response')$class

#Send results to text file

train.info<-cbind(training.set[,26],training.set[,27],as.numeric(as.vector(predicted.gend.train)))
colnames(train.info)<-c("Train", "ID", "Gender Prediction")
test.info<-cbind(test.set[,26], test.set[,27], as.numeric(as.vector(predicted.gend.test)))
colnames(test.info)<-c("Train", "ID", "Gender Prediction")
output<-merge(train.info, test.info, all=TRUE)
output.sort<-output[order(output$ID),]
write.table(output.sort, '/Volumes/PUBLIC/RIT Year 2/Spring/Capstone/GenderMeas_Pred.txt', col.names=FALSE, row.names=FALSE, sep=" ")

# 7
# Additional Visualization

plot(training.set[,c(1,3,6,17,19)],col=ifelse(training.set$Gend=='0','pink','blue'))