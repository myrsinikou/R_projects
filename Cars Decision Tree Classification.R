library(caTools)
library(rpart)
library(rpart.plot)
library(rattle)
library(tidyverse)

#Importing and viewing data
View(cars)
str(cars)

#Changing brand into factor
cars$brand<-as.factor(cars$brand)
class(cars$brand)
cars<-cars[,-7]

#Descriptive Statistics, checking for NAs and outliers
summary(cars)
View(cars%>%filter(!complete.cases(.)))
cars$cubicinches[is.na(cars$cubicinches)]<-mean(cars$cubicinches,na.rm=TRUE)
cars$weightlbs[is.na(cars$weightlbs)]<-mean(cars$weightlbs,na.rm=TRUE)
summary(cars)
par(mfrow=c(2,3))
par(mar=c(2,2,1,1))
boxplot(cars[1],show.names=TRUE)
boxplot(cars[2],show.names=TRUE)
boxplot(cars[3],show.names=TRUE)
boxplot(cars[4],show.names=TRUE)
boxplot(cars[5],show.names=TRUE)
boxplot(cars[6],show.names=TRUE)

#Removing outliers
cars_new<-cars
dim(cars_new)
quartiles<-quantile(cars_new$time.to.60,probs = c(0.25,0.75),na.rm = FALSE)
IQR<-IQR(cars_new$time.to.60)
lower<-quartiles[1]-1.5*IQR
upper<-quartiles[2]+1.5*IQR
cars_new<-subset(cars_new,cars_new$time.to.60>lower & cars_new$time.to.60<upper)
dim(cars_new)
View(cars_new)

#Splitting to train and test set
set.seed(10)
sample<-sample.split(cars_new$brand,SplitRatio = 0.7)
train<-subset(cars_new,sample==TRUE)
test<-subset(cars_new,sample==FALSE)
print(dim(train))
print(dim(test))

#Verifying the randomization process
prop.table(table(train$brand))
prop.table(table(test$brand))

#Building the decision tree model
brand_tree<-rpart(brand~.,data=train,method = "class")
summary(brand_tree)
par(mfrow=c(1,1))
fancyRpartPlot(brand_tree)

#Predicting the brand
predict_brand<-predict(brand_tree,newdata=test,type="class")
table_mat<-table(test$brand,predict_brand)
table_mat

#Checking accuracy
acc_test<-sum(diag(table_mat))/sum(table_mat)
print(acc_test)

#Tuning Hyperparameters
accuracy_tune <- function(brand_tree) {
  predict_brand <- predict(brand_tree, newdata=test, type = 'class')
  table_mat<-table(test$brand,predict_brand)
  print(table_mat)
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  accuracy_Test
}
control <- rpart.control(minsplit = 10,
                         minbucket = round(10 / 3),
                         maxdepth = 4,
                         cp = 0)
brand_fit <- rpart(brand~., data = train, method = 'class', control = control)
fancyRpartPlot(brand_fit)
accuracy_tune(brand_fit)


