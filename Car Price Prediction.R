library(tidyverse)
library(eeptools)
library(tidyr)
library(caTools)
library(car)
library(corrplot)

#Importing and Viewing Dataset
View(Car_sales)
str(Car_sales)

#Changing Vehicle_type into factor and Latest_Launch into Date
Car_sales[,5]<-lapply(Car_sales[,5],factor)
Car_sales$Latest_Launch<-as.Date(Car_sales$Latest_Launch,"%m/%d/%Y")
class(Car_sales$Latest_Launch)

#Creating age column
date_today<-Sys.Date()
format(date_today,format="%m/%d/%Y")
age<-age_calc(Car_sales$Latest_Launch,date_today,units = "years")
Car_sales<-cbind(Car_sales,age)

#Exploring Data
summary(Car_sales)
View(Car_sales%>%filter(!complete.cases(.)))

#Replacing Missing values
for (i in c(4,6,7,8,9,10,11,12,13,14,16,17)) {
  Car_sales[,i][is.na(Car_sales[,i])]<-median(Car_sales[,i],na.rm=T)
}
summary(Car_sales)

#Correlation matrix
correl_mat<-cor(Car_sales[,c(3,4,6,7,8,9,10,11,12,13,14,16,17)])
View(correl_mat)
corr<-round(correl_mat,2)

#Normality
hist(Car_sales$Price_in_thousands,col="blue")

#Linearity
plot(Car_sales$Price_in_thousands~Car_sales$Engine_size,data=Car_sales)
plot(Car_sales$Price_in_thousands~Car_sales$Horsepower,data=Car_sales)
plot(Car_sales$Price_in_thousands~Car_sales$Power_perf_factor,data=Car_sales)
plot(Car_sales$Price_in_thousands~Car_sales$X__year_resale_value,data=Car_sales)

#Spliting into train and test set
set.seed(100)
spl<-sample.split(Car_sales$Price_in_thousands,SplitRatio = 0.7)
train<-subset(Car_sales,spl==TRUE)
test<-subset(Car_sales,spl==FALSE)
print(dim(train))
print(dim(test))

#Model 1
sales1<-lm(Price_in_thousands~Sales_in_thousands+Wheelbase+Width+Length+Curb_weight+Fuel_capacity+Fuel_efficiency+age+Horsepower+Engine_size+X__year_resale_value+Power_perf_factor,data=train)
summary(sales1)
vif1<-vif(sales1)
View(vif1)
pred_value1<-predict(sales1,newdata = test)
pred_table1<-cbind(test$Price_in_thousands,pred_value1)
View(pred_table1)

#Plot of observed and predicted values
plot(pred_value1,test$Price_in_thousands,xlab="Predicted Values",ylab="Observed Values")
abline(a=0,b=1,col="red",lwd=3)

#Evaluation with RMSE
sqrt(mean(test$Price_in_thousands-pred_value1)^2)

#Model 2
sales2<-lm(Price_in_thousands~Curb_weight+Fuel_efficiency+Horsepower+Engine_size+X__year_resale_value+Power_perf_factor,data=train)
summary(sales2)
vif2<-vif(sales2)
View(vif2)
pred_value2<-predict(sales2,newdata = test)
pred_table2<-cbind(test$Price_in_thousands,pred_value2)
View(pred_table2)

#Plot of observed and predicted values
plot(pred_value2,test$Price_in_thousands,xlab="Predicted Values",ylab="Observed Values")
abline(a=0,b=1,col="red",lwd=3)

#Evaluation with RMSE
sqrt(mean(test$Price_in_thousands-pred_value2)^2)

#Model 3
sales3<-lm(Price_in_thousands~Curb_weight+Horsepower+Engine_size+X__year_resale_value+Power_perf_factor,data=train)
summary(sales3)
vif3<-vif(sales3)
View(vif3)
pred_value3<-predict(sales3,newdata = test)
pred_table3<-cbind(test$Price_in_thousands,pred_value3)
View(pred_table3)

#Plot of observed and predicted values
plot(pred_value3,test$Price_in_thousands,xlab="Predicted Values",ylab="Observed Values")
abline(a=0,b=1,col="red",lwd=3)

#Evaluation with RMSE
sqrt(mean(test$Price_in_thousands-pred_value3)^2)

#Model 4
sales4<-lm(Price_in_thousands~Horsepower+Engine_size+X__year_resale_value+Power_perf_factor,data=train)
summary(sales4)
vif4<-vif(sales4)
View(vif4)
pred_value4<-predict(sales4,newdata = test)
pred_table4<-cbind(test$Price_in_thousands,pred_value4)
View(pred_table4)

#Plot of observed and predicted values
plot(pred_value4,test$Price_in_thousands,xlab="Predicted Values",ylab="Observed Values")
abline(a=0,b=1,col="red",lwd=3)

#Evaluation with RMSE
sqrt(mean(test$Price_in_thousands-pred_value4)^2)