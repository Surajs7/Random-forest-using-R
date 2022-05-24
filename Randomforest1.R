#Random forest
mydata <- read.csv(choose.files())
head(mydata)
dim(mydata)
str(mydata)
table(mydata$price_range)
library(dplyr)
newprice <- mydata %>%
  select(price_range) %>%
  mutate(low=as.numeric(price_range<1),
         medium=as.numeric(price_range<2 & price_range>1),
        high=as.numeric(price_range<3 & price_range>=2),
        very_high=as.numeric(price_range>=3))

#Missing values
colSums(is.na(mydata))
table(mydata$price_range)
#Outlier
boxplot(mydata$battery_power)
boxplot(mydata$blue)
boxplot(mydata$clock_speed)
boxplot(mydata$dual_sim)
boxplot(mydata$fc)
boxplot(mydata$four_g)
boxplot(mydata$int_memory)
boxplot(mydata$m_dep)
boxplot(mydata$mobile_wt)
boxplot(mydata$n_cores)
boxplot(mydata$pc)
boxplot(mydata$px_height)
boxplot(mydata$px_width)
#Outlier treatment
Q1<-1.000
Q2<-7.000
IQR<-Q3-Q1
#Split the project into train and test
library(caTools)
table(split)
training <- subset(mydata,split==TRUE)
test <- subset(mydata,split==FALSE)
print(table(split))
print(nrow(training))
print(nrow(test))
#Model building
install.packages("randomforest")

rf_pred<-predict(rf,newdata=test)
rf_pred<-ifelse(rf_pred>=0.6,1,0)
cm<-table(test$price_range,rf_pred)
library(caret)
confusionMatrix(cm)
#It is random forest model, hence no need to split the data
training <- subset(mydata,split==T)
test <- subset(mydata,split==F)
#Building randomForest model
library(randomForest)
rf<-randomForest(price_range ~.,data=training)
rf
#Predict the model
y_pred <- predict(rf,newdata=mydata)
y_pred
max(y_pred)
min(y_pred)
#Give threshold value for price range
#0=low,1=medium,2=high,3=vert high
#Less than 1=low, less than 2 and 1 more than 1=medium
#2 to less than 3=high, 3 or more than 3=very high-predicted
library(dplyr)
new_price <- y_pred_cbind %>%
select(y_pred) %>%  
mutate(low=as.numeric(y_pred<1),
       medium=as.numeric(y_pred),
       high=as.numeric(y_pred>=2 & y_pred<3),
       very_high=as.numeric(y_pred>=3))
head(new_price)
MAE <- mean((mydata$price_range-new_price)^2)
MAE
y_pred_cbind1 <- 
#What is my answer
dim(y_pred_)