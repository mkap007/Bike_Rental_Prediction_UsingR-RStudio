setwd("C:/Users/mkap0/OneDrive/Desktop/Desk - 2.0/Symbiosis/Apllication/Intern/Webriy/R")
getwd()

#Packages

x = c("ggplot2", "corrgram", "ggcorrplot", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", "sampling", "DataCombine", "inTrees", "ranger", "rpart.plot")
install.packages(x)
lapply(x, require, character.only = TRUE)
rm(x)


#### Data Import & Preprocess

bike_df<-read.csv("C:/Users/mkap0/OneDrive/Desktop/Desk - 2.0/Symbiosis/Apllication/Intern/Webriy/1657875746_day.csv")
head(bike_df,5)

bike_df<-subset(bike_df,select=-c(casual,registered))
head(bike_df,5)


dim(bike_df)
summary(bike_df)
str(bike_df)

#Sorting Column Names
names(bike_df)<-c('rec_id','datetime','season','year','month','holiday','weekday','workingday','weather_condition','temp','atemp','humidity','windspeed','total_count')
head(bike_df,5)


#Type Casting
bike_df$datetime<- as.Date(bike_df$datetime)
bike_df$year<-as.factor(bike_df$year)
bike_df$month<-as.factor(bike_df$month)
bike_df$season <- as.factor(bike_df$season)
bike_df$holiday<- as.factor(bike_df$holiday)
bike_df$weekday<- as.factor(bike_df$weekday)
bike_df$workingday<- as.factor(bike_df$workingday)
bike_df$weather_condition<- as.factor(bike_df$weather_condition)


#Finding Missing Value
missing_val<-data.frame(apply(bike_df,2,function(x){sum(is.na(x))}))
names(missing_val)[1]='missing_val'
missing_val


##Distribution Plots

#Seasonal monthly distribution 
ggplot(bike_df, aes(x = factor(month), y = total_count, fill = season)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set3") + 
  theme_gray() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        legend.position = "top") +
  labs(x = 'Month',
       y = 'Total Count',
       fill = 'Season',
       title = 'Season Wise Monthly Distribution of Counts')


#Weekday monthly distribution
ggplot(bike_df, aes(x = factor(month), y = total_count, fill = weekday)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_brewer(palette = "Spectral") + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        legend.position = "top") + 
  labs(x = 'Month',
       y = 'Total Count',
       fill = 'Weekday',
       title = 'Weekday Wise Monthly Distribution of Counts')


#Yearly distribution
ggplot(bike_df, aes(x = factor(year), y = total_count, fill = factor(year))) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_brewer(palette = "Dark2") + 
  theme_light() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        legend.position = "top") + 
  labs(x = 'Year',
       y = 'Total Count',
       fill = 'Year',
       title = 'Yearly Distribution of Counts')


#Holiday distribution
ggplot(bike_df, aes(x = factor(holiday), y = total_count, fill = season)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_brewer(palette = "Paired") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5), 
        legend.position = "top") + 
  labs(x = 'Holiday',
       y = 'Total Count',
       fill = 'Season',
       title = 'Distribution of Counts by Holiday')


#Working day distribution

ggplot(bike_df, aes(x = factor(workingday), y = total_count, fill = season)) +
  geom_col(position = "dodge") + 
  scale_fill_brewer(palette = "Set2") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(hjust = 0.5),
        legend.position = "top") + 
  labs(x = 'Working Day',
       y = 'Total Count',
       fill = 'Season',
       title = 'Distribution of Counts by Working Day')


#weather condition distribution
ggplot(bike_df, aes(x = weather_condition, y = total_count, fill = season)) +
  geom_col() +
  scale_fill_brewer(palette = "Set1") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(hjust = 0.5)) + 
  labs(x = 'Weather Condition',
       y = 'Total Count',
       fill = 'Season',
       title = 'Distribution of Counts by Weather Condition')


# Create a box plot with bike_df$total_count as the y-axis
ggplot(bike_df, aes(y = total_count)) + geom_boxplot(fill = "green", outlier.color = "red", outlier.size = 3) +
  labs(title = "Total_count", y = "Count") +
  
  theme_minimal()


#Temp_WindSpeed_Humidity Distribution

# reshape the data to a long format
bike_long <- reshape2::melt(bike_df, id.vars = "datetime", measure.vars = c("temp", "humidity", "windspeed"))

ggplot(bike_long, aes(x = datetime, y = value, fill = variable)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("red", "green", "blue")) + 
  theme_bw() + 
  labs(x = "Date", y = "Value", fill = "Variable", 
       title = "Box plots for temp, humidity, and windspeed",
       subtitle = "Outliers are removed") 

head(bike_df,5)



## create a normal probability plot for total_count

ggplot(mapping = aes(sample = bike_df$total_count)) +
  stat_qq(size = 2, color = "red") + 
  stat_qq_line(color = "blue") + 
  theme_bw() + 
  labs(x = "Theoretical Quantiles", y = "Total Count",
       title = "Normal Probability Plot for Total Count")




####Correlation-Matrix


library(ggcorrplot)

# Calculate the correlation matrix
correlation_matrix <- cor(bike_df[,10:14])

#Correlation plot
ggcorrplot(correlation_matrix, 
           hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("#6D9EC1", "white", "#E46726"), 
           title="Correlation Plot") +
  theme(plot.title = element_text(hjust = 0.5))






###Split the dataset into train and test dataset

library(purrr)
train_index<-sample(1:nrow(bike_df),0.7*nrow(bike_df))
train_data<-bike_df[train_index,]
test_data<-bike_df[-train_index,]
dim(train_data)
dim(test_data)




##Train and Test data

#Read the train and test data
head(train_data,5)
head(test_data,5)



#Selecting the required independent and dependent variables

train<-subset(train_data,select=c('season','year','month','holiday', 'weekday','workingday','weather_condition','temp','humidity','windspeed','total_count'))
test<-subset(test_data,select=c('season','year','month','holiday','weekday','workingday','weather_condition','temp','humidity','windspeed','total_count'))
head(train,5)
head(test,5)




#Train and test categorical and numerical attributes

train_cat_attributes<-subset(train,select=c('season','holiday','workingday','weather_condition','year'))
test_cat_attributes<-subset(test,select=c('season','holiday','workingday','weather_condition','year'))
train_num_attributes<-subset(train,select=c('weekday','month','temp','humidity','windspeed','total_count'))
test_num_attributes<-subset(test,select=c('weekday','month','temp', 'humidity','windspeed','total_count'))




##Encoding the categorical features

#Train_encoded_attributes

library(caret)
othervars<-c('month','weekday','temp','humidity','windspeed','total_count')
set.seed(2626)
#Categorical variables
vars<-setdiff(colnames(train),c(train$total_count,othervars))
f <- paste('~', paste(vars, collapse = ' + '))
encoder<-dummyVars(as.formula(f), train)
encode_attributes<-predict(encoder,train)
train_encoded_attributes<-cbind(train_num_attributes,encode_attributes)
head(train_encoded_attributes,5)


#Test_encoded_attributes

set.seed(5662)
vars<-setdiff(colnames(test),c(test$total_count,othervars))
f<- paste('~',paste(vars,collapse='+'))
encoder<-dummyVars(as.formula(f),test)
encode_attributes<-predict(encoder,test)
test_encoded_attributes<-cbind(test_num_attributes,encode_attributes)
head(test_encoded_attributes,5)




####Modelling the training dataset


##Linear Regression Model


set.seed(672)
lr_model<-lm(train_encoded_attributes$total_count~.,train_encoded_attributes[,-c(6)])
summary(lr_model)



#Cross validation prediction

options(warn=-1)
set.seed(623)
train.control<-trainControl(method='CV',number=3)
CV_predict<-train(total_count~.,data=train_encoded_attributes,method='lm',trControl=train.control)
summary(CV_predict)



# Cross validation prediction plot

residuals <- resid(CV_predict)
y_train <- train_encoded_attributes$total_count

plot(y_train, residuals, 
     ylab = 'Residuals', 
     xlab = 'Observed', 
     main = 'Cross Validation Prediction Plot',
     pch = 19,
     col = rgb(0.2, 0.4, 0.6, alpha = 0.4))
abline(h = 0, col = "red", lwd = 2)
grid()
par(mar = c(5.1, 4.1, 4.1, 2.1))



#Model performance on test data

set.seed(6872)
options(warn=-1)
lm_predict<- predict(lr_model,test_encoded_attributes[,-c(6)])
head(lm_predict,5)



#Root mean squared error and mean absolute error

set.seed(688)
rmse<-RMSE(lm_predict, test_encoded_attributes$total_count)
print(rmse)
mae<-MAE(lm_predict, test_encoded_attributes$total_count)
print(mae)


# Residual plot

y_test <- test_encoded_attributes$total_count
residuals <- y_test - lm_predict

plot(y_test, residuals, 
     xlab = 'Observed', 
     ylab = 'Residuals', 
     main = 'Residual Plot',
     pch = 19,
     col = rgb(0.5, 0.1, 0.5, alpha = 0.4))
abline(h = 0, col = "green", lwd = 2)
grid()
par(mar = c(4, 3, 2, 1))







##Decision Tree Regressor


set.seed(568)
library(rpart)
rpart.control<-rpart.control(minbucket = 2,cp = 0.01,maxcompete = 3, maxsurrogate = 4, usesurrogate = 2, xval = 3,surrogatestyle = 0, maxdepth = 10) 
dtr<-rpart(train_encoded_attributes$total_count~.,data=train_encoded_attributes[,-c(6)],control=rpart.control,method='anova',cp=0.01)
dtr



library(rpart.plot)
# Plot the learned decision tree model
rpart.plot(dtr, 
           box.palette = "RdYlGn", 
           shadow.col = "gray", 
           nn = TRUE,
           roundint = FALSE,
           fallen.leaves = TRUE, 
           main = "Decision Tree Model",
           extra = 101, 
           tweak = 1.5)



#Cross validation prediction

options(warn=-1)
set.seed(5769)
train.control<-trainControl(method='CV',number=3)
dtr_CV_predict<-train(total_count~.,data=train_encoded_attributes,method='rpart',trControl=train.control)
dtr_CV_predict


# Cross validation prediction plot
residuals <- resid(dtr_CV_predict)

plot(y_train, residuals, 
     xlab = 'Observed', 
     ylab = 'Residuals', 
     main = 'Cross Validation Plot',
     pch = 19, 
     col = rgb(0.3, 0.2, 0.6, alpha = 0.4)) 
abline(h = 0, col = "red", lwd = 2)
grid()
par(mar = c(5.1, 4.1, 4.1, 2.1))


#Model performance on the test dataset

set.seed(7882)
dtr_predict<-predict(dtr,test_encoded_attributes[,-c(6)])
head(dtr_predict,5)


#Root mean squared error and mean absolute error

set.seed(6889)
rmse<-RMSE(y_test,dtr_predict)
print(rmse)
mae<-MAE(y_test,dtr_predict)
print(mae)


# Residual plot
residuals <- y_test - dtr_predict

plot(y_test, residuals, 
     xlab = 'Observed', 
     ylab = 'Residuals', 
     main = 'Residual Plot',
     pch = 19,
     col = rgb(0.5, 0.1, 0.5, alpha = 0.4))
abline(h = 0, col = "green", lwd = 2)
grid()
par(mar = c(5.1, 4.1, 4.1, 2.1))






##Random Forest

set.seed(6788271)
library(randomForest)
rf_model<-randomForest(total_count~.,train_encoded_attributes,importance=TRUE,ntree=200)
rf_model



#Cross validation prediction for Random Forest
options(warn=-1)
set.seed(6772)
library(randomForest)
library(ranger)
train.control<-trainControl(method='CV',number=3)
rf_CV_predict<-train(total_count~.,train_encoded_attributes,method='ranger',trControl=train.control)
rf_CV_predict



# Cross validation prediction plot
residuals <- resid(rf_CV_predict)

plot(y_train, residuals, 
     xlab = 'Observed', 
     ylab = 'Residuals', 
     main = 'Cross Validation Prediction Plot',
     pch = 19,
     col = rgb(0.2, 0.6, 0.4, alpha = 0.4))

abline(h = 0, col = "red", lwd = 2)
grid()
par(mar = c(5.1, 4.1, 4.1, 2.1))



#Model performance on the test dataset

set.seed(7889)
rf_predict<-predict(rf_model,test_encoded_attributes[,-c(6)])
head(rf_predict,5)


#Root mean squared error and mean absolute error

set.seed(667)
rmse<-RMSE(y_test,rf_predict)
print(rmse)
mae<-MAE(y_test,rf_predict)
print(mae)


# Residual plot
residuals <- y_test - rf_predict

plot(y_test, residuals, 
     xlab = 'Observed', 
     ylab = 'Residuals', 
     main = 'Residual Plot',
     pch = 20,
     col = rgb(0.9, 0.4, 0.1, alpha = 0.4))
abline(h = 0, col = "blue", lwd = 2)
grid()
par(mar = c(5.1, 4.1, 4.1, 2.1))







####Final model for predicting the bike rental count on daily basis

Bike_predictions=data.frame(y_test,rf_predict)
write.csv(Bike_predictions,'Bike_Renting_R.CSV',row.names=F)
Bike_predictions
