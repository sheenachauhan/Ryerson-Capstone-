#Used for quicker dataset loading due to the big datasets 
install.packages("data.table")
library(data.table)

install.packages("heuristica")

library(heuristica)

install.packages("plyr")
library(plyr)

install.packages("SDMTools")
library(SDMTools)

#data visualization 
install.packages("ggplot2")
library(ggplot2)
install.packages("pROC")
library(pROC)

#corrplot
install.packages("corrplot")
library(corrplot)

install.packages("lattice")
library(lattice)
#Cross validation and feature selection
install.packages("caret")
library(caret)
install.packages("MASS")
library(MASS)
install.packages("leaps")
library(leaps)

#Class imbalance
install.packages("ROSE")
library(ROSE)

#classifier models 

install.packages("randomForest")
library(randomForest)

#loading up datasets 
test <- fread("c:/users/gz50/Desktop/Sheena/test.csv", showProgress = T)
train <- fread("c:/users/gz50/Desktop/Sheena/train.csv", showProgress = T,nrows=18790469)

#quick look at the data 
head(train)
tail(train)
str(train)

head(train)

#checking for missing values broken down by variables  
colSums(is.na(test))
colSums(is.na(train))

#Note attribute_time having blank entries which makes sense since they did not download app (target variable). Proven below where the number matches 
colSums(train=="")
table(train$is_attributed)



#taking a look at the dataset of target variable. Noted that it is skewed (0.24% shows target attribute) 
table(train$is_attributed)

#to control randomization for future processing 
set.seed(575)

#sampling to make this datasets smaller for easier computation. Note computer limitations and crashing on R. 
#Would usually do a 70/30 split, however, original percentage differences between test and train is 90/10 split 
s.train <- train[sample(nrow(train), 25000), ]
s.test <- test[sample(nrow(test), 25000), ]

#Ensuring reduced set has similar distribution to original dataset for target variable 
table(s.train$is_attribute)

#Checking unique values for each categories 
sapply(s.train, function(x) length(unique(x)))



check_index <- sample(1:nrow(s.train), 0.7 * nrow(s.train))
TrainData.set <- s.train[check_index,]
TestData.set <- s.train[-check_index,]


#target variable. Still skewed. 0.25% shows target attribute. Similar to original dataset.
#will need to balance dataset (undersample/oversample)
table(TrainData.set$is_attributed) 

#splitting click_time into different columns for better analysis 
#removing click_time and year and month since they are the same for all 
#consider adding in seconds?
TrainData.set$click_time<-as.POSIXct(TrainData.set$click_time, format = "%Y-%m-%d %H:%M")
TrainData.set$year=year(TrainData.set$click_time)
TrainData.set$month=month(TrainData.set$click_time)
TrainData.set$days=weekdays(TrainData.set$click_time)
TrainData.set$hour=hour(TrainData.set$click_time)
table(TrainData.set$year)
table(TrainData.set$month)
TrainData.set$click_time=NULL
TrainData.set$year=NULL
TrainData.set$month=NULL

#Changed days to numeric (monday = 1, Tuesday =2, wednesday-3, thursday = 4). Remember to switch to test as well later
TrainData.set$days <- gsub("Thursday", "4", TrainData.set$days)
TrainData.set$days <- gsub("Wednesday", "3", TrainData.set$days)
TrainData.set$days <- gsub("Tuesday", "2", TrainData.set$days)
TrainData.set$days <- gsub("Monday", "1", TrainData.set$days)

table(TrainData.set$days)

#Will remove attributed_time as it is dependent on is_attributited meaning there is a vlaue if is_attributed = 1
TrainData.set$attributed_time=NULL

# Plotting graphs
Count.target<- table(TrainData.set$is_attributed)
barplot(Count.target, main= "Barplot for target attribute", xlab="traget Attribute")


#Split by target attribute 
split0 <- TrainData.set[which(TrainData.set$is_attributed == "0"),]
split1 <- TrainData.set[which(TrainData.set$is_attributed == "1"),]

#Top 10 frequency for ips (not target/target)
count.ip0 <- sort(table(split0$ip), decreasing = TRUE)[1:10]
count.ip1 <- sort(table(split1$ip), decreasing = TRUE)[1:10]

#Top 10 frequency for apps (not target/target)
count.app0 <- sort(table(split0$app), decreasing = TRUE)[1:10]
count.app1 <- sort(table(split1$app), decreasing = TRUE)[1:10]

#Top 10 frequency for devices (not target/target)
count.device0 <- sort(table(split0$device), decreasing = TRUE)[1:10]
count.device1 <- sort(table(split1$device), decreasing = TRUE)[1:10]

#Top 10 frequency for os's (not target/target)
count.os0 <- sort(table(split0$os), decreasing = TRUE)[1:10]
count.os1 <- sort(table(split1$os), decreasing = TRUE)[1:10]

#Top 10 frequency for channels (not target/target)
count.channel0 <- sort(table(split0$channel), decreasing = TRUE)[1:10]
count.channel1 <- sort(table(split1$channel), decreasing = TRUE)[1:10]

#Top frequency for days (not target/target)
count.day0 <- sort(table(split0$days), decreasing = TRUE)[1:4]
count.day1 <- sort(table(split1$days), decreasing = TRUE)[1:4]

#Top frequency for hours (not target/target)
count.hour0 <- sort(table(split0$hour), decreasing = TRUE)[1:24]
count.hour1 <- sort(table(split1$hour), decreasing = TRUE)[1:24]

#Barplots  
par(mfrow=c(2,2))
barplot(count.ip0, main="Top 10 for ips (not target)", xlab="ips", col="darkblue", las=2)
barplot(count.ip1, main="Top 10 for ips (target)", xlab="ips", col="red", las=2)
barplot(count.app0, main="Top 10 for apps (not target)", xlab="apps", col="darkblue", las=2)
barplot(count.app1, main="Top 10 for apps (target)", xlab="apps", col="red", las=2)

par(mfrow=c(2,2))
barplot(count.device0, main="Top 10 for devices (not target)", xlab="devices", col="darkblue", las=2)
barplot(count.device1, main="Top 10 for devices (target)", xlab="devices", col="red", las=2)
barplot(count.os0, main="Top 10 for os's (not target)", xlab="os's", col="darkblue", las=2)
barplot(count.os1, main="Top 10 for os's (target)", xlab="os's", col="red", las=2)

par(mfrow=c(2,2))
barplot(count.channel0, main="Top 10 for channels (not target)", xlab="channels", col="darkblue", las=2)
barplot(count.channel1, main="Top 10 for channels (target)", xlab="channels", col="red", las=2)
barplot(count.day0, main="Top days (not target)", xlab="days", col="darkblue", las=2)
barplot(count.day1, main="Top days (target)", xlab="days", col="red", las=2)

par(mfrow=c(1,1))
barplot(count.hour0, main="Top hours (not target)", xlab="hours", col="darkblue", las=2)
barplot(count.hour1, main="Top hours (target)", xlab="hours", col="red", las=2)

split0$hour <- ifelse(split0$hour >= 05 & split0$hour <= 11, "Morning",
                      ifelse(split0$hour > 11 & split0$hour <= 16, "Afternoon",
                             ifelse(split0$hour > 16 & split0$hour <= 19, "Evening", "Night")))

split1$hour <- ifelse(split1$hour >= 05 & split1$hour <= 11, "Morning",
                      ifelse(split1$hour > 11 & split1$hour <= 16, "Afternoon",
                             ifelse(split1$hour > 16 & split1$hour <= 19, "Evening", "Night")))

#Top frequency for hours for morning (5-11), afternoon (12-4), evening (5-7) and night (everything else) (not target/target)
count.hoursplit0 <- sort(table(split0$hour), decreasing = TRUE)[1:4]
count.hoursplit1 <- sort(table(split1$hour), decreasing = TRUE)[1:4]

par(mfrow=c(1,1))
barplot(count.hoursplit0, main="Top hours (not target)", xlab="Time of Day", col="darkblue", las=2)
barplot(count.hoursplit1, main="Top hours (target)", xlab="Time of Day", col="red", las=2)

#Correlation 
#Changing factor to numeric for correlation. Will need to change back to target attribute back to factor later.
TrainData.set$is_attributed <- as.numeric(as.character(TrainData.set$is_attributed))
TrainData.set$days <- as.numeric(as.character(TrainData.set$days))
#corelation , note negative weak correlation for channel and app and for days and hour
corrplot(cor(TrainData.set, method="spearman"), method="number")

#STEP 3: FEATURE SELECTION  
#Forward 
full <- lm(is_attributed~., data=TrainData.set)
null <- lm(is_attributed~1, data=TrainData.set)
forward <- stepAIC(null,scope=list(lower=null, upper=full), direction ="forward", trace=TRUE)
summary(forward)
#Results shows ip, app, channel, os should be selected only 

#Target variable back to factor 
TrainData.set$is_attributed  = factor(TrainData.set$is_attributed)

#Removal of variables for modelling as determined by feature selection
reducedvar.set <- TrainData.set[,c(-3,-7,-8)]

#STEP 4: Classification Algorithms and STEP 5: Evaluation of Models 

#To split given training set to training/testing sets for modelling. Seed set for future replication
set.seed(738)
check_index <- sample(1:nrow(reducedvar.set ), 0.7 * nrow(reducedvar.set ))
train.set <- reducedvar.set[check_index,]
test.set <- reducedvar.set [-check_index,]

#Target variable. Still skewed. 0.0025% shows target attribute. Closely similar to original dataset.
#49/17451 = 0.0028%
table(train.set$is_attributed) 

#Cross Validation (repeatedcv)
train_control<- trainControl(method="repeatedcv", number=5, repeats =3, summaryFunction=twoClassSummary, classProbs=T, savePredictions = T)
train_controlAcc<- trainControl(method="repeatedcv", number=5, repeats =3, savePredictions = T)

#To fix Classprobs error for names 
levels(train.set$is_attributed) <- c("No", "Target")
levels(test.set$is_attributed) <- c("No", "Target")

#seed set constant for all training of models for future evaluations
#RandomForest via Caret (No balancing) 
set.seed(450)
NB_RF.trainROC = train(is_attributed ~ ., data = Train.set, trControl = train_control, method = "rf", metric="ROC")
NB_RF.trainROC
NB_RF.predictROC=predict(NB_RF.trainROC, test.set, type="prob")
roc(test.set$is_attributed, NB_RF.predictROC$Target)$auc

set.seed(450) 
NB_RF.trainAcc = train(is_attributed ~ ., data = Train.set, trControl = train_controlAcc, method = "rf")
NB_RF.trainAcc
NB_RF.predictAcc <- predict(NB_RF.trainAcc, test.set)
confusionMatrixFor_Neg1_0_1(NB_RF.predictAcc, test.set$is_attributed, mode= "everything", positive = "Target")

#XGboost via Caret (No balancing) 
set.seed(450)
NB_XGB.trainROC = train(is_attributed ~ ., data = Train.set, trControl = train_control, method = "xgbTree", metric="ROC")
NB_XGB.trainROC
NB_XGB.predictROC=predict(NB_XGB.trainROC, test.set, type="prob")
roc(test.set$is_attributed, NB_XGB.predictROC$Target)$auc

set.seed(450)
NB_XGB.trainAcc = train(is_attributed ~ ., data = train.set, trControl = train_controlAcc, method = "xgbTree")
NB_XGB.trainAcc
NB_XGB.predictAcc <- predict(NB_XGB.trainAcc, test.set)
confusionMatrixFor_Neg1_0_1(NB_XGB.predictAcc, test.set$is_attributed, mode = "everything", positive = "Target")

#RandomForest via Caret (SMOTE)
set.seed(450)
train_control$sampling = "smote"
SMOTE_RF.trainROC = train(is_attributed ~ ., data = train.set, trControl = train_control, method = "rf", metric="ROC")
SMOTE_RF.trainROC
SMOTE_RF.predictROC=predict(SMOTE_RF.trainROC, test.set, type="prob")
roc(test.set$is_attributed, SMOTE_RF.predictROC$Target)$auc

set.seed(450)
train_controlAcc$sampling = "smote"
SMOTE_RF.trainAcc = train(is_attributed ~ ., data = train.set, trControl = train_controlAcc, method = "rf")
SMOTE_RF.trainAcc
SMOTE_RF.predictAcc <- predict(SMOTE_RF.trainAcc, test.set)
confusionMatrix(SMOTE_RF.predictAcc, test.set$is_attributed, mode = "everything", positive = "Target")

#XGboost via Caret (SMOTE) 
set.seed(450)
train_control$sampling = "smote"
SMOTE_XGB.trainROC = train(is_attributed ~ ., data = train.set, trControl = train_control, method = "xgbTree", metric="ROC")
SMOTE_XGB.trainROC
SMOTE_XGB.predictROC=predict(SMOTE_XGB.trainROC, test.set, type="prob")
roc(test.set$is_attributed, SMOTE_XGB.predictROC$Target)$auc

set.seed(450)
train_controlAcc$sampling = "smote"
SMOTE_XGB.trainAcc = train(is_attributed ~ ., data = train.set, trControl = train_controlAcc, method = "xgbTree")
SMOTE_XGB.trainAcc
SMOTE_XGB.predictAcc <- predict(SMOTE_XGB.trainAcc, test.set)
confusionMatrix(SMOTE_XGB.predictAcc, test.set$is_attributed, mode = "everything", positive = "Target")

#RandomForest via Caret (Over)
set.seed(450)
train_control$sampling = "up"
UP_RF.trainROC = train(is_attributed ~ ., data = train.set, trControl = train_control, method = "rf", metric="ROC")
UP_RF.trainROC
UP_RF.predictROC=predict(UP_RF.trainROC, test.set, type="prob")
roc(test.set$is_attributed, UP_RF.predictROC$Target)$auc

set.seed(450)
train_controlAcc$sampling = "up"
UP_RF.trainAcc = train(is_attributed ~ ., data = train.set, trControl = train_controlAcc, method = "rf")
UP_RF.trainAcc
UP_RF.predictAcc <- predict(UP_RF.trainAcc, test.set)
confusionMatrix(UP_RF.predictAcc, test.set$is_attributed, mode = "everything", positive = "Target")

#XGboost via Caret (Over) 
set.seed(450)
train_control$sampling = "up"
UP_XGB.trainROC = train(is_attributed ~ ., data = train.set, trControl = train_control, method = "xgbTree", metric="ROC")
UP_XGB.trainROC
UP_XGB.predictROC=predict(UP_XGB.trainROC, test.set, type="prob")
roc(test.set$is_attributed, UP_XGB.predictROC$Target)$auc

set.seed(450)
train_controlAcc$sampling = "up"
UP_XGB.trainAcc = train(is_attributed ~ ., data = train.set, trControl = train_controlAcc, method = "xgbTree")
UP_XGB.trainAcc
UP_XGB.predictAcc <- predict(UP_XGB.trainAcc, test.set)
confusionMatrix(UP_XGB.predictAcc, test.set$is_attributed, mode = "everything", positive = "Target")

#Model comparision using resamples and diff - ROC
resamps.ROC <- resamples(list(RF_NB = NB_RF.trainROC, XGB_NB = NB_XGB.trainROC,
                              RF_SMOTE = SMOTE_RF.trainROC, XBG_SMOTE = SMOTE_XGB.trainROC,
                              RF_UP = UP_RF.trainROC, XGB_UP = UP_XGB.trainROC))
summary(resamps.ROC)
difs.ROC <- diff(resamps.ROC)
difs.ROC
summary(difs.ROC)

#Model comparision using resamples and diff - ACC
resamps.ACC <- resamples(list(RF_NB = NB_RF.trainAcc, XGB_NB = NB_XGB.trainAcc,
                              RF_SMOTE = SMOTE_RF.trainAcc, XBG_SMOTE = SMOTE_XGB.trainAcc,
                              RF_UP = UP_RF.trainAcc, XGB_UP = UP_XGB.trainAcc))
summary(resamps.ACC)
difs.ACC <- diff(resamps.ACC)
difs.ACC
summary(difs.ACC)
