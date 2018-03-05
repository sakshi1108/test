library("dplyr")
library("ROSE")
library("ggplot2")
library("caret")

setwd("C:/Users/Yogesh/Desktop/Desktop Backup/Churn Modelling") #set your working directory by replacing folder location
churndata <- read.csv("churn.csv",sep = ",",header = TRUE,na.strings = c(NA,"")) #command to import dataset
prop.table(table(churndata$Churn)) #checking distribution of churners and non churners


# Exploratory data analysis
glimpse(churndata)
churndata$Area.Code<-as.factor(churndata$Area.Code)
churndata$Churn <- as.factor(churndata$Churn)

ggplot(churndata,aes(x=Churn,y=CustServ.Calls))+geom_bar(stat="Identity")

# check missing value in dataset 
colSums(is.na(churndata))


# dataprepration 

index<-createDataPartition(y = churndata$Churn,times = 1,p = 0.7,list = FALSE)
traindata <- churndata[index,]
testdata <- churndata[-index,]


prop.table(table(traindata$Churn))
prop.table(table(testdata$Churn))


# AS class imbalance is there create copy of over, under and both over and undersample

traindata_over<-ovun.sample(Churn~.,data = traindata,method = "over")$data
table(traindata_over$Churn)

traindata_under<-ovun.sample(Churn~.,data = traindata ,method = "under")$data
table(traindata_under$Churn)

traindata_both <- ovun.sample(Churn~.,data=traindata,method="both")$data
table(traindata_both$Churn)

traindata_rose <- ROSE(Churn ~ ., data = traindata,  seed=111)$data
table(traindata_rose$Churn)

# apply logistic regression model on differrnt samples
over_classifier = glm(formula = Churn ~. -State -Area.Code, family = binomial, data = traindata_over)
under_classifier = glm(formula = Churn ~ ., family = binomial, data = traindata_under)
both_classifier = glm(formula = Churn ~ ., family = binomial, data = traindata_both)
rose_classifier = glm(formula = Churn ~ ., family = binomial, data = traindata_rose)
imb_classifier = glm(formula = Churn ~ ., family = binomial, data = traindata_imb)

#test above model on testdata

# Prediction on test set using sampling classifiers

# Predicting the test set using Over sampling classifier
over_probability_predict = predict(over_classifier, type = 'response', newdata = test_set[-31])
y_pred_over = ifelse(over_probability_predict>0.5, 1, 0)

# Predicting the test set using Under sampling classifier
under_probability_predict = predict(under_classifier, type = 'response', newdata = test_set[-31])
y_pred_under = ifelse(under_probability_predict>0.5, 1, 0)

# Predicting the test set using Mixed sampling classifier
both_probability_predict = predict(both_classifier, type = 'response', newdata = test_set[-31])
y_pred_both = ifelse(both_probability_predict>0.5, 1, 0)

# Predicting the test set using ROSE classifier
rose_probability_predict = predict(rose_classifier, type = 'response', newdata = test_set[-31])
y_pred_rose = ifelse(rose_probability_predict>0.5, 1, 0)

# Predicting the test set using SMOTE classifier
smote_probability_predict = predict(smote_classifier, type = 'response', newdata = test_set[-31])
y_pred_smote = ifelse(smote_probability_predict>0.5, 1, 0)


# ROC curve of over sampling data
roc_over <- roc.curve(test_set$Class, y_pred_over)
print(roc_over)
# ROC curve of Under sampling data
roc_under <- roc.curve(test_set$Class, y_pred_under)
print(roc_under)
# ROC curve of both sampling data
roc_both <- roc.curve(test_set$Class, y_pred_both)
print(roc_both)
# ROC curve of ROSE sampling data
roc_rose <- roc.curve(test_set$Class, y_pred_rose)
print(roc_rose)
# ROC curve of SMOTE sampling data
roc_smote <- roc.curve(test_set$Class, y_pred_smote)
print(roc_smote)

