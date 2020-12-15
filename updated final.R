#******************** IMPORTING DATASET ***************************************#

setwd("C:\\Users\\Yogesh\\Desktop\\Desktop Backup\\Churn Modelling") #set your working directory by replacing folder location
churndata <- read.csv("churn.csv",sep = ",",header = TRUE,na.strings = c(NA,"")) #command to import dataset
prop.table(table(churndata$Churn)) #check percentage of churners and non churners in dataset

str(churndata)#checking structure of our dataset


#************* Preparing dataset ************# 

churndata$Churn<-factor(churndata$Churn,levels = c("1","0")) #converting target variable to factor

#converting categorical data into binary #
churndata$Int.l.Plan_ohe<-NA #one-hor encoding
churndata$Int.l.Plan_ohe[which(churndata$Int.l.Plan=="yes")]=1
churndata$Int.l.Plan_ohe[which(is.na(churndata$Int.l.Plan_ohe))]=0
# validating transformation
table(churndata$Int.l.Plan)
table(churndata$Int.l.Plan_ohe)
churndata$Int.l.Plan<-NULL #removing categorical variable

churndata$VMail.Plan_ohe <-NA
churndata$VMail.Plan_ohe[which(churndata$VMail.Plan=="yes")]=1
churndata$VMail.Plan_ohe[which(is.na(churndata$VMail.Plan_ohe))]=0
#validating transformation
table(churndata$VMail.Plan)
table(churndata$VMail.Plan_ohe)
churndata$VMail.Plan<-NULL

row.names(churndata)<-churndata$Phone
churndata$Phone<-NULL


#*******************PART 1 : Descriptive Stats *****************************#

#ploting distribution of all variable 
hist(churndata$Account.Length,col = "red",main = "Account length")
# hist(log(churndata$VMail.Message),col = "red",main = "No of Voice Mail Messages",breaks = 10) ***
hist(churndata$Day.Mins,col = "red", main ="Totalno Day Mins")
hist(churndata$Day.Calls,col= "red" ,main= "Total No of Day Calls")# hist(churndata$Day.Charge,col= "red" ,main= "Total Day Charge")
# hist(sqrt(churndata$Intl.Calls)) ***
hist(churndata$Intl.Charge)
plot(as.factor(churndata$Int.l.Plan_ohe) ,col="red",main="International Plan Subscribed ")
plot(as.factor(churndata$VMail.Plan_ohe),col="red",main="Voice Mail Plan")
plot(churndata$Churn,col="red",main="No of churners and Non Churners")
legend("bottomright",c("1 = Churned","0 = Not churned"),cex = 0.4)


#plot1
#Hypotheses1:more number of call to customer care higher chances of churning
count<-table(churndata$Churn,churndata$CustServ.Calls)
prop<-prop.table(table(churndata$Churn,churndata$CustServ.Calls),margin = 2)
par(mar=c(5.1, 4.1, 4.1, 7.1), xpd=TRUE)
barplot(prop, col=heat.colors(length(rownames(prop))), width=2,main="call to customer care v/s churning")
legend("topright",inset=c(-0.25,0), fill=heat.colors(length(rownames(prop))),legend = rownames(count))

#plot2
# hypotheses 2: subscribing International calling plan increases churn
count1<-table(churndata$Churn,churndata$Int.l.Plan)
prop1<-prop.table(table(churndata$Churn,churndata$Int.l.Plan),margin = 2)
barplot(prop1,col=heat.colors(length(rownames(prop1))),width=2,main="Int'l call v/s churning rate")
legend("topright",inset=c(-0.25,0),fill=heat.colors(length(rownames(prop1))),legend=rownames(count1))

#plot3 
# hypotheses 3: Customers with the VoiceMail Plan tend to churn less frequently.
count2<-table(churndata$Churn,as.factor(churndata$VMail.Plan_ohe))
prop2<-prop.table(table(churndata$Churn,as.factor(churndata$VMail.Plan_ohe)),margin = 2)
barplot(prop2,col=heat.colors(length(rownames(prop2))),width = 2,main="Voice Mail Plan v/s churning rate")
legend("topright",inset=c(-0.25,0),fill=heat.colors(length(rownames(prop2))),legend=c("yes","no"))

churndata$CustServ.Calls<-as.numeric(churndata$CustServ.Calls) #again convert variable to its original form

summary(churndata) # lets check is there any missing value in dataset


#***************************************PART 2 : DATA PARTITIONING *********************************************#

#removing not required variables

churndata = churndata[,! names(churndata) %in% c("State", "Area.Code") ] # 

# creating training and testdata set using caret packge
library("caret")
index<-createDataPartition(y = churndata$Churn,times = 1,p = 0.7,list = FALSE)


set.seed(2) # set seed to get the same sample 
ind = sample(2, nrow(churndata), replace = TRUE, prob=c(0.7, 0.3))
trainset = churndata[ind == 1,]
testset = churndata[ind == 2,]

# checking proportion of churn in both the data set (appox. 15 % churner 85 % non churners)
prop.table(table(trainset$Churn))
prop.table(table(testset$Churn))

# checking dimension of both the dataset
dim(trainset)
dim(testset)


#************************************ PART 3 : MODEL BUILDING ************************************************#

# MODEL 1 "DECISION TREE"                                                                                     #
# MODEL 2 "Logistic Regression"                                                                               #
# MODEL 3 "SVM"                                                                                               #
# MODEL 4 "RANDOM FORREST"                                                                                    #
# MODEL 5 "NEURAL NETWORK"                                                                                    
# MODEL 5 "CLTV ANALYSIS" #


#*************************************************************************************************************#
# MODEL 1 : DECISON TREE #
#************************#


# install.packages("rpart")          
 library("rpart") # load package

# using rpart function to build a classification model
# FIRST WE ARE DEALING WITH FULLY GROWN TREE THEN WE WILL PRUNE THE TREE AND PREDICT WITH OPTIMIZED TREE

churn.rp.full<-rpart(Churn~.,data = trainset,control = rpart.control(cp=0)) # It will generate fully grown tree
print(churn.rp.full) # retriving node details of decision tree
printcp(churn.rp.full) # examining complexity parameter to avoid overfitting of data
plotcp(churn.rp.full) # It is the other way to visualize above table and we can see that min.xvalidation error is occuuring when tree size is 12

# summary(churn.rp)

# Visualizing Tree
plot(churn.rp.full,margin=0.1,uniform=TRUE, branch=0.6)
text(churn.rp.full,all=TRUE,use.n = TRUE)

#measuring prediction performance of tree model on train dataset
library("caret")
pred.f.t<-predict(churn.rp.full,testset,type="class")
#confusionMatrix(table(pred.f.t,trainset$Churn))

# measuring prediction performance of tree model on test dataset

conf.matrix.f.tree<-confusionMatrix(table(pred.f.t, testset$Churn),positive = "1") 
conf.matrix.f.tree

# #****************************code for ploting ROC Curve of FUll tree ************************#
# pred.f.t<-predict(churn.rp.full,testset,type="prob")                                       #
# predscore.f.t<-prediction(pred.f.t[,2],testset$Churn)                                      #
# perf.f.t<-performance(predscore.f.t,"fpr","tpr")                                           #
# plot(perf.f.t,col="blue",lwd=1,main="ROC : Classification of churner and non churner")     #
# abline(0, 1, lty = 8, col = "grey")                                                        #


# To avoid overfitting we want to prune our decision tree and remove those variable which are not helping much in prediction we will prune tree on the basis of cross complexity parameter, first set a seed to make sure the results are reproducible as mentioned in the video, because you will be examining cross-validated error results. 

min(churn.rp.full$cptable[,"xerror"]) # find minimum crossvalidation error of tree
which.min(churn.rp.full$cptable[,"xerror"]) # locate record with minimum crossvalidation error
churn.cp = churn.rp.full$cptable[8,"CP"] #Get the cost complexity parameter of the record with the minimum cross-validation errors
churn.cp
prune.tree = prune(churn.rp.full, cp= churn.cp) # prune the tree by setting min cross validation error

# visualize prune tree
plot(prune.tree, margin= 0.1,uniform=TRUE,branch=0.6)
text(prune.tree, all=TRUE , use.n=TRUE)

#lets check the performance of pruned tree
pred.p.t<-predict(prune.tree,testset,type = "class")
conf.matrix.p.t<-confusionMatrix(table(pred.p.t,testset$Churn),positive = "1") # if we check our specificity has been improved


library("ROCR")
#****************************code for ploting ROC Curve of prune tree ***********************#
 # pred.p.t<-predict(prune.tree,testset,type="prob")                                          #
 # predscore.p.t<-prediction(pred.p.t[,2],testset$Churn)                                      #
 # perf.p.t<-performance(predscore.p.t,"fpr","tpr")                                           #
 # plot(perf.p.t,col="blue",lwd=1,main="ROC : Pruned Decision Tree")                          #
 # abline(0, 1, lty = 8, col = "grey")                                                        #

# summary(prune.tree)

# Comparing and plotting ROC for both trees
plot(perf.f.t,col="blue",lwd=1,main="ROC : FUll V/S Pruned Tree")
plot(perf.p.t,col="red",lwd=1,add=TRUE)
abline(0, 1, lty = 8, col = "grey")
legend("bottomright", c("Full Tree","Pruned Tree"), cex=0.8,
       col=c("blue","red"),lty=1:2)



# model build by decision tree is stored in prune.tree 


#********** Model 2 Logistic Regression (with log transfromation of variable V.mail.message)********#

# logistic regresssion after transforming few variables #
trainsetlr<-trainset # creating copy of trainset and testset
testsetlr<-testset
trainsetlr$VMail.Message<-log(trainsetlr$VMail.Message+1)
testsetlr$VMail.Message<-log(testsetlr$VMail.Message+1)

churn.lr<-glm(Churn~.,data=trainsetlr,family = binomial)
summary(churn.lr)
pred.lr1<- predict(churn.lr,testsetlr,type="response")
class = pred.lr1 > .5
summary(class)

churn.mod = ifelse(testsetlr$Churn == "1", 1, 0)
pred_class = churn.mod
pred_class[pred.lr1<=.5] = 1- pred_class[pred.lr1<=.5]
ctb = table(churn.mod, pred_class)
ctb
confusionMatrix(ctb,positive = "1")

# Part 2 using only statistically significant variables as per previous run
lr.fit<-glm(Churn~ Int.l.Plan_ohe + Intl.Calls+CustServ.Calls,data = trainsetlr,family = binomial)# building model using selected features
summary(lr.fit)
pred.lr = predict(lr.fit,testsetlr, type="response")

class=pred.lr>.5
summary(class)
# sum(class)
# tb=table(testset$Churn,class)
# tb

# CONFUSION MATRIX FOR LOGISTIC REGRESSION #
churn.mod = ifelse(testsetlr$Churn == "1", 1, 0)
pred_class = churn.mod
pred_class[pred.lr<=.5] = 1- pred_class[pred.lr<=.5]
ctb = table(churn.mod, pred_class)
ctb
confusionMatrix(ctb,positive = "1")

# #plotting ROC for Logistic Regression#
predscore.lr1 <- prediction(pred.lr1,testsetlr$Churn)
perf.lr1 <- performance(predscore.lr1,"fpr","tpr")
predscore.lr <-prediction(pred.lr,testsetlr$Churn)
perf.lr <- performance(predscore.lr,"fpr","tpr")
plot(perf.lr1,main="ROC:Logistic Regression",col="green",lwd=1)
plot(perf.lr ,col="red",add=TRUE)
abline(0, 1, lty = 8, col = "grey")  
legend("bottomright", c("LR with all var","LR with few var"), cex=0.8,
       col=c("green","red"),lty=1:2)

#***** MODEL "churn.lr" is working fine after log transformation of variable V.Mail ****# 


#************************ Model 3 Support Vector Machine ****************************#

# Since it is one of the black box approach of modelling it is less interpretable than other we can use it to show and compare its performance with other model

# install.packages("e1071")
library("e1071")

churn.svm = svm(Churn~., data = trainset, kernel="radial", cost=1, gamma = 1/ncol(trainset))
summary(churn.svm)

#predicting target variables using model we created from trainset
pred.svm = predict(churn.svm,testset[,!names(testset) %in% c("Churn")]) #predicting model performance on testdataset
svm.table= table(pred.svm,testset$Churn) #comparing predicted value with actual
svm.table
confusionMatrix(svm.table)

# Plotting ROC Curve for SVM , we have to train svm by setting prob=True 
churn.svm = svm(Churn~.,data=trainset,kernel="radial",cost=1,gamma = 1/ncol(trainset),prob=TRUE)
pred.svm = predict(churn.svm,testset[,!names(testset) %in% c("Churn")],probability = TRUE) #predicting model performance on testdataset
pred.prob = attr(pred.svm, "probabilities") # obtaining probablities for 0 and 1
pred.to.roc = pred.prob[, 2] # extracting probablities for 1
pred.rocr = prediction(pred.to.roc, testset$Churn) #using probablites for prediction
perf.rocr = performance(pred.rocr, measure = "auc", x.measure = "cutoff") 
perf.tpr.rocr = performance(pred.rocr, "tpr","fpr")
plot(perf.tpr.rocr, colorize=T,main=paste("AUC:",(perf.rocr@y.values)))
abline(0, 1, lty = 8, col = "grey")


# Tuning SVM using e1071 package by adjusting cost and gamma value we will try to identify best fit of svm using tune function from svm which performs grid search on available solutions#

tuned = tune.svm(Churn~.,data = trainset,gamma=10^(-6:-1), cost = 10^(1:2))
summary(tuned) # we can check best gamma = 0.01 and cost = 100 (It may be different in your case) by using this we will run model again

churn.svm.tuned<-svm(Churn~., data = trainset, gamma = tuned$best.parameters$gamma, cost = tuned$best.parameters$cost,prob=TRUE)
summary(churn.svm.tuned)
pred.svm.tuned = predict(churn.svm.tuned, testset[, !names(testset) %in% c("churn")],probability = TRUE)
table.svm.tuned = table(pred.svm.tuned, testset$Churn)
table.svm.tuned
confusionMatrix(table.svm.tuned)

# there is very slight impovement in result

#****** MODEL "churn.svm.tuned" is giving best result ******#

#*********************** Model 4 Ensemble Models (Random Forrest) ***********************************#
# install.packages("randomForest")
library("randomForest")

#fitting random forest classsifier to training dataset
churn.rf<-randomForest(Churn~.,data = trainset,importance=T,mtry=6)
churn.rf
#predicting testdata on fitted model
pred.rf<-predict(churn.rf,testset)
table(pred.rf,testset$Churn)

#list important variable
importance(churn.rf)

#we can also plot variable importance plot
varImpPlot(churn.rf)

# confusion matrix for random forest

conf.matrix.rf<-confusionMatrix(pred.rf,testset$Churn)
conf.matrix.rf

#ploting ROC curve for random forest

pred.rf.prob<-predict(churn.rf,testset,type = "prob") # getting prbablity of 0 and 1 prediction for each row in testdataset
pred.score.rf<-prediction(pred.rf.prob[,2],testset$Churn)
perf.rf <-performance(pred.score.rf,"fpr","tpr")                                           #
plot(perf.rf,col="blue",lwd=1,main="ROC : Random Forest") 
abline(0, 1, lty = 8, col = "grey")


#*******     Tuning Random Forest by extending caret package     *****#
# there are no. of techniques for tuning random forest , ntree and mtry (no of column to try) are two important parameter for considertaion while tuning random forest. There are different ways to tune RF like i) writing your own code for finding optimal combination of both parameters ii) using e1071 package iii) using caret package iv) using tune( function from random forest package, we will find mtry by using tune() from )
# 
# mtry : number of variables selected at each split - default = sqrt(no of variables) for classification
# ntree : number of trees to grow: default = 500
# nodesize : minimum size of terminal nodes default = 1

control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
set.seed(12345)
tunegrid <- expand.grid(.mtry=c(1:8))
rf_gridsearch <- train(Churn~., data=trainset, method="rf", tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch) # we can check best value of accuracy is coming when mtry = 6
# so we will develop our model with mtry = 6 value is already  used in above code.




#**************************** Model 5 Neural Network *********************************************#

# install.packages("nnet")
library("nnet")
trainsetnn<-trainsetlr # using numeric data only 
testsetnn<-testsetlr
# 
names<-c(16:18)
trainsetnn[,-c(16,17,18)]<-scale(trainsetnn[,-c(16,17,18)])
testsetnn[,-c(16,17,18)]<-scale(testsetnn[,-c(16,17,18)])

trainsetnn[,names]<- lapply(trainsetnn[,names], factor)
testsetnn[,names] <- lapply(testsetnn[,names],factor)
str(trainsetnn)
str(testsetnn)
# trainsetnn$Churn<-as.numeric(as.character(trainsetnn$Churn))
# testsetnn$Churn<-as.numeric(as.character(testsetnn$Churn))
# trainsetnn$Churn<-as.factor(trainsetnn$Churn)
# testsetnn$Churn<-as.factor(testsetnn$Churn)

churn.nn <- nnet(Churn~.,data=trainsetnn,size=9,rang=0.1,decay =5e-4,maxit=300) # as a rule of thumb hidden layer size should be 2/3rd of total no. of variables in our case size = 12

nn.predict<-predict(churn.nn,testsetnn[,-16],type="class")
nn.table=table(testsetnn$Churn,nn.predict)
nn.table
confusionMatrix(nn.predict,testsetnn$Churn,positive = "1")
  
tunednn<-tune.nnet(Churn~.,data = trainsetnn,size = c(9:14),decay = 5e-4)
tunednn$best.parameters # 9 is the best parameter size we are getting replace it in above code and get the optimized result
 

#****************************** PART 4 : MODEL COMPARISION ***************************************#

plot(perf.rf,col="blue",lwd=1,main="Model Comparision") 
plot(perf.tpr.rocr, col="green",add=TRUE)
plot(perf.lr,col="red",add=TRUE)
plot(perf.p.t,col="orange",lwd=1,add=TRUE)
abline(0, 1, lty = 8, col = "grey")
legend("bottomright", c("Random Forest","SVM","Logistic","Decision Tree","Neural Networks"), cex=0.8,
       col=c("blue","green","red","orange"),lty=1:2)


#**************************** PART 5 SEGMENTING CUSTOMER PROFILE ***********************************#



# different approaches can be followed for customer segmentation into high profile and low profile 
# for example using CLTV value we can cluster customers into high value and low value , using RFM analysis we can cluseter, based on some managerial decisions we can cluster and using data mining techniques we can cluster



# CLTV calculation #
# Assumptions i) given data contains details of one year transaction
#            ii) charge unit is dollars
#           iii) assuming company is getting 5% of margin from day charges 10% of margin from evening and 15% and 20% from night and international charges respectively
#           iv)  assuming monthly churn rate of industry is 4%

churndata$netprofit <- (0.05*churndata$Day.Charge+0.10*churndata$Eve.Charge+0.15*churndata$Night.Charge+0.20*churndata$Intl.Charge)
churnrate = 0.04 
churndata$cltv<-(churndata$netprofit-.5*churndata$CustServ.Calls)/churnrate
summary(churndata$cltv)
hist(churndata$cltv,breaks=100)
boxplot(churndata$cltv)
mean(churndata$cltv)

churndata$segment<-"NA"
churndata$segment[which(churndata$cltv < mean(churndata$cltv))]="LVC" 
churndata$segment[which(churndata$segment=="NA")]="HVC"
prop.table(table(churndata$segment,churndata$Churn)) # we can see out of 14% churners 8 % of churners are high value customers, according to our segmentation criterion

# subsetting high value customers
hvcustomers <- churndata[which(churndata$segment=="HVC"),]

# predicting hvc likely to churn using previously developed models

# random forest is giving best result we will use it to predict voluntary churners in our dataset

hvcustomers$prob <- predict(churn.rf,hvcustomers[,-c(16,19:21)],type="prob")

finalprediction<-predict(churn.rf,hvcustomers[,-c(16,19:21)])
table(finalprediction) # we can see out of 1726 high value customers 275 are likely to churn by extracting them companies can design retention policy for individual or group

# added some comments

# added some more comments
