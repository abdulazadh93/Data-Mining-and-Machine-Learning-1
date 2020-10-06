library(knitr)
library(tidyverse)
library(ggplot2)
library(mice)
library(lattice)
library(reshape2)
library(DataExplorer)
library(ROSE)
library(dummies)
library(caret)

rawdata <- read.csv("C:/Users/abdul/Desktop/SEM 1 Modules/ML1/Project/Dataset/FINAL datasets/Credit card/default of credit card clients (2).csv",header=T,stringsAsFactors = T)
recoverydata = rawdata
head(recoverydata)

head(rawdata)
str(rawdata)

###REMOVING 1st ROW
colnames(rawdata) <- as.character(unlist(rawdata[1,]))
rawdata = rawdata[-1, ]

head(rawdata)
summary(rawdata)

#################################################################
#replacing the dependent variable name
colnames(rawdata)[colnames(rawdata)=="default payment next month"] <- "Result"
head(rawdata)
str(rawdata)


#####DATA IS READY TO WORK ON
#converting factors to numeric
rawdata[, 1:25] <- sapply(rawdata[, 1:25], as.numeric)
str(rawdata)

#Correlation plot
library(corrplot)
M <- cor(rawdata)
corrplot(M,method='color')

####deleting the in significant independent variables
datanew_1 <- select(rawdata, -one_of('ID','AGE','EDUCATION', 'BILL_AMT1','BILL_AMT2','BILL_AMT3','BILL_AMT4','BILL_AMT5','BILL_AMT6'))
head(datanew_1)
str(datanew_1)

#####################################
#Scale transformation 
datanew_1[, 1:15] <- scale(datanew_1[, 1:15])
head(datanew_1)
str(datanew_1)


#converting result values asfactor 
datanew_1$Result = as.factor(datanew_1$Result)
str(datanew_1)
#head(datanew_1)

####Splitting data
set.seed(1300)
smp_size <- floor(0.75 * nrow(datanew_1))
split_data <- sample(seq_len(nrow(datanew_1)), size = smp_size)
train_1 = datanew_1[split_data,]
test_1 = datanew_1[-split_data,]

dim(train_1)
dim(test_1)

str(train_1)
str(test_1)
test_2= test_1

###########Training model with training data

logisticModel<-glm(Result~.,family=binomial(link="logit"), data = train_1)
summary(logisticModel)


########################################################################################

predicted_values <- predict(logisticModel, type = "response", newdata = test_2)
test_2$predicted_values <- predicted_values         ## adding predicting valus as a column in test data
test_2$prediction <- test_2$predicted_values > 0.5                  
test_2$prediction <- as.numeric(test_2$prediction)                
test_2$prediction <- as.factor(test_2$prediction)

str(test_2)
caret::confusionMatrix(test_2$prediction, test_2$Result)


######ROC curve
install.packages('ROCR')
library(ROCR)

ROCRpred <- prediction(test_2$prediction,test_2$Result)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))

##AUC 

library(ModelMetrics)
auc(test_2$prediction, test_2$Result)


##############################################################################################################

################ KNN ###########################################################

###importing the raw data 
KNNrawdata = recoverydata
str(KNNrawdata)

###deleting the 1st row
colnames(KNNrawdata) <- as.character(unlist(KNNrawdata[1,]))
dataknn = KNNrawdata[-1, ]
head(dataknn)

###Dependent variable name change
colnames(rawdata)[colnames(rawdata)=="default payment next month"] <- "Result"


##converting the independent variables into numeric
dataknn[, 1:24] <- sapply(dataknn[, 1:24], as.numeric)
str(dataknn)

##Conversion of catergorical variables into factors
dataknn$SEX=as.factor(dataknn$SEX)
dataknn$EDUCATION=as.factor(dataknn$EDUCATION)
dataknn$MARRIAGE=as.factor(dataknn$MARRIAGE)

####Normalizing the data
n <-sapply(dataknn,function(x){is.numeric(x)})
numerics<-dataknn[,n]

normalize <- function(x) {return((x - min(x))/(max(x) - min(x)))}
numericNormal <- normalize(numerics)

KNNData <- dataknn[,!n]
normalizedDataKNN <- cbind(KNNData,numericNormal)

tkNN <- dummy.data.frame(normalizedDataKNN[,-4])
str(tkNN)

normalizedData <- cbind(tkNN,normalizedDataKNN$Result)
str(normalizedData)

###Over sampling the unbalanced data
dataknn$Result=as.factor(dataknn$Result)
overknn <- ovun.sample(Result~.,data = normalizedDataKNN,method = "over")$data 
prop.table(table(overknn$Result)) * 100

#splitting data
set.seed(100)
id  <- sample(2,nrow(overknn),prob= c(0.75,0.25),replace=T)
kNNTraining <-overknn[id==1,]
kNNTesting <-overknn[id==2,]
Train_labels <- overknn[id==1,4]
Test_labels <- overknn[id==2,4]


########Training the model
control <- caret::trainControl(method="repeatedcv", repeats=3)
metric <- "Accuracy"

knnmodel <- caret::train(Result ~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + AGE + PAY_0 + PAY_2 + PAY_3 + BILL_AMT1 + PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + PAY_AMT4,data=kNNTraining,
                         method="knn", metric=metric, trControl=control,preProcess = c("center","scale"),tuneLength = 10)

###############Prediction
knnprediction <- predict(knnmodel, kNNTesting)

##Model Prediction
caret::confusionMatrix(knnprediction, Test_labels)

###ROC curve
ROCRpred_1 <- prediction(as.numeric(Test_labels),as.numeric(knnprediction))
ROCRperf_1 <- performance(ROCRpred_1, 'tpr','fpr')
plot(ROCRperf_1, colorize = TRUE, text.adj = c(-0.2,1.7))

### AUC ########################
auc(Test_labels,knnprediction)

#####ROC Curve combined##################################################
plot(ROCRperf, main="AUC ROC Curve",col ="red")
plot(ROCRperf_1, add = TRUE, col ="blue")

legend("bottomright", c( "Logistic Regression ROC curve","KNN ROC curve"), lty=5, 
       col = c("red", "blue"), bty="n", inset=c(0,0.15))
legend("bottomright", c( "Logistic Regression - AUC = 0.76 ","KNN - AUC = 0.74"), lty=8, 
       col = c("red", "blue"), bty="n", inset=c(0,0.50))

