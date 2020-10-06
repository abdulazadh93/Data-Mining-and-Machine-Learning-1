library(e1071)
library(predict)
library(stats)

SVMdata <- read.csv("C:/Users/abdul/Desktop/SEM 1 Modules/ML1/Project/Dataset/FINAL datasets/Bank Marketing/bank-additional-full.csv",header=T,stringsAsFactors = T)
#SVMdata = read.csv(file.choose(),header=T)
SVMrawdata = SVMdata
SVMdata = SVMrawdata 
str(SVMdata)

summary(SVMdata)
#converting to 
#SVMdata$job= as.integer(SVMdata$job)
#SVMdata$marital= as.integer(SVMdata$marital)
#SVMdata$education = as.integer(SVMdata$education )
#SVMdata$default= as.integer(SVMdata$default)
#SVMdata$housing= as.integer(SVMdata$housing)
#SVMdata$loan= as.integer(SVMdata$loan)
#SVMdata$contact= as.integer(SVMdata$contact)
#SVMdata$month= as.integer(SVMdata$month)
#SVMdata$day_of_week= as.integer(SVMdata$day_of_week)
#SVMdata$emp.var.rate= as.integer(SVMdata$emp.var.rate)
#SVMdata$cons.price.idx= as.integer(SVMdata$cons.price.idx)
#SVMdata$cons.conf.idx= as.integer(SVMdata$cons.conf.idx)
#SVMdata$euribor3m= as.integer(SVMdata$euribor3m)
#SVMdata$nr.employed = as.integer(SVMdata$nr.employed )

str(SVMdata)
SVMdata[, 1:21] <- sapply(SVMdata[, 1:21], as.numeric)


###CORR plot
library(corrplot)
M_1 <- cor(SVMdata)
corrplot(M_1,method='color')

#converting dependent to factor
SVMdata$y = as.factor(SVMdata$y)

str(SVMdata)

###SCALE transformation
SVMdata[, 1:20] <- scale(SVMdata[, 1:20])
str(SVMdata)

####Splitting data
set.seed(100)
smp_size_1 <- floor(0.75 * nrow(SVMdata))
split_data_1 <- sample(seq_len(nrow(SVMdata)), size = smp_size_1)
SVMtrain = SVMdata[split_data_1,]
SVMtest = SVMdata[-split_data_1,]

dim(SVMtrain)
dim(SVMtest)


#Training the model
SVMtrain_Model <- svm(SVMtrain$y ~., data = SVMtrain, kernel="linear")
summary(SVMtrain_Model)

#Tesing
#SVM_prediction <- predict(SVMtrain_Model, SVMtrain[-21])
#caret::confusionMatrix(as.factor(SVMtrain$y), SVM_prediction)

SVM_prediction_1 <- predict(SVMtrain_Model, SVMtest[-21])
caret::confusionMatrix(as.factor(SVMtest$y), SVM_prediction_1)

###AUC
library(ModelMetrics)
auc(as.factor(SVMtest$y), SVM_prediction_1)

####ROC curve

library(ROCR)

ROCRpred_2 <- prediction(as.numeric(SVMtest$y),as.numeric (SVM_prediction_1))
ROCRperf_2 <- performance(ROCRpred_2, 'tpr','fpr')
plot(ROCRperf_2, colorize = TRUE, text.adj = c(-0.2,1.7))

legend("bottomright", c( "SVM - AUC = 0.63 "), lty=8, 
       col = "red", bty="n", inset=0)

