install.packages('factoextra')
install.packages('magrittr')
install.packages('dplyr')
library(factoextra)
library(magrittr) 
library(dplyr)

Desic_data = read.csv("C:/Users/abdul/Desktop/SEM 1 Modules/ML1/Project/Dataset/FINAL datasets/Churn/Telecom_customer churn.csv", header=T)
str(Desic_data) 
Desic_data_raw = Desic_data

########DATA EXPLORATION########################3

summary(Desic_data)
sum(is.na(Desic_data))

####converting the output variable into factor
Desic_data$churn = as.factor(Desic_data$churn)

######
#complete.cases(Desic_data)
#x= Desic_data[complete.cases(Desic_data),]


####Removing the Blank categorical variable

Desic_data <- Desic_data[!(Desic_data$prizm_social_one == ""), ]
Desic_data <- Desic_data[!(Desic_data$dualband == ""), ]
Desic_data <- Desic_data[!(Desic_data$refurb_new == ""), ]
Desic_data <- Desic_data[!(Desic_data$hnd_webcap == ""), ]
Desic_data <- Desic_data[!(Desic_data$ownrent == ""), ]
Desic_data <- Desic_data[!(Desic_data$dwlltype == ""), ]
Desic_data <- Desic_data[!(Desic_data$marital == ""), ]
Desic_data <- Desic_data[!(Desic_data$infobase == ""), ]
Desic_data <- Desic_data[!(Desic_data$HHstatin == ""), ]
Desic_data <- Desic_data[!(Desic_data$dwllsize == ""), ]
Desic_data <- Desic_data[!(Desic_data$kid0_2 == ""), ]
Desic_data <- Desic_data[!(Desic_data$kid3_5 == ""), ]
Desic_data <- Desic_data[!(Desic_data$kid6_10 == ""), ]
Desic_data <- Desic_data[!(Desic_data$kid11_15 == ""), ]
Desic_data <- Desic_data[!(Desic_data$kid16_17 == ""), ]
Desic_data <- Desic_data[!(Desic_data$creditcd == ""), ]
str(Desic_data)

summary(Desic_data)

###removing Null continuous values
NA.post <- which(is.na(Desic_data$recv_vce_Mean))
Desic_data <- Desic_data[-c(NA.post),]
#str(Desic_data)
summary(Desic_data)



#####Handling the missing values in continuous variables with Mean Value

NA.position <- which(is.na(Desic_data$change_rev))
Desic_data$change_rev[NA.position] = mean(Desic_data$change_rev, na.rm = TRUE)

NA.position <- which(is.na(Desic_data$change_mou))
Desic_data$change_mou[NA.position] = mean(Desic_data$change_mou, na.rm = TRUE)

NA.position <- which(is.na(Desic_data$avg6mou))
Desic_data$avg6mou[NA.position] = mean(Desic_data$avg6mou, na.rm = TRUE)
##
NA.position <- which(is.na(Desic_data$avg6qty))
Desic_data$avg6qty[NA.position] = mean(Desic_data$avg6qty, na.rm = TRUE)

NA.position <- which(is.na(Desic_data$avg6rev))
Desic_data$avg6rev[NA.position] = mean(Desic_data$avg6rev, na.rm = TRUE)

NA.position <- which(is.na(Desic_data$hnd_price))
Desic_data$hnd_price[NA.position] = mean(Desic_data$hnd_price, na.rm = TRUE)

NA.position <- which(is.na(Desic_data$numbcars))
Desic_data$numbcars[NA.position] = mean(Desic_data$numbcars, na.rm = TRUE)

####
NA.position <- which(is.na(Desic_data$rev_Mean))
Desic_data$rev_Mean[NA.position] = mean(Desic_data$rev_Mean, na.rm = TRUE)

NA.position <- which(is.na(Desic_data$mou_Mean))
Desic_data$mou_Mean[NA.position] = mean(Desic_data$mou_Mean, na.rm = TRUE)

NA.position <- which(is.na(Desic_data$totmrc_Mean))
Desic_data$totmrc_Mean[NA.position] = mean(Desic_data$totmrc_Mean, na.rm = TRUE)

NA.position <- which(is.na(Desic_data$da_Mean))
Desic_data$da_Mean[NA.position] = mean(Desic_data$da_Mean, na.rm = TRUE)

NA.position <- which(is.na(Desic_data$ovrmou_Mean))
Desic_data$ovrmou_Mean[NA.position] = mean(Desic_data$ovrmou_Mean, na.rm = TRUE)

NA.position <- which(is.na(Desic_data$ovrrev_Mean))
Desic_data$ovrrev_Mean[NA.position] = mean(Desic_data$ovrrev_Mean, na.rm = TRUE)

NA.position <- which(is.na(Desic_data$vceovr_Mean))
Desic_data$vceovr_Mean[NA.position] = mean(Desic_data$vceovr_Mean, na.rm = TRUE)

NA.position <- which(is.na(Desic_data$datovr_Mean))
Desic_data$datovr_Mean[NA.position] = mean(Desic_data$datovr_Mean, na.rm = TRUE)

NA.position <- which(is.na(Desic_data$roam_Mean))
Desic_data$roam_Mean[NA.position] = mean(Desic_data$roam_Mean, na.rm = TRUE)

NA.position <- which(is.na(Desic_data$change_mou))
Desic_data$change_mou[NA.position] = mean(Desic_data$change_mou, na.rm = TRUE)

NA.position <- which(is.na(Desic_data$change_rev))
Desic_data$change_rev[NA.position] = mean(Desic_data$change_rev, na.rm = TRUE)


summary(Desic_data)
str(Desic_data)

########## Seperation of Continuous and Categorical variables for PCA analysis

Desic_data$churn = as.factor(Desic_data$churn)

##Seperating the numerical and categorical columns

Categorical_columns= c('new_cell','crclscod','asl_flag','prizm_social_one','area','dualband','refurb_new','hnd_webcap','ownrent','dwlltype','marital',
                       'infobase','HHstatin','dwllsize','ethnic','kid0_2','kid3_5','kid6_10','kid11_15','kid16_17','creditcd','churn')


chrun_num= Desic_data[,c(1:48,50,51,52,56:70,75,76,77,79,80,82,85,87,88,91,99)]

chrun_fac= Desic_data[Categorical_columns]

str(chrun_fac)
str(chrun_num)

summary(chrun_num)
sum(is.na(chrun_num))
sum(is.na(chrun_fac))

##############################PCA ANALYSIS on the Continous variables###################

res.pca <- prcomp(chrun_num, scale = TRUE)

names(res.pca)         #Viewing the PCA output

fviz_eig(res.pca)      #Visualizing the PCA Output for finding the Optimum value

res.pca$rotation[,1:5]

res.pca$x[,1:5]         

fviz_pca_var(res.pca,col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE)

eig.val <- get_eigenvalue(res.pca)
eig.val
res.var <- get_pca_var(res.pca)
res.var$coord          
res.var$contrib       
res.var$cos2        

res.ind <- get_pca_ind(res.pca)           #Individual coordinates contribution
res.ind$coord

Churn_PCA = cbind(res.pca$x[,1:5],chrun_fac)        #combining PCA result and continous variables

str(Churn_PCA)       ##analyzing the result
summary(Churn_PCA)

#####Oversampling
Churn_PCA$churn=as.factor(Churn_PCA$churn)
Churn_samp <- ovun.sample(churn~.,data = Churn_PCA,method = "over")$data 
prop.table(table(Churn_samp$churn)) * 100

str(Churn_samp)

##deleting unnecessary factor
PCA_churn = select (Churn_samp,-c(crclscod))

###splitting the data
set.seed(200)
smp_size_ch <- floor(0.70 * nrow(PCA_churn))
split_data_ch <- sample(seq_len(nrow(PCA_churn)), size = smp_size_ch)
Chruntrain = PCA_churn[split_data_ch,]
Chruntest = PCA_churn[-split_data_ch,]
dim(Chruntrain)
dim(Chruntest)
str()

str(Chruntrain)
summary(Chruntrain)

###########
#levels(Chruntrain$prizm_social_one)[4]= "missing"
#levels(Chruntrain$area)[2]= "missing"
#levels(Chruntrain$dualband)[2]= "missing"
#levels(Chruntrain$refurb_new)[2]= "missing"
#levels(Chruntrain$hnd_webcap)[3]= "missing"
#levels(Chruntrain$ownrent)[2]= "missing"
#levels(Chruntrain$dwlltype)[3]= "missing"
#levels(Chruntrain$marital)[4]= "missing"
#levels(Chruntrain$infobase)[2]= "missing"
#levels(Chruntrain$HHstatin)[4]= "missing"
#levels(Chruntrain$dwllsize)[2]= "missing"
#levels(Chruntrain$ethnic)[12]= "missing"
#levels(Chruntrain$kid0_2)[3]= "missing"
#levels(Chruntrain$kid3_5)[3]= "missing"
#levels(Chruntrain$kid6_10)[3]= "missing"
#levels(Chruntrain$kid11_15)[2]= "missing"
#levels(Chruntrain$kid16_17)[2]= "missing"
#levels(Chruntrain$creditcd)[3]= "missing"
str(Chruntrain)

##########################DECISION TREE#############################################

install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

###Training the model
fit <- rpart(churn~., data = Chruntrain, method = 'class')
rpart.plot(fit, extra = 100)

###Prediction
predict_unseen <-predict(fit, Chruntest, type = 'class')

##### CONFUSTION MATRIX##################
caret::confusionMatrix(Chruntest$churn, predict_unseen)

varImp(fit)     ####Variable importance for Feature Selection             

########## Training Decision tree with Feature selection######################
fit_2= rpart(churn~ area+ethnic+hnd_webcap+PC2+PC3+PC4+PC5, data = Chruntrain, method = 'class')
rpart.plot(fit_2, extra = 100)

###Prediction
predict_unseen_2 <-predict(fit_2, Chruntest, type = 'class')

##COnfustion matrix
caret::confusionMatrix(Chruntest$churn, predict_unseen_2)

##AUC
library(ModelMetrics)
auc(Chruntest$churn, predict_unseen)        ##AUC value without Feature Selection
auc(Chruntest$churn, predict_unseen_2)      ##AUC value with Feature selection

##ROC curve without Feature selection
library(ROCR)

ROCRpred_ch_decision <- prediction(as.numeric(Chruntest$churn),as.numeric(predict_unseen))
ROCRperf_ch_decision <- performance(ROCRpred_ch_decision, 'tpr','fpr')
plot(ROCRperf_ch_decision, colorize = TRUE, text.adj = c(-0.2,1.7))

###################ROC curve with Feature selction
ROCRpred_ch_decision1 <- prediction(as.numeric(Chruntest$churn),as.numeric(predict_unseen_2))
ROCRperf_ch_decision1 <- performance(ROCRpred_ch_decision1, 'tpr','fpr')
plot(ROCRperf_ch_decision1, colorize = TRUE, text.adj = c(-0.2,1.7))

########################################################################################
##################################RANDOM FOREST########################################
library(caTools)
library(caret)
library(randomForest)

#####initial Random forest

#ch_model <- randomForest(churn ~., data = Chruntrain, importance = TRUE)
#ch_model

#pred_churn <- predict(ch_model, Chruntest, type = "class")
#caret::confusionMatrix(pred_churn, Chruntest$churn)


#Building the Random forest model with tuning factors

ch_model_1 <- randomForest(churn ~., data = Chruntrain, ntree = 500, mtry=6,importance = TRUE, na.action = na.roughfix)
ch_model_1

pred_churn_1 <- predict(ch_model_1, Chruntest, type = "class")
caret::confusionMatrix(pred_churn_1, Chruntest$churn)

varImpPlot(ch_model_1)                     #Variance importance Plot

ch_model_2 <- randomForest(churn ~ area +ethnic+PC1+PC4+PC2+PC5+prizm_social_one+PC3+dwllsize, data = Chruntrain, ntree = 500, mtry=6,importance = TRUE, na.action = na.roughfix)
ch_model_2

varImpPlot(ch_model_2)

#Predicting the Model using the Tesing dataset
pred_churn <- predict(ch_model_2, Chruntest, type = "class")

#Confusion Matrix
caret::confusionMatrix(pred_churn, Chruntest$churn)

##AUC
library(ModelMetrics)
auc(pred_churn, Chruntest$churn)

##### ROC curve without feature selection
ROCRpred_ch_1 <- prediction(as.numeric(pred_churn_1),as.numeric(Chruntest$churn))
ROCRperf_ch_1 <- performance(ROCRpred_ch_1, 'tpr','fpr')
plot(ROCRperf_ch_1, colorize = TRUE, text.adj = c(-0.2,1.7))


####ROCR with feature selection
library(ROCR)
ROCRpred_ch <- prediction(as.numeric(pred_churn),as.numeric(Chruntest$churn))
ROCRperf_ch <- performance(ROCRpred_ch, 'tpr','fpr')
plot(ROCRperf_ch, colorize = TRUE, text.adj = c(-0.2,1.7))

############################################################################################################

####COmbined ROC AUC curve without feature selection#################
plot(ROCRperf_ch_decision, main="AUC ROC Curve",col ="red")
plot(ROCRperf_ch_1, add = TRUE, col ="blue")

legend("bottomright", c( "Decision tree ROC curve","Random Forest ROC curve"), lty=5, 
       col = c("red", "blue"), bty="n", inset=c(0,0.15))
legend("bottomright", c( "Decision tree - AUC = 0.53 ","Random Forest - AUC = 0.55"), lty=8, 
       col = c("red", "blue"), bty="n", inset=c(0,0.50))


####COmbined ROC AUC curve with feature selection#########
plot(ROCRperf_ch_decision, main="AUC ROC Curve",col ="red")
plot(ROCRperf_ch, add = TRUE, col ="blue")

legend("bottomright", c( "Decision tree ROC curve","Random Forest ROC curve"), lty=5, 
       col = c("red", "blue"), bty="n", inset=c(0,0.15))
legend("bottomright", c( "Decision tree - AUC = 0.53 ","Random Forest - AUC = 0.70"), lty=8, 
       col = c("red", "blue"), bty="n", inset=c(0,0.50))

