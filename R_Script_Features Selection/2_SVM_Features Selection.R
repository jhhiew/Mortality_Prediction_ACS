# SVM Features Selection 


setwd("D:/FYP/FYP_ACS/Full 4 Years Dataset with No Missing values/in_Hos/Latest 25 Jan/Using z-scores/SVM_zscores")

library(readr)


# read the train data 
train_data <- read.csv("train_afterPreprocess_inHos_ACS_normalised dataset_z-scores.csv", header = TRUE, sep = ",") 
dim(train_data)
train_data <- train_data[,2:56]   # delete the dummy column 1


# read the test data 
test_data <- read.csv("test_afterPreprocess_inHos_ACS_normalised dataset_z-scores.csv", header = TRUE, sep = ",") 
dim(test_data)  # 3956 * 55
test_data <- test_data[,2:56]

#    0    1 
# 4583 4651 
table(train_data$ptoutcome)
#    0    1 
#  203 3753 
table(test_data$ptoutcome)

# Convert all the outcome to factors 
train_data$ptoutcome <- ifelse(train_data$ptoutcome == 1, "Survive", "Died")
train_data$ptoutcome <- as.factor(train_data$ptoutcome)

test_data$ptoutcome <- ifelse(test_data$ptoutcome == 1, "Survive", "Died" )
test_data$ptoutcome <- as.factor(test_data$ptoutcome)
# test_data$ptoutcome <- as.numeric(test_data$ptoutcome) - 1


####################  Seleced Features  ###########################################
# Train with 35 variables 
selected_features <- c("ptoutcome","lipidla","ecgabnormlocationrv","canginamt2wk","cpvascular", "insulin",
                       "ldlc","cdm","ecgabnormtypestelev1", "pci","ecgabnormtypetwave", "ecgabnormtypebbb",
                       "cheartfail", "bb","cabg","ptrace","ck", "ccap", "cmi","smokingstatus","ecgabnormlocational", "statin",
                       "cardiaccath","crenal", "acei","hdlc","bpdias" , "canginapast2wk","tg", "ecgabnormtypestdep", 
                       "oralhypogly","antiarr","heartrate","fbg","ptageatnotification","killipclass")
                       

# Train with 14 variables without ptrace
selected_features_svm<- c("ptoutcome","lipidla",
                          "ldlc","ecgabnormtypestelev1",
                          "cabg", "statin",
                          "cardiaccath","hdlc", "canginapast2wk", "oralhypogly","antiarr","heartrate","fbg",
                          "ptageatnotification","killipclass") 

train_data = train_data[c(selected_features)] 
dim(train_data)   # 9234 * x1
test_data = test_data[c(selected_features)] 
dim(test_data) 


##################      SVM       #################################################
###################################################################################
library(caret)
library(doParallel)
registerDoParallel()

set.seed(333)
ctrl <- trainControl (method = "cv",
                      number = 10, 
                      classProbs = T,
                      summaryFunction=twoClassSummary)


set.seed(333)  
#Train SVM
model_svm <- train(form = ptoutcome~.,
                   data = train_data,
                   method = "svmLinear",
                   metric = "ROC",
                   trControl = ctrl)

model_svm

# save the model to disk
saveRDS(model_svm,"model_svm_ff.rds")

# load the model
model_svm <- readRDS("model_svm_ee.rds")
# model_svm <- readRDS("model_svm_NEW_1a.rds")

result.predicted <- predict(model_svm, newdata = test_data , type="prob" ) # Prediction
result.predicted 

# Default threshold -> 0.5
cm_all_svm <- confusionMatrix (predict(model_svm, newdata = test_data), test_data$ptoutcome)
cm_all_svm

library(pROC)
library(ROCR)

result.roc_svm <- roc(test_data$ptoutcome, result.predicted$Died, type="prob")
plot(result.roc_svm, print.thres = "best", print.thres.best.method = "closest.topleft")
round(result.roc_svm$auc, digit=8)


#############    Looping for Backward Elimination    ########################

x = colnames(train_data[2:17])

variable = c("all") #to store in roc list 
#result.roc_svm_lr$auc <- 0.869238927413078

# auc from the full model
roc_list = data.frame(variable,result.roc_svm$auc) #list to store auc for all variables 

# For loop
for(i in c(1:16)){
  print(x[i])
  #if (x[i] == y[i]){ 
  # data 
  train_data_sel = train_data[, !(names(train_data) %in% x[i])] #remove the col
  dim(train_data_sel)
  #} 
  
  #if (x[i] == y[i]){ 
  test_data_sel = test_data[, !(names(test_data) %in% x[i])] 
  dim(test_data_sel)
  #} 
  
  set.seed(333)
  ctrl <- trainControl (method = "cv",
                        number = 10, 
                        classProbs = T,
                        summaryFunction=twoClassSummary)
  
  #Train the model(original dataset)
  set.seed(333)
  model_svm <- train(form = ptoutcome~.,
                     data = train_data_sel,
                     method = "svmLinear",
                     metric = "ROC",
                     trControl = ctrl)
  
  #Confusion Matrix
  #cm_all_rf <- confusionMatrix (predict (model_rf, newdata = test_data_sel),
  #                              test_data_sel$ptoutcome)
  #cm_all_rf
  
  #Prediction
  result.predicted_svm <- predict(model_svm, newdata = test_data_sel, type = "prob")
  #result.predicted_rf
  
  #Plot ROC ---- Nonsurvival
  result.roc_svm1 <- roc(test_data_sel$ptoutcome, result.predicted_svm$Died,
                         type = "prob")
  
  #plot(result.roc_svm1, print.thres = "best", print.thres.best.method = "closest.topleft")
  result.roc_svm1
  
  ## Plot roc 
  #sel_roc <- roc(as.vector(Y_test_label_sel), as.vector(sel_prob), positive = 0, type ="prob") 
  new_row = c(x[i],result.roc_svm1$auc)
  roc_list = rbind(roc_list,new_row)
}

roc_list
roc_list_sort = roc_list[order(roc_list$result.roc_svm.auc),]
roc_list_sort

write.csv(roc_list_sort, "List After Eliminating_SVM_17_Variables.csv")

#####################################################################################\
# load the model
model_svm <- readRDS("model_svm_features_14_without ptrace.rds")
model_svm

result.predicted_svm <- predict(model_svm, newdata = test_data_svm , type="prob" ) # Prediction
result.predicted_svm

# Default threshold -> 0.5
cm_all_svm <- confusionMatrix (predict(model_svm, newdata = test_data_svm), test_data_svm$ptoutcome)
cm_all_svm


result.roc_svm <- roc(test_data_svm$ptoutcome, result.predicted_svm$Died, type="prob")
round(result.roc_svm$auc, digit=8)

##############################################################################################
################     TEST SEPERAETELY    #####################################################

# read the test data of stemi and nstemi respectively
test_stemi_data_svm <- read.csv("STEMI_test_afterPreprocess_inHos_ACS_normalised dataset_z-scores.csv", header = TRUE, sep = ",") 
test_nstemi_data_svm <- read.csv("NSTEMI_test_afterPreprocess_inHos_ACS_normalised dataset_z-scores.csv", header = TRUE, sep = ",")

dim(test_stemi_data_svm)   #2322 * 56
dim(test_nstemi_data_svm)  #1634 * 56

test_stemi_data_svm <- test_stemi_data_svm[,c(2:55,59)]    # delete the dummy column 1
test_nstemi_data_svm <- test_nstemi_data_svm[,c(2:55,59)]  # delete the dummy column 1

# Compute the actual outcome for graph drawing 
test_stemi_data_svm[,55]
# write.csv( test_stemi_data_svm[,55], "actual outcome SVM_STEMI (14 Var).csv")
test_nstemi_data_svm[,55]
# write.csv(test_nstemi_data_svm[,55] , "actual outcome SVM_NSTEMI (14 Var).csv")

test_stemi_data_svm$ptoutcome <- ifelse(test_stemi_data_svm$ptoutcome == 1, "Survive", "Died" )
test_stemi_data_svm$ptoutcome <- as.factor(test_stemi_data_svm$ptoutcome)
# test_stemi_data_svm$ptoutcome <- as.numeric(test_stemi_data_svm$ptoutcome) - 1

test_nstemi_data_svm$ptoutcome <- ifelse(test_nstemi_data_svm$ptoutcome == 1, "Survive", "Died")
test_nstemi_data_svm$ptoutcome <- as.factor(test_nstemi_data_svm$ptoutcome)
# test_nstemi_data_svm$ptoutcome <- as.numeric(test_nstemi_data_svm$ptoutcome) - 1 


test_stemi_data_svm = test_stemi_data_svm[c(selected_features_svm)] 
test_nstemi_data_svm = test_nstemi_data_svm[c(selected_features_svm)] 

dim(test_stemi_data_svm)      # 1634 * x
dim(test_nstemi_data_svm)     # 2322 * x


##########################   STEMI SVM  ############################################
result.predicted_stemi_svm <- predict(model_svm, newdata = test_stemi_data_svm , type="prob" ) # Prediction
# result.predicted_stemi_svm 
# write.csv(result.predicted_stemi_svm , "probability SVM_STEMI (14 Var).csv")

library(pROC)
library(ROCR)
pred_stemi_svm <- prediction(result.predicted_stemi_svm[,2], test_stemi_data_svm$ptoutcome)
eval <- performance(pred_stemi_svm, "acc")
#plot(eval)

# roc <- performance(pred_stemi_svm, "tpr", "fpr")
# plot(roc)

# Default threshold -> 0.5
cm_all_svm <- confusionMatrix (predict(model_svm, newdata = test_stemi_data_svm), test_stemi_data_svm$ptoutcome)
cm_all_svm


# Compute AUC for predicting Class with the model

result.roc_stemi_svm<- roc(test_stemi_data_svm$ptoutcome, result.predicted_stemi_svm$Died, type="prob")
#result.roc <- roc(test_data$ptoutcome, result.predicted_stemi_svm[,1], type="prob")
plot(result.roc_stemi_svm, print.thres = "best", print.thres.best.method = "closest.topleft")
result.roc_stemi_svm
ci.auc(test_stemi_data_svm$ptoutcome, result.predicted_stemi_svm$Died)


#############################   NSTEMI SVM  ##########################################
result.predicted_nstemi_svm<- predict(model_svm, newdata = test_nstemi_data_svm , type="prob" ) # Prediction
result.predicted_nstemi_svm 
# write.csv(result.predicted_nstemi_svm , "probability SVM_NSTEMI (14 Var).csv")

pred_nstemi_svm <- prediction(result.predicted_nstemi_svm[,2], test_nstemi_data_svm$ptoutcome)
# eval <- performance(pred_nstemi_sv,, "acc")
# plot(eval)

# roc_nstemi_svm <- performance(pred_nstemi_svm, "tpr", "fpr")
# plot(roc_nstemi_svm, col="purple")

# # Default threshold -> 0.5
cm_all_svm <- confusionMatrix (predict(model_svm, newdata = test_nstemi_data_svm), test_nstemi_data_svm$ptoutcome)
cm_all_svm


#  Compute AUC for predicting Class with the model
result.roc_nstemi_svm<- roc(test_nstemi_data_svm$ptoutcome, result.predicted_nstemi_svm$Died, type="prob")
# result.roc <- roc(test_data$ptoutcome, result.predicted[,1], type="prob")
plot(result.roc_nstemi_svm, print.thres = "best", print.thres.best.method = "closest.topleft")
result.roc_nstemi_svm
ci.auc(test_nstemi_data_svm$ptoutcome, result.predicted_nstemi_svm$Died)
