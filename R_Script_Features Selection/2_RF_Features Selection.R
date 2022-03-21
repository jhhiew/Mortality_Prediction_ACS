# RF Features Selection 


setwd("D:/FYP/FYP_ACS/Full 4 Years Dataset with No Missing values/in_Hos/Latest 25 Jan/Using z-scores/RF_zscores")

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


##############################################################################
# Remove the features
# train with 38 variables
# selected_features <- c("ptoutcome","statin","ecgabnormtypebbb","ldlc","ccerebrovascular","acei",
#                        "canginapast2wk","antiarr", "ecgabnormlocationil", "gpri","chpt", "heparin",
#                        "oralhypogly","ecgabnormlocational","ccap", "arb","smokingstatus","insulin",
#                        "crenal","bb","bpsys","cmi", "cpvascular","heartrate","cardiaccath",
#                        "ecgabnormlocationll","ck","ptsex","ecgabnormtypestelev2", "ecgabnormtypetwave",
#                        "cdys", "tc", "diuretic", "cheartfail","ecgabnormtypestelev1","asa", "fbg",
#                        "ptageatnotification", "killipclass")

# train with 30 variables
# selected_features <- c("ptoutcome","statin","ecgabnormtypebbb","ldlc","ccerebrovascular","acei",
#                        "ecgabnormlocationil", "heparin",
#                        "oralhypogly","ccap", "arb","insulin",
#                        "bb","bpsys","cmi", "cpvascular","heartrate","cardiaccath",
#                        "ecgabnormlocationll","ck","ptsex","ecgabnormtypestelev2", "ecgabnormtypetwave",
#                        "cdys", "tc",  "cheartfail","ecgabnormtypestelev1","asa", "fbg",
#                        "ptageatnotification", "killipclass")

# train with 34 variables
# selected_features <- c("ptoutcome","statin","ecgabnormtypebbb","ldlc","ccerebrovascular","acei",
#                        "antiarr", "ecgabnormlocationil", "gpri","chpt", "heparin",
#                        "oralhypogly","ecgabnormlocational","ccap", "arb","insulin",
#                        "bb","bpsys","cmi", "cpvascular","heartrate","cardiaccath",
#                        "ecgabnormlocationll","ck","ptsex","ecgabnormtypestelev2", "ecgabnormtypetwave",
#                        "cdys", "tc",  "cheartfail","ecgabnormtypestelev1","asa", "fbg",
#                        "ptageatnotification", "killipclass")

# train with 31 variables
# selected_features <- c("ptoutcome","statin","ldlc","ccerebrovascular","acei",
#                        "antiarr", "ecgabnormlocationil", "gpri","chpt", "heparin",
#                        "oralhypogly","ecgabnormlocational","ccap", "arb","insulin",
#                        "bb","bpsys", "cpvascular","heartrate","cardiaccath",
#                        "ecgabnormlocationll","ck","ecgabnormtypestelev2", "ecgabnormtypetwave",
#                        "cdys", "tc",  "cheartfail","ecgabnormtypestelev1","asa", "fbg",
#                        "ptageatnotification", "killipclass")

# train with 32 variables
# selected_features <- c("ptoutcome","statin","ldlc","ccerebrovascular","acei",
#                        "antiarr", "ecgabnormlocationil", "gpri","chpt", "heparin",
#                        "oralhypogly","ecgabnormlocational","ccap", "arb","insulin",
#                        "bb","bpsys","cmi", "cpvascular","heartrate","cardiaccath",
#                        "ecgabnormlocationll","ck","ecgabnormtypestelev2", "ecgabnormtypetwave",
#                        "cdys", "tc",  "cheartfail","ecgabnormtypestelev1","asa", "fbg",
#                        "ptageatnotification", "killipclass")

# train with 33 variables
# selected_features <- c("ptoutcome","statin","ldlc","ccerebrovascular","acei",
#                        "antiarr", "ecgabnormlocationil", "gpri","chpt", "heparin",
#                        "oralhypogly","ecgabnormlocational","ccap", "arb","insulin",
#                        "bb","bpsys","cmi", "cpvascular","heartrate","cardiaccath",
#                        "ecgabnormlocationll","ck","ptsex","ecgabnormtypestelev2", "ecgabnormtypetwave",
#                        "cdys", "tc",  "cheartfail","ecgabnormtypestelev1","asa", "fbg",
#                        "ptageatnotification", "killipclass")

# train with 29 variables
# selected_features <- c("ptoutcome","statin","ldlc","ccerebrovascular",
#                        "antiarr", "ecgabnormlocationil", "gpri","chpt", "heparin",
#                        "oralhypogly","ecgabnormlocational","ccap", "arb","insulin",
#                        "bb","bpsys","cmi", "cpvascular","heartrate","cardiaccath",
#                        "ecgabnormlocationll","ck","ptsex","ecgabnormtypestelev2", 
#                         "cheartfail","ecgabnormtypestelev1","asa", "fbg",
#                        "ptageatnotification", "killipclass")


# train with 31 variables
# selected_features <- c("ptoutcome","statin","ldlc","ccerebrovascular","acei",
#                        "antiarr", "ecgabnormlocationil", "gpri","chpt", "heparin",
#                        "oralhypogly","ecgabnormlocational","ccap", "arb","insulin",
#                        "bb","bpsys","cmi", "cpvascular","heartrate","cardiaccath",
#                        "ecgabnormlocationll","ck","ptsex","ecgabnormtypestelev2", 
#                         "tc",  "cheartfail","ecgabnormtypestelev1","asa", "fbg",
#                        "ptageatnotification", "killipclass")


# train with 30 variables
# selected_features <- c("ptoutcome","statin","ldlc","ccerebrovascular",
#                        "antiarr", "ecgabnormlocationil", "gpri","chpt", "heparin",
#                        "oralhypogly","ecgabnormlocational","ccap", "arb","insulin",
#                        "bb","bpsys","cmi", "cpvascular","heartrate","cardiaccath",
#                        "ecgabnormlocationll","ck","ptsex","ecgabnormtypestelev2", 
#                         "tc",  "cheartfail","ecgabnormtypestelev1","asa", "fbg",
#                        "ptageatnotification", "killipclass")

# train with 29 variables
# selected_features <- c("ptoutcome","statin","ldlc","acei",
#                        "antiarr", "ecgabnormlocationil", "gpri","chpt", "heparin",
#                        "oralhypogly","ecgabnormlocational","ccap", "arb","insulin",
#                        "bb","bpsys", "cpvascular","heartrate","cardiaccath",
#                        "ecgabnormlocationll","ck","ptsex","ecgabnormtypestelev2", 
#                         "tc",  "cheartfail","ecgabnormtypestelev1","asa", "fbg",
#                        "ptageatnotification", "killipclass")

# train with 27 variables
# selected_features <- c("ptoutcome","statin","acei",
#                        "antiarr", "ecgabnormlocationil", "gpri","chpt", "heparin",
#                        "oralhypogly","ecgabnormlocational","ccap", "arb","insulin",
#                        "bpsys", "cpvascular","heartrate","cardiaccath",
#                        "ecgabnormlocationll","ck","ptsex","ecgabnormtypestelev2", 
#                         "tc",  "cheartfail","ecgabnormtypestelev1","asa", "fbg",
#                        "ptageatnotification", "killipclass")

# train with 28 variables
# selected_features <- c("ptoutcome","statin","acei",
#                        "antiarr", "ecgabnormlocationil", "gpri","chpt", "heparin",
#                        "oralhypogly","ecgabnormlocational","ccap", "arb","insulin",
#                        "bb","bpsys", "cpvascular","heartrate","cardiaccath",
#                        "ecgabnormlocationll","ck","ptsex","ecgabnormtypestelev2", 
#                         "tc",  "cheartfail","ecgabnormtypestelev1","asa", "fbg",
#                        "ptageatnotification", "killipclass")


# train with 27 variables
# selected_features <- c("ptoutcome","statin","ldlc","acei",
#                        "antiarr", "ecgabnormlocationil", "gpri","chpt", "heparin",
#                        "oralhypogly","ecgabnormlocational","ccap",
#                        "bb","bpsys", "cpvascular","heartrate","cardiaccath",
#                        "ecgabnormlocationll","ck","ptsex","ecgabnormtypestelev2", 
#                         "tc",  "cheartfail","ecgabnormtypestelev1","asa", "fbg",
#                        "ptageatnotification", "killipclass")

# train with 28 variables
# selected_features <- c("ptoutcome","statin","ldlc","acei",
#                       "antiarr", "ecgabnormlocationil", "gpri","chpt", "heparin",
#                       "oralhypogly","ecgabnormlocational","ccap", "arb",
#                       "bb","bpsys", "cpvascular","heartrate","cardiaccath",
#                       "ecgabnormlocationll","ck","ptsex","ecgabnormtypestelev2", 
#                       "tc",  "cheartfail","ecgabnormtypestelev1","asa", "fbg",
#                       "ptageatnotification", "killipclass")

# train with 27 variables
selected_features <- c("ptoutcome","statin","ldlc","acei",
                       "antiarr", "ecgabnormlocationil", "gpri","chpt", "heparin",
                       "oralhypogly","ecgabnormlocational","ccap", "arb",
                       "bb","bpsys", "cpvascular","heartrate","cardiaccath",
                       "ecgabnormlocationll","ck","ecgabnormtypestelev2", 
                       "tc",  "cheartfail","ecgabnormtypestelev1","asa", "fbg",
                       "ptageatnotification", "killipclass")

# train with 20 variables
selected_features_rf <- c("ptoutcome","ldlc","acei",
                          "antiarr", "gpri","chpt", "heparin",
                          "oralhypogly","ccap", "arb",
                          "bb","bpsys", "cpvascular","heartrate","cardiaccath",
                          "ck",
                          "tc",  "cheartfail", "fbg",
                          "ptageatnotification", "killipclass")


train_data = train_data[c(selected_features_rf)] 
test_data = test_data[c(selected_features_rf)]                  

dim(train_data)           # 9234 * x
dim(test_data)            # 3956 * x


##################  Feature Selection   #########################################
###################################################################################
library(caret)

# library(doParallel)
# registerDoParallel()

set.seed(333)
ctrl <- trainControl (method = "cv",
                      number = 10, 
                      classProbs = T,
                      summaryFunction=twoClassSummary,
                      search = "grid")

#Train RF
set.seed(333) 
model_rf <- train(ptoutcome~.,
                  data = train_data,
                  method = "rf",
                  ntree = 1000,
                  trControl = ctrl,
                  metric = "ROC",
                  tuneLength =10)
model_rf

# model_rf <- read_rds('model_rf_NEW_1a.rds')

#Confusion Matrix
cm_all_rf<- confusionMatrix (predict (model_rf, newdata = test_data),
                             test_data$ptoutcome)
cm_all_rf


#Prediction
result.predicted_rf <- predict(model_rf, newdata = test_data, type = "prob")
result.predicted_rf

library(pROC)
library(ROCR)

#Plot ROC ---- Nonsurvival-------
result.roc1_rf<- roc(test_data$ptoutcome, result.predicted_rf$Died,
                     type = "prob")
plot(result.roc1_rf, print.thres = "best", print.thres.best.method = "closest.topleft")
result.roc1_rf
round(result.roc1_rf$auc, digits=10)


#############################################################################
#############    Looping for Backward Elimination    ########################

# Trying data first 

# sorted_data <- read.csv("Variables_Important_LR.csv", header = TRUE, sep = ",") 

#y = colnames(train_data) 
#z = colnames(test_stemi_data)

# x = sorted_data$X #get the rows names of all variable
x = colnames(train_data[2:28])  # remove ptoutcome

variable = c("all") #to store in roc list 
# result.roc_stemi$auc <- 0.895360788532851

# auc from the full model
roc_list = data.frame(variable,result.roc1_rf$auc) #list to store auc for all variables
roc_list 

# For loop
for(i in c(1:5)){ #from col 1 to 54
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
                        summaryFunction=twoClassSummary,
                        search = "grid" )
  
  
  set.seed(333) 
  model_rf <- train(ptoutcome~.,
                    data = train_data_sel,
                    method = "rf",
                    ntree = 1000,
                    trControl = ctrl, 
                    metric = "ROC",
                    tuneLength = 10)
  
  #Confusion Matrix
  #cm_all_rf <- confusionMatrix (predict (model_rf, newdata = test_data_sel),
  #                              test_data_sel$ptoutcome)
  #cm_all_rf
  
  #Prediction
  result.predicted_rf <- predict(model_rf, newdata = test_data_sel, type = "prob")
  #result.predicted_rf
  
  #Plot ROC ---- Nonsurvival
  result.roc_rf1 <- roc(test_data_sel$ptoutcome, result.predicted_rf$Died,
                        type = "prob")
  
  #plot(result.roc_svm1, print.thres = "best", print.thres.best.method = "closest.topleft")
  result.roc_rf1
  
  ## Plot roc 
  #sel_roc <- roc(as.vector(Y_test_label_sel), as.vector(sel_prob), positive = 0, type ="prob") 
  new_row = c(x[i],result.roc_rf1$auc)
  roc_list = rbind(roc_list,new_row)
}

roc_list
#roc_list_sort = roc_list[order(roc_list$result.roc1_rf.auc),]
#roc_list_sort

#write.csv(roc_list, "List After Eliminating_RF.csv")


# load the model
model_rf <- readRDS("model_rf_feature_20.rds")
model_rf


#Confusion Matrix
cm_all_rf<- confusionMatrix (predict (model_rf, newdata = test_data_rf),
                             test_data_rf$ptoutcome)
cm_all_rf


#Prediction
result.predicted_rf <- predict(model_rf, newdata = test_data_rf, type = "prob")
result.predicted_rf

library(pROC)
library(ROCR)

#Plot ROC ---- Nonsurvival-------
result.roc1_rf<- roc(test_data_rf$ptoutcome, result.predicted_rf$Died,
                     type = "prob")
plot(result.roc1_rf, print.thres = "best", print.thres.best.method = "closest.topleft")
result.roc1_rf
round(result.roc1_rf$auc, digits=10)


################     TEST SEPERAETELY    #####################################################

# read the test data of stemi and nstemi respectively
test_stemi_data_rf <- read.csv("STEMI_test_afterPreprocess_inHos_ACS_normalised dataset_z-scores.csv", header = TRUE, sep = ",") 
test_nstemi_data_rf <- read.csv("NSTEMI_test_afterPreprocess_inHos_ACS_normalised dataset_z-scores.csv", header = TRUE, sep = ",")

dim(test_stemi_data_rf)   #2322 * 56
dim(test_nstemi_data_rf)  #1634 * 56

test_stemi_data_rf <- test_stemi_data_rf[,c(2:55,59)]    # delete the dummy column 1
test_nstemi_data_rf <- test_nstemi_data_rf[,c(2:55,59)]  # delete the dummy column 1

# Compute the actual outcome for graph drawing 
test_stemi_data_rf[,55]
# write.csv( test_stemi_data_rf[,55], "actual outcome RF_STEMI (20 Var).csv")
test_nstemi_data_rf[,55]
# write.csv(test_nstemi_data_rf[,55] , "actual outcome RF_NSTEMI (20 Var).csv")


test_stemi_data_rf$ptoutcome <- ifelse(test_stemi_data_rf$ptoutcome == 1, "Survive", "Died" )
test_stemi_data_rf$ptoutcome <- as.factor(test_stemi_data_rf$ptoutcome)
# test_stemi_data_rf$ptoutcome <- as.numeric(test_stemi_data_rf$ptoutcome) - 1

test_nstemi_data_rf$ptoutcome <- ifelse(test_nstemi_data_rf$ptoutcome == 1, "Survive", "Died")
test_nstemi_data_rf$ptoutcome <- as.factor(test_nstemi_data_rf$ptoutcome)
# test_nstemi_data_rf$ptoutcome <- as.numeric(test_nstemi_data_rf$ptoutcome) - 1 


test_stemi_data_rf = test_stemi_data_rf[c(selected_features_rf)] 
test_nstemi_data_rf = test_nstemi_data_rf[c(selected_features_rf)] 

dim(test_stemi_data_rf)      # 1634 * x
dim(test_nstemi_data_rf)     # 2322 * x


##########################   STEMI   ################################################
result.predicted_stemi_rf <- predict(model_rf, newdata = test_stemi_data_rf , type="prob" ) # Prediction
result.predicted_stemi_rf 

library(pROC)
library(ROCR)
pred_stemi_rf <- prediction(result.predicted_stemi_rf[,2], test_stemi_data_rf$ptoutcome)
eval <- performance(pred_stemi_rf, "acc")
#plot(eval)

# roc <- performance(pred_stemi_rf, "tpr", "fpr")
# plot(roc)

# Default threshold -> 0.5
cm_all_stemi_rf <- confusionMatrix (predict(model_rf, newdata = test_stemi_data_rf), test_stemi_data_rf$ptoutcome)
cm_all_stemi_rf



# Compute AUC for predicting Class with the model

result.roc_stemi_rf<- roc(test_stemi_data_rf$ptoutcome, result.predicted_stemi_rf$Died, type="prob")
#result.roc <- roc(test_stemi_data_rf$ptoutcome, result.predicted_stemi_svm[,1], type="prob")
# plot(result.roc_stemi_data_rf, print.thres = "best", print.thres.best.method = "closest.topleft")
plot(result.roc_stemi_rf,col="blue", legacy.axes = TRUE,
     main= "ROC Curves of Different Models for In-hospital STEMI Patients")
result.roc_stemi_rf
ci.auc(test_stemi_data_rf$ptoutcome, result.predicted_stemi_rf$Died) #0.893952165988236, 0.925454272478487


######################  NSTEMI  #####################################################
result.predicted_nstemi_rf <- predict(model_rf, newdata = test_nstemi_data_rf , type="prob" ) # Prediction
result.predicted_nstemi_rf 

library(pROC)
library(ROCR)
pred_nstemi_rf <- prediction(result.predicted_nstemi_rf[,2], test_nstemi_data_rf$ptoutcome)
eval <- performance(pred_nstemi_rf, "acc")
#plot(eval)

# roc <- performance(pred_nstemi_rf, "tpr", "fpr")
# plot(roc)

# Default threshold -> 0.5
cm_all_nstemi_rf <- confusionMatrix (predict(model_rf, newdata = test_nstemi_data_rf), test_nstemi_data_rf$ptoutcome)
cm_all_nstemi_rf


# Compute AUC for predicting Class with the model

result.roc_nstemi_rf<- roc(test_nstemi_data_rf$ptoutcome, result.predicted_nstemi_rf$Died, type="prob")
#result.roc <- roc(test_nstemi_data_rf$ptoutcome, result.predicted_nstemi_rf[,1], type="prob")
# plot(result.roc_nstemi_data_rf, print.thres = "best", print.thres.best.method = "closest.topleft")
plot(result.roc_nstemi_rf,col="blue", legacy.axes = TRUE,
     main= "ROC Curves of Different Models for In-hospital NSTEMI Patients")
result.roc_nstemi_rf
ci.auc(test_nstemi_data_rf$ptoutcome, result.predicted_nstemi_rf$Died)