# LR - Feature Selection 

setwd("D:/FYP/FYP_ACS/Full 4 Years Dataset with No Missing values/in_Hos/Latest 25 Jan/LR")

library(readr)

# read the train data 
train_data<- read.csv("train_afterPreprocess_inHos_ACS_normalised dataset.csv", header = TRUE, sep = ",") 
dim(train_data)
train_data<- train_data[,2:56]   # delete the dummy column 1


# read the test data 
test_data <- read.csv("test_afterPreprocess_inHos_ACS_normalised dataset.csv", header = TRUE, sep = ",") 
dim(test_data)  # 3956 * 55
test_data<- test_data[,2:56] 

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


###########################################################################################

# Remove the features
# train with 38 variables
selected_features <- c("ptoutcome","ecgabnormlocationrv","calcantagonist","ldlc","canginamt2wk","clung", 
                       "ecgabnormtypetwave","chpt", "heparin","ecgabnormlocationll","cpvascular", "tg", "insulin",
                       "bb","bpdias", "ecgabnormtypestelev1","acei", "cheartfail","crenal","hdlc", 
                       "ecgabnormlocational", "cmi", "ptrace", "smokingstatus","cardiaccath", "cabg", 
                       "bpsys","statin","pci","ecgabnormtypebbb","ck","oralhypogly","canginapast2wk",
                       "ecgabnormtypestdep","antiarr","fbg", "heartrate","ptageatnotification", "killipclass")

# train with 30 variables
selected_features <- c("ptoutcome","ldlc",
                       "ecgabnormtypetwave","chpt", "cpvascular", "tg",
                       "bb","bpdias", "ecgabnormtypestelev1","acei", "cheartfail","crenal","hdlc", 
                       "ecgabnormlocational",  "ptrace", "smokingstatus","cardiaccath", "cabg", 
                       "bpsys","statin","pci","ecgabnormtypebbb","ck","oralhypogly","canginapast2wk",
                       "ecgabnormtypestdep","antiarr","fbg", "heartrate","ptageatnotification", "killipclass")

# train with 22 variables
selected_features <- c("ptoutcome","ldlc","tg",
                       "bb", "ecgabnormtypestelev1","acei", "hdlc", 
                       "ecgabnormlocational",  "ptrace", "smokingstatus","cardiaccath", "cabg", 
                       "bpsys","statin","ecgabnormtypebbb","oralhypogly","canginapast2wk",
                       "ecgabnormtypestdep","antiarr","fbg", "heartrate","ptageatnotification", "killipclass")

# train with 18 variables
selected_features <- c("ptoutcome","tg",
                       "bb", "ecgabnormtypestelev1", "hdlc", 
                       "ecgabnormlocational", "cardiaccath", "cabg", 
                       "bpsys","statin","ecgabnormtypebbb","oralhypogly","canginapast2wk",
                       "ecgabnormtypestdep","antiarr","fbg", "heartrate","ptageatnotification", "killipclass")

# train with 17 variables
selected_features <- c("ptoutcome","tg",
                       "bb", "hdlc", 
                       "ecgabnormlocational", "cardiaccath", "cabg", 
                       "bpsys","statin","ecgabnormtypebbb","oralhypogly","canginapast2wk",
                       "ecgabnormtypestdep","antiarr","fbg", "heartrate","ptageatnotification", "killipclass")

train_data = train_data[c(selected_features)] 
test_data = test_data[c(selected_features)]                  

dim(train_data)           # 9234 * x
dim(test_data)            # 3956 * x

##################  Feature Selection   #########################################
###################################################################################


# method = glm specifies that we will fit a generalized linear model

library(caret)

set.seed(333)
ctrl <- trainControl(method = "cv",
                     number = 10, 
                     classProbs = T, 
                     savePredictions = TRUE)

# tunegrid <- expand.grid(.mtry=c(1:10))

set.seed(333)
model_lr <- train(ptoutcome ~., 
                  data=train_data, 
                  method="glm", 
                  family="binomial",
                  trControl = ctrl, 
                  tuneLength = 10)
model_lr

# save the model
# saveRDS(model_lr,"model_lr_feature_1.rds")

# load the model
# model_lr <- readRDS("model_lr_1.rds")

result.predicted <- predict(model_lr, newdata = test_data , type="prob" ) # Prediction
result.predicted 

library(pROC)
library(ROCR)

# Default threshold -> 0.5
cm_all_lr <- confusionMatrix (predict(model_lr, newdata = test_data), test_data$ptoutcome)
cm_all_lr


# Compute AUC for predicting Class with the model

result.roc_lr<- roc(test_data$ptoutcome, result.predicted$Died, type="prob")
#result.roc <- roc(test_data$ptoutcome, result.predicted[,1], type="prob")
plot(result.roc_lr, print.thres = "best", print.thres.best.method = "closest.topleft")
result.roc_lr
round(result.roc_lr$auc, digits=10)


#############    Looping for Backward Elimination    ########################

# Trying data first 

# sorted_data <- read.csv("Variables_Important_LR.csv", header = TRUE, sep = ",") 

#y = colnames(train_data) 
#z = colnames(test_stemi_data)

# x = sorted_data$X #get the rows names of all variable
x = colnames(train_data[2:19])  # remove ptoutcome

variable = c("all") #to store in roc list 
# result.roc_stemi$auc <- 0.895360788532851

# auc from the full model
roc_list = data.frame(variable,result.roc_lr$auc) #list to store auc for all variables 

# For loop
for(i in c(1:18)){ 
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
  ctrl <- trainControl (method = "cv",number = 10, classProbs = T,savePrediction = T)
  
  #Train the model(original dataset)
  set.seed(333) 
  model_lr <- train(ptoutcome ~., 
                    data=train_data_sel, 
                    method="glm", 
                    family="binomial",
                    trControl = ctrl, 
                    tuneLength = 10)
  
  #Confusion Matrix
  #cm_all_rf <- confusionMatrix (predict (model_rf, newdata = test_data_sel),
  #                              test_data_sel$ptoutcome)
  #cm_all_rf
  
  #Prediction
  result.predicted_lr <- predict(model_lr, newdata = test_data_sel, type = "prob")
  #result.predicted_rf
  
  #Plot ROC ---- Nonsurvival
  result.roc1 <- roc(test_data_sel$ptoutcome, result.predicted_lr$Died,
                     type = "prob")
  
  #plot(result.roc1, print.thres = "best", print.thres.best.method = "closest.topleft")
  result.roc1
  
  ## Plot roc 
  #sel_roc <- roc(as.vector(Y_test_label_sel), as.vector(sel_prob), positive = 0, type ="prob") 
  new_row = c(x[i],result.roc1$auc)
  roc_list = rbind(roc_list,new_row)
}

roc_list
roc_list_sort = roc_list[order(roc_list$result.roc_lr.auc),]
roc_list_sort

write.csv(roc_list_sort, "List After Eliminating_LR_Sorted(with 18 variables).csv")


##############################################################################################
################     TEST SEPERAETELY    #####################################################

# read the test data of stemi and nstemi respectively
test_stemi_data <- read.csv("STEMI_test_afterPreprocess_inHos_ACS_normalised dataset_with TIMI Scores.csv", header = TRUE, sep = ",") 
test_nstemi_data <- read.csv("NSTEMI_test_afterPreprocess_inHos_ACS_normalised dataset_with TIMI Scores.csv", header = TRUE, sep = ",")

dim(test_stemi_data)   #2322 * 56
dim(test_nstemi_data)  #1634 * 56

test_stemi_data <- test_stemi_data[,c(2:55,59)]    # delete the dummy column 1
test_nstemi_data <- test_nstemi_data[,c(2:55,59)]  # delete the dummy column 1

test_stemi_data$ptoutcome <- ifelse(test_stemi_data$ptoutcome == 1, "Survive", "Died" )
test_stemi_data$ptoutcome <- as.factor(test_stemi_data$ptoutcome)
# test_stemi_data$ptoutcome <- as.numeric(test_stemi_data$ptoutcome) - 1

test_nstemi_data$ptoutcome <- ifelse(test_nstemi_data$ptoutcome == 1, "Survive", "Died")
test_nstemi_data$ptoutcome <- as.factor(test_nstemi_data$ptoutcome)
# test_nstemi_data$ptoutcome <- as.numeric(test_nstemi_data$ptoutcome) - 1 


test_stemi_data = test_stemi_data[c(selected_features)] 
test_nstemi_data = test_nstemi_data[c(selected_features)] 

dim(test_stemi_data)      # 1634 * x
dim(test_nstemi_data)     # 2322 * x

##########################   STEMI   ############################################
result.predicted <- predict(model_lr, newdata = test_stemi_data , type="prob" ) # Prediction
result.predicted 

library(pROC)
library(ROCR)
pred <- prediction(result.predicted[,2], test_stemi_data$ptoutcome)
eval <- performance(pred, "acc")
plot(eval)

roc <- performance(pred, "tpr", "fpr")
plot(roc)

# Default threshold -> 0.5
cm_all_lr <- confusionMatrix (predict(model_lr, newdata = test_stemi_data), test_stemi_data$ptoutcome)
cm_all_lr


# Compute AUC for predicting Class with the model

result.roc_lr<- roc(test_stemi_data$ptoutcome, result.predicted$Died, type="prob")
#result.roc <- roc(test_data$ptoutcome, result.predicted[,1], type="prob")
plot(result.roc_lr, print.thres = "best", print.thres.best.method = "closest.topleft")
result.roc_lr
ci.auc(test_stemi_data$ptoutcome, result.predicted$Died)  # 95% CI: 0.8909-0.9392 (DeLong)


#############################   NSTEMI   ##########################################
result.predicted <- predict(model_lr, newdata = test_nstemi_data , type="prob" ) # Prediction
result.predicted 

library(pROC)
library(ROCR)
pred <- prediction(result.predicted[,2], test_nstemi_data$ptoutcome)
eval <- performance(pred, "acc")
plot(eval)

roc <- performance(pred, "tpr", "fpr")
plot(roc)

# Default threshold -> 0.5
cm_all_lr <- confusionMatrix (predict(model_lr, newdata = test_nstemi_data), test_nstemi_data$ptoutcome)
cm_all_lr


# Compute AUC for predicting Class with the model

result.roc_lr<- roc(test_nstemi_data$ptoutcome, result.predicted$Died, type="prob")
#result.roc <- roc(test_data$ptoutcome, result.predicted[,1], type="prob")
plot(result.roc_lr, print.thres = "best", print.thres.best.method = "closest.topleft")
result.roc_lr
ci.auc(test_nstemi_data$ptoutcome, result.predicted$Died) #95% CI: 0.8297-0.9059 (DeLong)
