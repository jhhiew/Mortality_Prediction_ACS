# Random Forest using z-scores 

# Machine Learning ACS inHos RF (combine testing STEMI NSTEMI) 
# with 70:30 Data 
# with z-scores Normalization 
# With Looping for features selection 

setwd("D:/FYP/FYP_ACS/Full 4 Years Dataset with No Missing values/in_Hos/Latest 25 Jan/Using z-scores/RF_zscores")

library(readr)
library(caret)

# read the train data 
train_data_rf <- read.csv("train_afterPreprocess_inHos_ACS_normalised dataset_z-scores.csv", header = TRUE, sep = ",") 
dim(train_data_rf)
train_data_rf <- train_data_rf[,2:56]   # delete the dummy column 1


# read the test data 
test_data_rf <- read.csv("test_afterPreprocess_inHos_ACS_normalised dataset_z-scores.csv", header = TRUE, sep = ",") 
dim(test_data_rf)  # 3956 * 55
test_data_rf<- test_data_rf[,2:56] 

#    0    1 
# 4583 4651 
table(train_data_rf$ptoutcome)
#    0    1 
#  203 3753 
table(test_data_rf$ptoutcome)

# Convert all the outcome to factors 
train_data_rf$ptoutcome <- ifelse(train_data_rf$ptoutcome == 1, "Survive", "Died")
train_data_rf$ptoutcome <- as.factor(train_data_rf$ptoutcome)

test_data_rf$ptoutcome <- ifelse(test_data_rf$ptoutcome == 1, "Survive", "Died" )
test_data_rf$ptoutcome <- as.factor(test_data_rf$ptoutcome)
# test_data_rf$ptoutcome <- as.numeric(test_data_rf$ptoutcome) - 1


##################      RF      #################################################
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

# tunegrid <- expand.grid(.mtry=c(1:10))
# tunegrid <- expand.grid(.ntree=c(500,1000,1500,2000))

# Train RF
set.seed(333) 
model_rf <- train(ptoutcome~.,
                  data = train_data_rf,
                  method = "rf",
                  ntree = 1000,
                  # preProcess = c("scale","center"),
                  # preProcess = "range",
                  trControl = ctrl,
                  metric = "ROC",
                  tuneLength =10)
model_rf

# save the model
saveRDS(model_rf,"model_rf_NEW_4.rds")

# load the model
model_rf <- readRDS("model_rf_NEW_1a.rds")

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


threshold <- 0.213
pred <- factor(ifelse(result.predicted_rf[,"Died"]>threshold,"Died","Survive"))
pred <- relevel(pred,"Died")
confusionMatrix(pred,test_data_rf$ptoutcome)


#######################        CHECKING            ############################
####################### For Train Data Itself #################################
result.predicted <- predict(model_rf, newdata = train_data_rf , type="prob" ) # Prediction

cm_all_rf <- confusionMatrix (predict(model_rf, newdata = train_data_rf), train_data_rf$ptoutcome)
cm_all_rf

result.roc_svm<- roc(train_data_rf$ptoutcome, result.predicted$Died, type="prob")
#result.roc <- roc(test_data_rf$ptoutcome, result.predicted[,1], type="prob")
plot(result.roc_svm, print.thres = "best", print.thres.best.method = "closest.topleft")
result.roc_svm


##############     Variables Importance      ################################
rf_importance <- varImp(model_rf)
rf_importance
#write.csv(rf_importance[[1]], "Variables_Important_RF.csv")


# To get the area under the ROC curve for each predictor
roc_imp <- filterVarImp(x = train_data_rf[, -ncol(test__data)], y =train_data_rf$ptoutcome)
roc_imp
head(roc_imp)   # first 5

# plot variable importace 
plot(rf_importance, top = 54) 


#############    Looping for Backward Elimination    ########################

# Open and sort the data in ascending order, cuz i want to eliminte the less important first
sorted_data_rf <- read.csv("Variables_Important_RF.csv", header = TRUE, sep = ",") 
sorted_data_rf <- sorted_data_rf[order(sorted_data_rf$Overall),]

#y = colnames(train_data_rf) 
#z = colnames(test_data_rf)

x = sorted_data_rf$X #get the rows names of all variable

variable = c("all") #to store in roc list 
#result.roc_svm_lr$auc <- 0.8749119

# auc from the full model
roc_list = data.frame(variable,result.roc1_rf$auc) #list to store auc for all variables 

# For loop
for(i in c(1:54)){ #from col 1 to 54
  print(x[i])
  #if (x[i] == y[i]){ 
  # data 
  train_data_rf_sel = train_data_rf[, !(names(train_data_rf) %in% x[i])] #remove the col
  dim(train_data_rf_sel)
  #} 
  
  #if (x[i] == y[i]){ 
  test_data_rf_sel = test_data_rf[, !(names(test_data_rf) %in% x[i])] 
  dim(test_data_rf_sel)
  #} 
  
  set.seed(333)
  ctrl <- trainControl (method = "cv",
                        number = 10, 
                        classProbs = T,
                        summaryFunction=twoClassSummary,
                        search = 'grid')
                        
  
  set.seed(333) 
  model_rf <- train(ptoutcome~.,
                    data = train_data_rf_sel,
                    method = "rf",
                    ntree = 1000,
                    trControl = ctrl, 
                    metric = "ROC",
                    tuneLength = 10)
  
  #Confusion Matrix
  #cm_all_rf <- confusionMatrix (predict (model_rf, newdata = test_data_rf_sel),
  #                              test_data_rf_sel$ptoutcome)
  #cm_all_rf
  
  #Prediction
  result.predicted_rf <- predict(model_rf, newdata = test_data_rf_sel, type = "prob")
  #result.predicted_rf
  
  #Plot ROC ---- Nonsurvival
  result.roc_rf1 <- roc(test_data_rf_sel$ptoutcome, result.predicted_rf$Died,
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

# write.csv(roc_list, "List After Eliminating_RF_New_Part iV.csv")



#########  Plot the Graph  ##############################################

list_of_AUC <- read.csv("List After Eliminating_RF_New_Latest.csv", header = TRUE, sep = ",") 
dim(list_of_AUC)
list_of_AUC <- list_of_AUC[2:55,2:3]  # Remove the numbering and the variable all

x <- list_of_AUC[,1]
y <- list_of_AUC[,2]
names(y)=x

# par(mar=c(5.1,4.1,4.1,2.1))  # default
par(mar=c(8.5, 4, 2, 2))

plot(y, xaxt="n",   # xaxt -> remove x ticks marks 
     type="o", 
     col = "blue",
     ylab="AUC",
     xlab='',
     main="Random Forest (RF) ACS In Hospital")

axis(1, at=1:54, labels = names(y),las=2, cex.axis=0.65, font=1)
abline(h=0.889305633,col="red")

#legend("topright", legend="AUC = 0.8594",
#       col="red", cex=0.2, box.lty=0, text.font=4)

#locator(1)
#text(54, 0.85, "",col='red')

# fix the x-label 
mtext(text = "Variables",
      side = 1,#side 1 = bottom
      line = 6.3)

##########  Sorting the list for backward elimination  ####################

#list_of_AUC <- read.csv("List After Eliminating_LR.csv", header = TRUE, sep = ",") 
#dim(list_of_AUC)
#list_of_AUC <- list_of_AUC[2:55,2:3]
#list_of_AUC$result.roc_lr.auc
# Sorted in descending order 
#write.csv(sorted_list_of_AUC, "Sorted_list_of_AUC_LR_STEMI.csv")


#############################################################################
# sort the list in descending order
list_of_AUC <- read.csv("List After Eliminating_RF_New_Latest.csv", header = TRUE, sep = ",") 
dim(list_of_AUC)
list_of_AUC <- list_of_AUC[2:55,2:3] 
sorted_list_of_AUC<- list_of_AUC[order(-list_of_AUC$result.roc1_rf.auc),]

# Eliminate the variable one by one in the loop  
k <-  sorted_list_of_AUC$variable #get the rows names of all variable

variable = c("all") #to store in roc list 
#result.roc_stemi$auc <- 0.895360788532851

# auc from the full model
roc_list2 = data.frame(variable,result.roc1_rf$auc) #list to store auc for all variables 

# For loop
for(i in 1:54){ #from col 1 to 54
  print(i)
  my_list <- k[1:i]
  print(my_list)
  
  #if (x[i] == y[i]){ 
  # data 
  train_data_rf_sel = train_data_rf[, !(names(train_data_rf) %in% my_list)] #remove the col
  #train_data_rf_sel = train_data_rf[,j:54] 
  #dim(train_data_rf_sel)
  #} 
  
  #if (x[i] == y[i]){ 
  test_data_rf_sel = test_data_rf[, !(names(test_data_rf) %in% my_list)]
  #test_data_rf_sel = test_data_rf[, j:54] 
  #dim(test_data_rf_sel)
  #} 
  
  set.seed(333)
  ctrl <- trainControl (method = "cv",
                        number = 10, 
                        classProbs = T,
                        summaryFunction=twoClassSummary,
                        search = "grid")
  
                        
  set.seed(333) 
  model_rf_2 <- train(ptoutcome~.,
                      data = train_data_rf_sel,
                      method = "rf",
                      ntree = 1000,
                      trControl = ctrl, 
                      metric = "ROC",
                      tuneLength = 10)
  
  #Confusion Matrix
  #cm_all_rf <- confusionMatrix (predict (model_rf, newdata = test_data_rf_sel),
  #                              test_data_rf_sel$ptoutcome)
  #cm_all_rf
  
  #Prediction
  result.predicted_rf_2 <- predict(model_rf_2, newdata = test_data_rf_sel, type = "prob")
  #result.predicted_rf
  
  #Plot ROC ---- Nonsurvival
  result.roc2 <- roc(test_data_rf_sel$ptoutcome, result.predicted_rf_2$Died,
                     type = "prob")
  
  #plot(result.roc1, print.thres = "best", print.thres.best.method = "closest.topleft")
  result.roc2
  
  ## Plot roc 
  #sel_roc <- roc(as.vector(Y_test_label_sel), as.vector(sel_prob), positive = 0, type ="prob") 
  new_row = c(54-i,result.roc2$auc)
  roc_list2 = rbind(roc_list2,new_row)
}

roc_list2
write.csv(roc_list2, "Certain Numbers of Variables_RF_NEW_Latest_Part 5.csv")
