# SVM using z-scores 

# SVM
# Machine Learning ACS inHos Logistic Regression (combine testing STEMI NSTEMI) 
# with 70:30 Data 
# with Z-scores Normalization 
# With Looping for features selection 

setwd("D:/FYP/FYP_ACS/Full 4 Years Dataset with No Missing values/in_Hos/Latest 25 Jan/Using z-scores/SVM_zscores")

library(readr)

# read the train data 
train_data_svm<- read.csv("train_afterPreprocess_inHos_ACS_normalised dataset_z-scores.csv", header = TRUE, sep = ",") 
dim(train_data_svm)
train_data_svm<- train_data_svm[,2:56]   # delete the dummy column 1


# read the test data 
test_data_svm <- read.csv("test_afterPreprocess_inHos_ACS_normalised dataset_z-scores.csv", header = TRUE, sep = ",") 
dim(test_data_svm)  # 3956 * 55
test_data_svm <- test_data_svm[,2:56]

#    0    1 
# 4583 4651 
table(train_data_svm$ptoutcome)
#    0    1 
#  203 3753 
table(test_data_svm$ptoutcome)

# Convert all the outcome to factors 
train_data_svm$ptoutcome <- ifelse(train_data_svm$ptoutcome == 1, "Survive", "Died")
train_data_svm$ptoutcome <- as.factor(train_data_svm$ptoutcome)

test_data_svm$ptoutcome <- ifelse(test_data_svm$ptoutcome == 1, "Survive", "Died" )
test_data_svm$ptoutcome <- as.factor(test_data_svm$ptoutcome)
# test_data_svm$ptoutcome <- as.numeric(test_data_svm$ptoutcome) - 1


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
                      # search = "grid"


set.seed(333)

#Train SVM
# C= 1.368421.
model_svm <- train(form = ptoutcome~.,
                   data = train_data_svm,
                   method = "svmLinear",
                   metric = "ROC",
                   # tuneGrid = expand.grid(C = seq(0, 2, length = 20)),
                   trControl = ctrl,
                   tuneLength = 10)

model_svm

# save the model to disk
saveRDS(model_svm,"model_svm_withPoly_tuneLen5.rds")

# load the model
model_svm <- readRDS("model_svm_NEW_1a.rds")


result.predicted <- predict(model_svm, newdata = test_data_svm , type="prob" ) # Prediction
result.predicted 

# Default threshold -> 0.5
cm_all_svm <- confusionMatrix (predict(model_svm, newdata = test_data_svm), test_data_svm$ptoutcome)
cm_all_svm

library(pROC)
library(ROCR)

result.roc_svm <- roc(test_data_svm$ptoutcome, result.predicted$Died, type="prob")
plot(result.roc_svm, print.thres = "best", print.thres.best.method = "closest.topleft")
result.roc_svm


#######################        CHECKING            ############################
####################### For Train Data Itself #################################
result.predicted <- predict(model_svm, newdata = train_data_svm_svm, type="prob" ) # Prediction

cm_all_svm <- confusionMatrix (predict(model_svm, newdata = train_data_svm), train_data_svm$ptoutcome)
cm_all_svm

result.roc_svm<- roc(train_data_svm$ptoutcome, result.predicted$Died, type="prob")
#result.roc <- roc(test_data_svm$ptoutcome, result.predicted[,1], type="prob")
plot(result.roc_svm, print.thres = "best", print.thres.best.method = "closest.topleft")
result.roc_svm


##############     Variables Importance      ################################
# VarImportance of SVM

svm_importance <- varImp(model_svm)
svm_importance
#class(svm_importance)
#write.csv(svm_importance[[1]], "Variables_Important_SVM.csv")

# plot variable importace 
plot(svm_importance, top = 54)


#############    Looping for Backward Elimination    ########################

# Open and sort the data in ascending order, cuz i want to eliminte the less important first
sorted_data_svm <- read.csv("Variables_Important_SVM.csv", header = TRUE, sep = ",") 
sorted_data_svm <- sorted_data_svm[order(sorted_data_svm$Died),]

#y = colnames(train_data_svm) 
#z = colnames(test_data_svm)

x = sorted_data_svm$X #get the rows names of all variable

variable = c("all") #to store in roc list 
#result.roc_svm_lr$auc <- 0.869238927413078

# auc from the full model
roc_list = data.frame(variable,result.roc_svm$auc) #list to store auc for all variables 

# For loop
for(i in c(1:54)){ #from col 1 to 54
  print(x[i])
  #if (x[i] == y[i]){ 
  # data  
  train_data_svm_sel = train_data_svm[, !(names(train_data_svm) %in% x[i])] #remove the col
  dim(train_data_svm_sel)
  #} 
  
  #if (x[i] == y[i]){ 
  test_data_svm_sel = test_data_svm[, !(names(test_data_svm) %in% x[i])] 
  dim(test_data_svm_sel)
  #} 
  
  set.seed(333)
  ctrl <- trainControl (method = "cv",
                        number = 10, 
                        classProbs = T,
                        summaryFunction=twoClassSummary)
                        
  
  #Train the model(original dataset)
  set.seed(333)
  model_svm <- train(form = ptoutcome~.,
                     data = train_data_svm_sel,
                     method = "svmLinear",
                     metric = "ROC",
                     trControl = ctrl)
  
  #Confusion Matrix
  #cm_all_rf <- confusionMatrix (predict (model_rf, newdata = test_data_svm_sel),
  #                              test_data_svm_sel$ptoutcome)
  #cm_all_rf
  
  #Prediction
  result.predicted_svm <- predict(model_svm, newdata = test_data_svm_sel, type = "prob")
  #result.predicted_rf
  
  #Plot ROC ---- Nonsurvival
  result.roc_svm1 <- roc(test_data_svm_sel$ptoutcome, result.predicted_svm$Died,
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

write.csv(roc_list, "List After Eliminating_SVM_Part_1.csv")



#########  Plot the Graph  ##############################################

list_of_AUC <- read.csv("List After Eliminating_SVM.csv", header = TRUE, sep = ",") 
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
     main="SVM ACS In Hospital")

axis(1, at=1:54, labels = names(y),las=2, cex.axis=0.65, font=1)
abline(h=0.891590832,col="red")

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
# sort th list in descending order
list_of_AUC <- read.csv("List After Eliminating_SVM.csv", header = TRUE, sep = ",") 
dim(list_of_AUC)
list_of_AUC <- list_of_AUC[2:55,2:3] 
sorted_list_of_AUC<- list_of_AUC[order(-list_of_AUC$result.roc_svm.auc),]

# Eliminate the variable one by one in the loop  
k <-  sorted_list_of_AUC$variable #get the rows names of all variable

variable = c("all") #to store in roc list 
#result.roc_stemi$auc <- 0.895360b788532851

# auc from the full model
roc_list2 = data.frame(variable,result.roc_svm$auc) #list to store auc for all variables 

# For loop
for(i in 1:54){ #from col 1 to 54
  print(i)
  my_list <- k[1:i]
  print(my_list)
  
  #if (x[i] == y[i]){ 
  # data 
  train_data_svm_sel = train_data_svm[, !(names(train_data_svm) %in% my_list)] #remove the col
  #train_data_svm_sel = train_data_svm[,j:54] 
  #dim(train_data_svm_sel)
  #} 
  
  #if (x[i] == y[i]){ 
  test_data_svm_sel = test_data_svm[, !(names(test_data_svm) %in% my_list)]
  #test_data_svm_sel = test_data_svm[, j:54] 
  #dim(test_data_svm_sel)
  #} 
  
  set.seed(333)
  ctrl <- trainControl (method = "cv",
                        number = 10, 
                        classProbs = T,
                        summaryFunction=twoClassSummary) 
                        
  
  #Train the model(original dataset)
  set.seed(333)
  model_svm_2 <- train(ptoutcome~.,
                       train_data_svm_sel,
                       method = "svmLinear",
                       metric = "ROC",
                       #tuneGrid = grid,
                       #preProcess = c("scale","center"),
                       trControl = ctrl)
  
  #Confusion Matrix 
  #cm_all_rf <- confusionMatrix (predict (model_rf, newdata = test_data_svm_sel),
  #                              test_data_svm_sel$ptoutcome)
  #cm_all_rf
  
  #Prediction
  result.predicted_svm_2 <- predict(model_svm_2, newdata = test_data_svm_sel, type = "prob")
  #result.predicted_rf
  
  #Plot ROC ---- Nonsurvival
  result.roc2 <- roc(test_data_svm_sel$ptoutcome, result.predicted_svm_2$Died,
                     type = "prob")
  
  #plot(result.roc1, print.thres = "best", print.thres.best.method = "closest.topleft")
  result.roc2
  
  ## Plot roc 
  #sel_roc <- roc(as.vector(Y_test_label_sel), as.vector(sel_prob), positive = 0, type ="prob") 
  new_row = c(54-i,result.roc2$auc)
  roc_list2 = rbind(roc_list2,new_row)
}

roc_list2
# roc_list2 = read.csv("Certain Numbers of Variables_SVM_2.csv", header = TRUE, sep = ",")
# write.csv(roc_list2, "Certain Numbers of Variables_SVM_3.csv")