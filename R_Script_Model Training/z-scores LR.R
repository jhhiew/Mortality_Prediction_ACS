# Logistic Regression using z-scores 

# Machine Learning ACS inHos Logistic Regression (combine testing STEMI NSTEMI) 
# with 70:30 Data 
# with Z-scores  Normalization 
# With Looping for features selection 

setwd("D:/FYP/FYP_ACS/Full 4 Years Dataset with No Missing values/in_Hos/Latest 25 Jan/Using z-scores/LR_zscores")
library(readr)

# read the train data 
train_data_lr<- read.csv("train_afterPreprocess_inHos_ACS_normalised dataset_z-scores.csv", header = TRUE, sep = ",") 
dim(train_data_lr)
train_data_lr<- train_data_lr[,2:56]   # delete the dummy column 1


# read the test data 
test_data_lr <- read.csv("test_afterPreprocess_inHos_ACS_normalised dataset_z-scores.csv", header = TRUE, sep = ",") 
dim(test_data_lr)  # 3956 * 55
test_data_lr<- test_data_lr[,2:56] 

#    0    1 
# 4583 4651 
table(train_data_lr$ptoutcome)
#    0    1 
#  203 3753 
table(test_data_lr$ptoutcome)


# Convert all the outcome to factors 
train_data_lr$ptoutcome <- ifelse(train_data_lr$ptoutcome == 1, "Survive", "Died")
train_data_lr$ptoutcome <- as.factor(train_data_lr$ptoutcome)

test_data_lr$ptoutcome <- ifelse(test_data_lr$ptoutcome == 1, "Survive", "Died" )
test_data_lr$ptoutcome <- as.factor(test_data_lr$ptoutcome)
# test_data_lr$ptoutcome <- as.numeric(test_data_lr$ptoutcome) - 1


##################  Logistic regression   #########################################
###################################################################################


# method = glm specifies that we will fit a generalized linear model

library(caret)


set.seed(333)
ctrl <- trainControl(method = "cv",
                     number = 10,
                     classProbs = T, 
                     savePredictions = TRUE,
                     search = "grid")
                   
                     

set.seed(333)
model_lr <- train(ptoutcome ~., 
                  data=train_data_lr, 
                  method="glm", 
                  family="binomial",
                  trControl = ctrl,
                  tuneLength = 10)

model_lr


# save the model
# saveRDS(model_lr,"model_lr_NEW_1.rds")

# load the model
model_lr <- readRDS("model_lr_NEW_1.rds")

result.predicted <- predict(model_lr, newdata = test_data_lr , type="prob" ) # Prediction
result.predicted 

library(pROC)
library(ROCR)

pred <- prediction(result.predicted[,2], test_data_lr$ptoutcome)
eval <- performance(pred, "acc")
plot(eval)

roc <- performance(pred, "tpr", "fpr")
plot(roc)

# Default threshold -> 0.5
cm_all_lr <- confusionMatrix (predict(model_lr, newdata = test_data_lr), test_data_lr$ptoutcome)
cm_all_lr

threshold <- 0.772
pred <- factor(ifelse(result.predicted[,"Died"]>threshold,"Died","Survive"))
pred <- relevel(pred,"Died")
confusionMatrix(pred,test_data_lr$ptoutcome)


threshold <- 0.99
pred <- factor(ifelse(result.predicted[,"Survive"]>threshold,"Survive","Died"))
#pred <- relevel(pred,"Survive")
confusionMatrix(pred,test_data_lr$ptoutcome)



# Compute AUC for predicting Class with the model

result.roc_lr<- roc(test_data_lr$ptoutcome, result.predicted$Died, type="prob")
#result.roc <- roc(test_data_lr$ptoutcome, result.predicted[,1], type="prob")
plot(result.roc_lr, print.thres = "best", print.thres.best.method = "closest.topleft")
result.roc_lr



#######################        CHECKING            ############################
####################### For Train Data Itself #################################
result.predicted <- predict(model_lr, newdata = train_data_lr , type="prob" ) # Prediction

cm_all_lr <- confusionMatrix (predict(model_lr, newdata = train_data_lr), train_data_lr$ptoutcome)
cm_all_lr

result.roc_lr<- roc(train_data_lr$ptoutcome, result.predicted$Died, type="prob")
#result.roc <- roc(test_data_lr$ptoutcome, result.predicted[,1], type="prob")
plot(result.roc, print.thres = "best", print.thres.best.method = "closest.topleft")
result.roc_lr


##############     Variables Importance      ################################
# VarImportance of LR

lr_importance <- varImp(model_lr)
lr_importance
# write.csv(lr_importance[[1]], "Variables_Important_LR.csv")

# plot variable importace 
plot(lr_importance, top = 54)


#############    Looping for Backward Elimination    ########################

# Open and sort the data in ascending order, cuz i want to eliminte the less important first
sorted_data <- read.csv("Variables_Important_LR.csv", header = TRUE, sep = ",") 
sorted_data <- sorted_data[order(sorted_data$Overall),]

#y = colnames(train_data_lr) 
#z = colnames(test_data_lr)

x = sorted_data$X #get the rows names of all variable

variable = c("all") #to store in roc list 
#result.roc_lr$auc <- 0.8749119

# auc from the full model
roc_list = data.frame(variable,result.roc_lr$auc) #list to store auc for all variables 

# For loop
for(i in c(1:54)){ #from col 1 to 54
  print(x[i])
  #if (x[i] == y[i]){ 
  # data 
  train_data_lr_sel = train_data_lr[, !(names(train_data_lr) %in% x[i])] #remove the col
  dim(train_data_lr_sel)
  #} 
  
  #if (x[i] == y[i]){ 
  test_data_lr_sel = test_data_lr[, !(names(test_data_lr) %in% x[i])] 
  dim(test_data_lr_sel)
  #} 
  
  set.seed(333) 
  ctrl <- trainControl (method = "cv",
                        number = 10, 
                        classProbs = T,
                        savePrediction = T)
  
  #Train the model(original dataset)
  set.seed(333) 
  model_lr <- train(ptoutcome ~., 
                    data=train_data_lr_sel, 
                    method="glm", 
                    family="binomial",
                    trControl = ctrl)
            
  
  #Confusion Matrix
  #cm_all_rf <- confusionMatrix (predict (model_rf, newdata = test_data_lr_sel),
  #                              test_data_lr_sel$ptoutcome)
  #cm_all_rf
  
  #Prediction
  result.predicted_lr <- predict(model_lr, newdata = test_data_lr_sel, type = "prob")
  #result.predicted_rf
  
  #Plot ROC ---- Nonsurvival
  result.roc1 <- roc(test_data_lr_sel$ptoutcome, result.predicted_lr$Died,
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

# write.csv(roc_list, "List After Eliminating_LR.csv")



#########  Plot the Graph  ##############################################

list_of_AUC <- read.csv("List After Eliminating_LR.csv", header = TRUE, sep = ",") 
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
     main="Logistic Regression ACS In Hospital")

axis(1, at=1:54, labels = names(y),las=2, cex.axis=0.65, font=1)
abline(h=0.8911169914643,col="red")

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
list_of_AUC <- read.csv("List After Eliminating_LR.csv", header = TRUE, sep = ",") 
dim(list_of_AUC)
list_of_AUC <- list_of_AUC[2:55,2:3] 
sorted_list_of_AUC<- list_of_AUC[order(-list_of_AUC$result.roc_lr.auc),]

# Eliminate the variable one by one in the loop  
k <-  sorted_list_of_AUC$variable #get the rows names of all variable

variable = c("all") #to store in roc list 
#result.roc_stemi$auc <- 0.895360788532851

# auc from the full model
roc_list2 = data.frame(variable,result.roc_lr$auc) #list to store auc for all variables 

# For loop
for(i in 1:54){ #from col 1 to 54
  print(i)
  my_list <- k[1:i]
  print(my_list)
  
  #if (x[i] == y[i]){ 
  # data 
  train_data_lr_sel = train_data_lr[, !(names(train_data_lr) %in% my_list)] #remove the col
  #train_data_lr_sel = train_data_lr[,j:54] 
  #dim(train_data_lr_sel)
  #} 
  
  #if (x[i] == y[i]){ 
  test_data_lr_sel = test_data_lr[, !(names(test_data_lr) %in% my_list)]
  #test_data_lr_sel = test_data_lr[, j:54] 
  #dim(test_data_lr_sel)
  #} 
  
  set.seed(333) 
  ctrl <- trainControl (method = "cv",number = 10, classProbs = T,savePrediction = T)
  
  #Train the model(original dataset)
  set.seed(333) 
  model_lr_2 <- train(ptoutcome ~., 
                      data=train_data_lr_sel, 
                      method="glm", 
                      family="binomial",
                      trControl = ctrl, 
                      tuneLength = 10)
  
  #Confusion Matrix
  #cm_all_rf <- confusionMatrix (predict (model_rf, newdata = test_data_lr_sel),
  #                              test_data_lr_sel$ptoutcome)
  #cm_all_rf
  
  #Prediction
  result.predicted_lr_2 <- predict(model_lr_2, newdata = test_data_lr_sel, type = "prob")
  #result.predicted_rf
  
  #Plot ROC ---- Nonsurvival
  result.roc2 <- roc(test_data_lr_sel$ptoutcome, result.predicted_lr_2$Died,
                     type = "prob")
  
  #plot(result.roc1, print.thres = "best", print.thres.best.method = "closest.topleft")
  result.roc2
  
  ## Plot roc 
  #sel_roc <- roc(as.vector(Y_test_label_sel), as.vector(sel_prob), positive = 0, type ="prob") 
  new_row = c(54-i,result.roc2$auc)
  roc_list2 = rbind(roc_list2,new_row)
}

roc_list2
# write.csv(roc_list2, "Certain Numbers of Variables_LR.csv")