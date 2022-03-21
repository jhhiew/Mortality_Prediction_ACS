# DL_with SVM Selected Features 

setwd("D:/FYP/FYP_ACS/Full 4 Years Dataset with No Missing values/in_Hos/Latest 25 Jan/Using z-scores/DL_zscores")

library(keras)
library(tensorflow)
library(pROC)
library(caret)
library(dplyr)

tensorflow::tf$random$set_seed(104)

# Load the data
train_data <- read.csv("train_afterPreprocess_inHos_ACS_normalised dataset_z-scores.csv", header = TRUE, sep = ",") 
test_data <- read.csv("test_afterPreprocess_inHos_ACS_normalised dataset_z-scores.csv", header = TRUE, sep = ",")

train_data<- train_data[,2:56] 
test_data<- test_data[,2:56] 

dim(train_data)         # 9234 * 55
dim(test_data)          # 3956 * 55

#     0     1 
#  4583  4651 
table(train_data$ptoutcome)
#     0     1 
#   203  3753 
table(test_data$ptoutcome) 

# Selected Variables
# Train with 14 variables 
selected_features <- c("lipidla",
                       "ldlc","ecgabnormtypestelev1",
                       "cabg", "statin",
                       "cardiaccath","hdlc", "canginapast2wk", "oralhypogly","antiarr","heartrate","fbg",
                       "ptageatnotification","killipclass","ptoutcome") 

train_data = train_data[c(selected_features)] 
test_data = test_data[c(selected_features)]                  

dim(train_data)           # 9234 * x
dim(test_data)            # 3956 * x


# change the data to matrix
train_data_mat_all <- as.matrix(train_data)
test_data_mat_all <- as.matrix(test_data)


# Data partition, train-test split
X_train_all = train_data_mat_all[,-15]
X_test_all = test_data_mat_all[,-15]

Y_train_all = train_data_mat_all[,15]
Y_test_all = test_data_mat_all[,15]

# Label - Hot Label Encoder
Y_train_label_all <- to_categorical(Y_train_all)
Y_test_label_all <- to_categorical(Y_test_all)


########################################################################################
## Model Buiding

model <- keras_model_sequential()

model %>%
  layer_dense(units=14, activation = 'relu', input_shape = ncol(X_train_all)) %>%
  layer_dropout(rate = 0.2, set.seed(123)) %>%
  layer_dense(units=32, activation = 'relu')  %>%
  layer_dropout(rate = 0.2, set.seed(123)) %>%
  # layer_dense(units=32, activation = 'relu')  %>%
  # layer_dropout(rate = 0.2, set.seed(123)) %>%
  # layer_dense(units=16, activation = 'relu')  %>%
  # layer_dropout(rate = 0.2, set.seed(123)) %>%
  layer_dense(units=16, activation = 'relu')  %>%
  layer_dropout(rate = 0.2, set.seed(123)) %>%
  layer_dense(units=8, activation = 'relu')  %>%
  layer_dropout(rate = 0.2, set.seed(123)) %>%
  # layer_dense(units=4, activation = 'relu')  %>%
  # layer_dropout(rate = 0.2, set.seed(123)) %>%
  layer_dense(units=2, activation = 'sigmoid')

#optimizer
adagrad= optimizer_adagrad(lr = 0.01)

# Compile the model 
model %>% 
  compile(loss='binary_crossentropy',
          optimizer = adagrad,
          metrics = 'accuracy')

summary(model)


# Fit the model 
model %>%
  fit(X_train_all,
      Y_train_label_all,
      epoch = 200, 
      batch_size= 16,
      validation_split = 0.2)


model %>% 
  evaluate (X_test_all, Y_test_label_all)

# Model prediction
all_prob <- model %>%
  predict_proba(X_test_all)
# write.csv(all_prob,"DL_14var.csv")


prediction <- model%>%
  predict_classes(X_test_all)

all_roc <- roc(as.vector(Y_test_label_all), as.vector(all_prob), positive = 0, type ="prob") 
all_roc
ci.auc(as.vector(Y_test_label_all), as.vector(all_prob)) 

prediction_vector <- as.factor(prediction)
Y_test <- as.factor(Y_test_all)

cm <- confusionMatrix(data = prediction_vector, reference= Y_test) #positive = '1')
cm


################     TEST SEPERAETELY    #####################################################

# read the test data of stemi and nstemi respectively
test_stemi_data <- read.csv("STEMI_test_afterPreprocess_inHos_ACS_normalised dataset_z-scores.csv", header = TRUE, sep = ",") 
test_nstemi_data <- read.csv("NSTEMI_test_afterPreprocess_inHos_ACS_normalised dataset_z-scores.csv", header = TRUE, sep = ",")

dim(test_stemi_data)   #2322 * 56
dim(test_nstemi_data)  #1634 * 56

test_stemi_data <- test_stemi_data[,c(2:55,59)]    # delete the dummy column 1
test_nstemi_data <- test_nstemi_data[,c(2:55,59)]       # delete the dummy column 1

table(test_stemi_data$ptoutcome)       # 0-> 123, 1 -> 2199 = 2322
table(test_nstemi_data$ptoutcome)      # 0 -> 80, 1 -> 1554 = 1634

test_stemi_data = test_stemi_data[c(selected_features)] 
test_nstemi_data = test_nstemi_data[c(selected_features)] 

summary(test_stemi_data)
summary(test_nstemi_data)


# change the data to matrix
test_stemi_data_mat_all <- as.matrix(test_stemi_data)
test_nstemi_data_mat_all <- as.matrix(test_nstemi_data)


dim(test_stemi_data_mat_all)      # 2322 * 55
dim(test_nstemi_data_mat_all)     # 1634 * 55 


# Data partition, train-test split
X_test_stemi_all = test_stemi_data_mat_all[,-15]
X_test_nstemi_all = test_nstemi_data_mat_all[,-15]


Y_test_stemi_all = test_stemi_data_mat_all[,15]
Y_test_nstemi_all = test_nstemi_data_mat_all[,15]


# Label - Hot Label Encoder
Y_test_stemi_label_all <- to_categorical(Y_test_stemi_all)
Y_test_nstemi_label_all <- to_categorical(Y_test_nstemi_all)


##########################   STEMI   ############################################
model %>% 
  evaluate (X_test_stemi_all, Y_test_stemi_label_all)

# Model prediction
all_prob <- model %>%
  predict_proba(X_test_stemi_all)
# write.csv(all_prob,"DL_stemi_14var.csv")

prediction <- model%>%
  predict_classes(X_test_stemi_all)


## Plot roc 
all_roc <- roc(as.vector(Y_test_stemi_label_all), as.vector(all_prob), positive = 0, type ="prob") 
all_roc
ci.auc(as.vector(Y_test_stemi_label_all), as.vector(all_prob)) #95% CI: 0.95-0.9599 (DeLong)

prediction_vector <- as.factor(prediction)
Y_test <- as.factor(Y_test_stemi_all)

cm <- confusionMatrix(data = prediction_vector, reference= Y_test) #positive = '1')
cm

# AUC Graph 
is.vector(all_prob[,2])
pred <- as.vector(all_prob[,2])

preds <- prediction(pred, Y_test_stemi_all) # FOLLOW!!!!both parameters must be in vectors/list

roc <- performance(preds, "tpr", "fpr")

plot(roc,
     main="ROC Graph of Deep Learning ACS",
     xlab= '1-Specificity',
     ylab= 'Sensitivity',
     col = "purple", 
     lwd = 2.0)

#####################   NSTEMI  ########################################################
model %>% 
  evaluate (X_test_nstemi_all, Y_test_nstemi_label_all)

# Model prediction
all_prob <- model %>%
  predict_proba(X_test_nstemi_all)
# write.csv(all_prob,"DL_nstemi_14var.csv")

prediction <- model%>%
  predict_classes(X_test_nstemi_all)


## Plot roc 
all_roc <- roc(as.vector(Y_test_nstemi_label_all), as.vector(all_prob), positive = 0, type ="prob") 
all_roc
ci.auc(as.vector(Y_test_nstemi_label_all), as.vector(all_prob))   #95% CI: 0.9408-0.9539 (DeLong)

prediction_vector <- as.factor(prediction)
Y_test <- as.factor(Y_test_nstemi_all)

cm <- confusionMatrix(data = prediction_vector, reference= Y_test) #positive = '1')
cm




