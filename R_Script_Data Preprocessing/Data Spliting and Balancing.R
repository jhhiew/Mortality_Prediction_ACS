# Data Preprocessing 1
# Data Partitioning 
# Data Balancing 

setwd("D:/FYP/FYP_ACS/Full 4 Years Dataset with No Missing values/in_Hos/Latest Models")

set.seed(123)
library (readr)


# Load the data 
data<- read.csv("Full dataset- inHos.csv", header = TRUE, sep = ",")


# Remove patientid
data <- data[,2:59]
dim(data)     # 13190 * 58


#####################   Data Partitioning      ################################################

# Separate into training data and testing data
library(mlbench)
library(caret)
library(doParallel)

set.seed(79)
train <- createDataPartition(data$ptoutcome, p=0.7,list=FALSE)
train_data <- data[train, ]
test_data <- data[-train, ]

# Remove accstratum(55), timiscorestemi(56) and timiscorenstemi(57)  for training dataset
train_data <- train_data[,c(1:54,58)]
dim(train_data)    # 9234 * 55

# Imbalaced Datasets for training
#   0    1 
# 475 8759 
table(train_data$ptoutcome)


# Check for testing datasets 
table(test_data$ptoutcome)     # 0 -> 203, 1 -> 3753
table(test_data$acsstratum)    # STEMI(1)= 2322  , NSTEMI(2) = 1634

#parallel processing
registerDoParallel()

#export the train data
# write.csv(train_data,"ori_train_acs_inHos.csv")



#######################  Data Balancing   ###############################################
#ROSE 
library(ROSE)

train_data <- read.csv("ori_train_acs_inHos.csv",header = T, sep =  ",")
dim(train_data)       # 9234 * 56 
summary(train_data)  
train_data <- train_data[,2:56]
data_balanced_both <- ovun.sample(ptoutcome~., data=train_data, method = "both", p=0.5, N=9234,seed=1)$data#[,c(1:55,59)]

#   0    1 
#4583 4651 
table(data_balanced_both$ptoutcome)
#   0    1 
# 475 8759 
table(train_data$ptoutcome)
summary(data_balanced_both)

#write prodicted outcome in excel file
#write.csv(test_data,"test_acs_inHos.csv")
#write.csv(data_balanced_both,"train_acs_inHos.csv")