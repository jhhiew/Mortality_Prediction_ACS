# Data Normalisation using z-scores 26 Jan 2021

setwd("D:/FYP/FYP_ACS/Full 4 Years Dataset with No Missing values/in_Hos/Latest 25 Jan/Using z-scores")

library(keras)
library(tensorflow)
tensorflow::tf$random$set_seed(104)
library(mlbench)
library(pROC)
library(caret)
library(dplyr)


#############################     Training Dataset   #############################################
# Load the data
train_data <- read.csv("train_acs_inHos.csv", header = TRUE, sep = ",") 

str(train_data)
summary(train_data)
dim(train_data)         

train_data <- train_data[,2:56]    # 9234 * 55

# Check for output balancing
#     0     1 
#  4583  4651 
table(train_data$ptoutcome)


# Data conversion
## Outcome
train_data$ptoutcome <- ifelse(train_data$ptoutcome == 1, 1, 0)
train_data$ptoutcome <- as.factor(train_data$ptoutcome)
train_data$ptoutcome <- as.numeric(train_data$ptoutcome) - 1


# Categorical Variables -> convert to 0 and 1
selected_col <- c( "ptsex", "cdys", "cdm","chpt",	"cpremcvd","cmi",
                   "ccap"	,"canginamt2wk", "canginapast2wk", "cheartfail",
                   "clung"	,	"crenal","ccerebrovascular","cpvascular",
                   "ecgabnormtypestelev1","ecgabnormtypestelev2",	
                   "ecgabnormtypestdep",	"ecgabnormtypetwave",
                   "ecgabnormtypebbb","ecgabnormlocationil",
                   "ecgabnormlocational"	,"ecgabnormlocationll",	
                   "ecgabnormlocationtp"	, "ecgabnormlocationrv",	
                   "cardiaccath","pci", "cabg", "asa",	"gpri",	
                   "heparin","lmwh","bb","acei","arb","statin",
                   "lipidla",	"diuretic",	"calcantagonist"	,		
                   "oralhypogly", "insulin","antiarr")


# Remain the Killip Class as it is 
train_data$ptrace <- as.numeric(train_data$ptrace)-1
train_data$smokingstatus <- as.numeric(train_data$smokingstatus)-1
train_data$killipclass<- as.numeric(train_data$killipclass)
train_data[,selected_col] <- ifelse(train_data[,selected_col] == 1, 0, 1)


str(train_data)


###########################   Testing dataset     #########################################
test_data <- read.csv("test_acs_inHos.csv", header = TRUE, sep = ",")
dim(test_data)                         # 3956 * 59 
# test_data <- test_data[,2:59] 
test_data <- test_data[,c(2:55,59)]    # 3956 * 55 

#     0    1 
#   203 3753 
table(test_data$ptoutcome) 


test_data$ptoutcome <- ifelse(test_data$ptoutcome == 1, 1, 0)
test_data$ptoutcome <- as.factor(test_data$ptoutcome)
test_data$ptoutcome <- as.numeric(test_data$ptoutcome) - 1 
#Y = test_data$ptoutcome #non ohe outcome


# Categorical Variables -> convert to 0 and 1
selected_col <- c( "ptsex", "cdys", "cdm","chpt",	"cpremcvd","cmi",
                   "ccap"	,"canginamt2wk", "canginapast2wk", "cheartfail",
                   "clung"	,	"crenal","ccerebrovascular","cpvascular",
                   "ecgabnormtypestelev1","ecgabnormtypestelev2",	
                   "ecgabnormtypestdep",	"ecgabnormtypetwave",
                   "ecgabnormtypebbb","ecgabnormlocationil",
                   "ecgabnormlocational"	,"ecgabnormlocationll",	
                   "ecgabnormlocationtp"	, "ecgabnormlocationrv",	
                   "cardiaccath","pci", "cabg", "asa",	"gpri",	
                   "heparin","lmwh","bb","acei","arb","statin",
                   "lipidla",	"diuretic",	"calcantagonist"	,		
                   "oralhypogly", "insulin","antiarr")


test_data$ptrace <- as.numeric(test_data$ptrace)-1
test_data$smokingstatus <- as.numeric(test_data$smokingstatus)-1
test_data$killipclass<- as.numeric(test_data$killipclass)
test_data[,selected_col] <- ifelse(test_data[,selected_col] == 1, 0, 1)


#########################         Z-scores Normalization     #############################

con_var <- c("ptageatnotification", "heartrate","bpsys", "bpdias", "ck","tc", "hdlc", "ldlc",
             "tg","fbg")
zscores_Norm <- preProcess(train_data[,con_var], method=c("center", "scale"))

train_data[,con_var] <- predict(zscores_Norm, train_data[,con_var])
test_data[,con_var] <- predict(zscores_Norm, test_data[,con_var])


#write.csv(train_data,"train_afterPreprocess_inHos_ACS_normalised dataset_z-scores.csv")
#write.csv(test_data,"test_afterPreprocess_inHos_ACS_normalised dataset_z-scores.csv")
#write.csv(test_data,"test_afterPreprocess_inHos_ACS_normalised dataset_z-scores_with TIMI Scores.csv")
