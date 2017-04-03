###########################################################################
#
# XGBOOST
#
# Description: XGBOOST Model for Cajamar. Write valid & test results.
# Author: Ana Valdivia
# Date: March 2017
###########################################################################

# Libraries
library(doParallel)
library(data.table)
library(xgboost)
library(zoo)
options(mc.cores = detectCores())

# Read data
# valid_prob <- read.csv("./predictions/valid_prob_ALLPROD.csv")
# valid <- read.csv("./predictions/valid.csv")
# test_prob <- read.csv("./predictions/test_prob_ALLPROD.csv")
# test <- read.csv("./predictions/test.csv")
# 
# valid_prob <- read.csv("./predictions2/valid_prob_ALLPROD.csv")
# valid <- read.csv("./predictions2/valid.csv")
# test_prob <- read.csv("./predictions2/test_prob_ALLPROD.csv")
# test <- read.csv("./predictions2/test.csv")

valid_prob <- read.csv("./predictions_v3/valid_prob_ALLPROD.csv")
valid <- read.csv("./predictions_v3/valid.csv")
test_prob <- read.csv("./predictions_v3/test_prob_ALLPROD.csv")
test <- read.csv("./predictions_v3/test.csv")


# Valid Results
# 1 product
valid$Cod_Prod_Pred <- gsub("prob_", "", colnames(valid_prob[,-c(1)])[max.col(valid_prob[,-c(1)], ties.method="first")])
valid$dummy <- ifelse(valid$Cod_Prod == valid$Cod_Prod_Pred, 1, 0)
# accuracy
sum(valid$dummy)/nrow(valid)

valid2 <- valid[, c(1, 2, 107, 108)]  
write.csv(valid2, "./predictions_v3/TestMission_valid_1product.csv", quote=FALSE, row.names=FALSE)

# 5 products
k <- 5
DF <- valid_prob[,-c(1)]
mx <- t(apply(DF,1,function(x)names(DF)[head(order(x,decreasing=TRUE),k)]))
mx <- as.data.frame(mx)                            
mx <- as.data.frame(apply(mx, 2, function(x)gsub("prob_", "", x)))
colnames(mx) <- c("Cod_Prod_Pred_1", "Cod_Prod_Pred_2", "Cod_Prod_Pred_3", "Cod_Prod_Pred_4", "Cod_Prod_Pred_5")
valid <- cbind(valid, mx)
valid$dummy <- ifelse(valid$Cod_Prod == valid$Cod_Prod_Pred_1, 1, 0)
valid$dummy <- ifelse(valid$Cod_Prod == valid$Cod_Prod_Pred_2, 1, valid$dummy)
valid$dummy <- ifelse(valid$Cod_Prod == valid$Cod_Prod_Pred_3, 1, valid$dummy)
valid$dummy <- ifelse(valid$Cod_Prod == valid$Cod_Prod_Pred_4, 1, valid$dummy)
valid$dummy <- ifelse(valid$Cod_Prod == valid$Cod_Prod_Pred_5, 1, valid$dummy)
# accuracy
sum(valid$dummy)/nrow(valid)

valid25 <- valid[, c(1, 2, 109, 110, 111, 112, 113, 108)]
write.csv(valid25, "./predictions_v3/TestMission_valid_5products.csv", quote=FALSE, row.names=FALSE)

# TestResults
# 1 product
test$Cod_Prod_Pred <- gsub("prob_", "", colnames(test_prob[,-c(1)])[max.col(test_prob[,-c(1)], ties.method="first")])

Test_Mission <- data.frame(ID_Customer = test$ID_Customer, Cod_Prod = test$Cod_Prod_Pred)
write.table(Test_Mission, "./predictions_v3/Test_Mission.txt", row.names=FALSE, sep="|")

# 5 products
k <- 5
DF <- test_prob[,-c(1)]
mx <- t(apply(DF,1,function(x)names(DF)[head(order(x,decreasing=TRUE),k)]))
mx <- as.data.frame(mx)                            
mx <- as.data.frame(apply(mx, 2, function(x)gsub("prob_", "", x)))
colnames(mx) <- c("Cod_Prod_1", "Cod_Prod_2", "Cod_Prod_3", "Cod_Prod_4", "Cod_Prod_5")
test <- cbind(test, mx)

Test_Mission_5 <- test[,c(1, 110:114)]
write.table(Test_Mission_5, "./predictions_v3/Test_Mission_5_Products.txt", row.names=FALSE, sep="|")
