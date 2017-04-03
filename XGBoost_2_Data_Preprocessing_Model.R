###########################################################################
#
# XGBOOST
#
# Description: XGBOOST Model for Cajamar. Version 3
# Author: Ana Valdivia
# Date: March 2017
###########################################################################


# Libraries
library(doParallel)
library(data.table)
library(xgboost)
library(zoo)
library(doSNOW)
options(mc.cores = detectCores())

# Load Data
path <- "./data/"
load(paste0(path, "fullseq2/fullseq2.Rdata"))

# Preparing dataset for the model
# Averaging SocioDemo variables
fullseq2$Age <- ifelse(fullseq2$Socio_Demo_01 == 1, (0+9)/2, NA)
fullseq2$Age <- ifelse(fullseq2$Socio_Demo_01 == 2, (18+30)/2, fullseq2$Age)
fullseq2$Age <- ifelse(fullseq2$Socio_Demo_01 == 3, (30+45)/2, fullseq2$Age)
fullseq2$Age <- ifelse(fullseq2$Socio_Demo_01 == 4, (45+65)/2, fullseq2$Age)
fullseq2$Age <- ifelse(fullseq2$Socio_Demo_01 == 5, (65+100)/2, fullseq2$Age)

fullseq2$Senior <- ifelse(fullseq2$Socio_Demo_02 == 1, (0+1)/2, NA)
fullseq2$Senior <- ifelse(fullseq2$Socio_Demo_02 == 2, (1+5)/2, fullseq2$Senior)
fullseq2$Senior <- ifelse(fullseq2$Socio_Demo_02 == 3, (5+10)/2, fullseq2$Senior)
fullseq2$Senior <- ifelse(fullseq2$Socio_Demo_02 == 4, (10+20)/2, fullseq2$Senior)
fullseq2$Senior <- ifelse(fullseq2$Socio_Demo_02 == 5, (20+50)/2, fullseq2$Senior)

fullseq2$Income <- ifelse(fullseq2$Socio_Demo_03 == 1, (0+6000)/2, NA)
fullseq2$Income <- ifelse(fullseq2$Socio_Demo_03 == 2, (6000+12000)/2, fullseq2$Income)
fullseq2$Income <- ifelse(fullseq2$Socio_Demo_03 == 3, (12000+24000)/2, fullseq2$Income)
fullseq2$Income <- ifelse(fullseq2$Socio_Demo_03 == 4, (24000+32000)/2, fullseq2$Income)
fullseq2$Income <- ifelse(fullseq2$Socio_Demo_03 == 5, (32000+50000)/2, fullseq2$Income)

fullseq2$Female <- ifelse(fullseq2$Socio_Demo_04 == 2, 1, 0)
fullseq2$Male <- ifelse(fullseq2$Socio_Demo_04 == 1, 1, 0)

fullseq2$Individual <- ifelse(fullseq2$Socio_Demo_05 == 0, 1, 0)
fullseq2$Farmer <- ifelse(fullseq2$Socio_Demo_05 == 1, 1, 0)
fullseq2$Business <- ifelse(fullseq2$Socio_Demo_05 == 2, 1, 0)
fullseq2$SelfEmployed <- ifelse(fullseq2$Socio_Demo_05 == 3, 1, 0)

fullseq2$YearNum <- as.numeric(as.yearmon(as.character(fullseq2$Cod_Fecha), "%Y-%m"))
# Difference between March 2017 and purchased product date
fullseq2$Diff_March2017 <- 2017.167 - fullseq2$YearNum
  
# Delete old
fullseq2$Socio_Demo_01 <- NULL
fullseq2$Socio_Demo_02 <- NULL
fullseq2$Socio_Demo_03 <- NULL
fullseq2$Socio_Demo_04 <- NULL
fullseq2$Socio_Demo_05 <- NULL
fullseq2$Cod_Fecha <- NULL
fullseq2$year <- NULL
fullseq2$month <- NULL

# Split train and test
trainSet <- fullseq2[fullseq2$class== "train", ]
testSet <- fullseq2[fullseq2$class== "test", ]
trainSet$class <- NULL
testSet$class <- NULL
# rm(fullseq2)

# Function that gets last product for each customer
getLastProductsL <- function(dataSeq){
  ptm <- proc.time()
  indexes <- nrow(dataSeq)-match(unique(dataSeq$ID_Customer),rev(dataSeq$ID_Customer))+1
  print(proc.time() - ptm)
  return(dataSeq[indexes,])
}

LastProducts <- getLastProductsL(trainSet)
LastProducts <- LastProducts[,c(1, 2, 3, 12)]

# Compute K-most-bought products
getLastProductsL <- function(dataSeq){
  ptm <- proc.time()
  indexes <- nrow(dataSeq)-match(unique(dataSeq$ID_Customer),rev(dataSeq$ID_Customer))+1
  print(proc.time() - ptm)
  return(dataSeq[indexes,])
}

LastProducts <- getLastProductsL(trainSet)
MostBoughtProducts <- sort(table(LastProducts[LastProducts$YearNum>=2014,]$Cod_Prod), decreasing=TRUE)
k <- length(MostBoughtProducts)
MostBoughtProducts_k <- MostBoughtProducts[1:k]

# Preparing dataset
newTest <- function(dataSeq){
  ptm <- proc.time()
  indexes <- nrow(dataSeq)-match(unique(dataSeq$ID_Customer),rev(dataSeq$ID_Customer))+1
  print(proc.time() - ptm)
  return(dataSeq[-indexes,])
}

# train set
LastProducts <- getLastProductsL(trainSet)
train <- newTest(trainSet)
tab <- table(train$ID_Customer, train$Cod_Prod)
rm(train)
tab <- as.data.frame.matrix(tab)
tab$ID_Customer <- rownames(tab)
train <- merge(LastProducts, tab, by = "ID_Customer")
train$Products <- do.call(paste0, c(train[, 15:ncol(train)]))
train$Products <- as.numeric(train$Products)
rm(tab)

# validation set
LastProducts <- getLastProductsL(testSet)
valid <- newTest(testSet)
tab <- table(valid$ID_Customer, valid$Cod_Prod)
rm(valid)
tab <- as.data.frame.matrix(tab)
tab$ID_Customer <- rownames(tab)
valid <- merge(LastProducts, tab, by = "ID_Customer")
valid$Products <- do.call(paste0, c(valid[, 15:ncol(valid)]))
valid$Products <- as.numeric(valid$Products)
rm(tab)

# test set
test <- testSet[!duplicated(testSet$ID_Customer),]
test$Cod_Prod <- NULL
test$seq <- test$seq+1
tab <- table(testSet$ID_Customer, testSet$Cod_Prod)
tab <- as.data.frame.matrix(tab)
tab$ID_Customer <- rownames(tab)
test <- merge(test, tab, by = "ID_Customer")
test$Products <- do.call(paste0, c(test[, 15:ncol(test)]))
test$Products <- as.numeric(test$Products)
rm(tab)

rm(fullseq2)
rm(trainSet)
rm(testSet)

# Building XGBoost for each product
for(i in 1:length(MostBoughtProducts_k)){
  # 71, 77, 79, 80, 81 products not in the model (all observed data is positive or negative)
  # select prod
  prod <- rownames(MostBoughtProducts_k)[i]
  print(paste0("iter: ", i, ", prod: ", prod))
  # build train, valid and test sets
  train_prd <- train
  train_prd$BoughtBefore <- train[,prod]
  train_prd$Pred <- ifelse(train$Cod_Prod == prod, 1, 0) 
  
  valid_prod <- valid
  valid_prod$BoughtBefore <- valid[, prod]
  valid_prod$Pred <- ifelse(valid$Cod_Prod ==  prod, 1, 0) 
  
  test_prod <- test
  test_prod$BoughtBefore <- test[, prod]
  test_prod$Pred <- NA
  
  # train_var <- as.matrix(train_601[,3:ncol(train_601)])
  train_var <- as.matrix(train_prd[,c(3:12,14, 109, 110)])
  train_label <- train_prd[,ncol(train_prd)]
  
  # valid_var <- as.matrix(valid_601[,3:ncol(valid_601)])
  valid_var <- as.matrix(valid_prod[,c(3:12,14, 108, 109)])
  valid_label <- valid_prod[,ncol(valid_prod)]
  
  test_var <- as.matrix(test_prod[,c(2:11, 13, 108, 109)])
  test_label <- test_prod[,ncol(test_prod)]
  
  dtrain <- xgb.DMatrix(data = train_var, label = train_label)
  dval <- xgb.DMatrix(data = valid_var, label = valid_label)
  dtest <- xgb.DMatrix(data = test_var, label = test_label)

  rm(train_var) 
  rm(valid_var)  
  rm(test_var)  
  # Train the model
  nrounds <- 1000
  early_stopping_round <- 50
  params <- list("eta"= 0.05,
                 "max_depth"= 4,
                 "min_child_weight" = 1,
                 "objective" = "binary:logistic",
                 "eval_metric"="auc")
  
  set.seed(0)
  model.xgb <- xgb.train(params=params,
                         data=dtrain,
                         nrounds=nrounds,
                         watchlist=list(train=dtrain, val=dval),
                         early_stopping_round=early_stopping_round,
                         print_every_n=10, 
                         base_score=mean(train_label))
  # Get predictions
  valid_result <- data.table(valid_prod$ID_Customer, valid_prod$BoughtBefore, predict(model.xgb, dval))
  test_result <- data.table(test_prod$ID_Customer, test_prod$BoughtBefore, predict(model.xgb, dtest))
  
  colnames(valid_result) <- c("ID_Customer", "BoughtBefore", "prob")
  colnames(test_result) <- c("ID_Customer", "BoughtBefore", "prob")
  
  valid_result$prob <- ifelse(valid_result$BoughtBefore == 1, 0, valid_result$prob)  
  test_result$prob <- ifelse(test_result$BoughtBefore == 1, 0, test_result$prob)
  
  valid_result$BoughtBefore  <- NULL
  test_result$BoughtBefore  <- NULL
  
  colnames(valid_result) <- c("ID_Customer", paste0("prob_", prod))
  colnames(test_result) <- c("ID_Customer", paste0("prob_", prod))
  
  if(i == 1){
    valid_result_ALLPROD <- valid_result
    test_result_ALLPROD <- test_result
    results <- data.frame(Product = prod, AUC = model.xgb$best_score)
  }else{
    valid_result_ALLPROD <- merge(valid_result_ALLPROD, valid_result, by = "ID_Customer")
    test_result_ALLPROD <- merge(test_result_ALLPROD, test_result, by = "ID_Customer")
    results_aux <- data.frame(Product = prod, AUC = model.xgb$best_score)
    results <- rbind(results, results_aux)
  }
  save(model.xgb, file=paste0("./models_v3/", "xgboost_", prod, ".rdata"))
  if( i %% 10 == 0){
    write.csv(valid_result_ALLPROD, "./predictions_v3/valid_prob_ALLPROD_aux.csv", quote=FALSE, row.names=FALSE)
    
    write.csv(test_result_ALLPROD, "./predictions_v3/test_prob_ALLPROD_aux.csv", quote=FALSE, row.names=FALSE)
  }
}

# Write results
write.csv(valid, "./predictions_v3/valid.csv", quote=FALSE, row.names=FALSE) 
write.csv(valid_result_ALLPROD, "./predictions_v3/valid_prob_ALLPROD.csv", quote=FALSE, row.names=FALSE)
 
write.csv(test, "./predictions_v3/test.csv", quote=FALSE, row.names=FALSE) 
write.csv(test_result_ALLPROD, "./predictions_v3/test_prob_ALLPROD.csv", quote=FALSE, row.names=FALSE)
  
write.csv(results, "./models_v3/AUCresults.csv", quote=FALSE, row.names=FALSE) 
