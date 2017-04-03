###########################################################################
#
# RECOMMENDATION ENGINE
#
# Description: Recommendation engine for Cajamar 
# Author: Ana Valdivia
# Date: February 2017
###########################################################################

# Libraries
library(doParallel)
library(data.table)
options(mc.cores = detectCores())

# Load Data
path <- "./data/"
load(paste0(path, "fullseq2/fullseq2.Rdata"))
trainSet <- fullseq2[fullseq2$class== "train", ]
testSet <- fullseq2[fullseq2$class== "test", ]
rm(fullseq2)

train <- trainSet
test <- testSet
rm(trainSet)
rm(testSet)

# Check
nrow(train[train$ID_Customer %in% test$ID_Customer,]) == 0

# Recommend by Socio_Demo Var + (Bought Products)
ID_Customer_test <- unique(test$ID_Customer)
# ID_Customer_test <- sample(ID_Customer_test, size = 100)

for(i in 1:length(ID_Customer_test)){
  print(i)
  predCustomer <- test[test$ID_Customer == ID_Customer_test[i],]
  contrProd <- unique(predCustomer$Cod_Prod)
  seqPred <- predCustomer[nrow(predCustomer),]$seq + 1
  
  # Select similar clients
  similarSocioDemo <- train[train$Socio_Demo_01 == predCustomer$Socio_Demo_01[1]
                            & train$Socio_Demo_02 == predCustomer$Socio_Demo_02[1]
                            & train$Socio_Demo_03 == predCustomer$Socio_Demo_03[1]
                            & train$Socio_Demo_04 == predCustomer$Socio_Demo_04[1]
                            & train$Socio_Demo_05 == predCustomer$Socio_Demo_05[1], ] 


  if(length(contrProd) < 7 ){
    similarClients <- unique(similarSocioDemo$ID_Customer)
    similarClients_sameProducts <- as.matrix(table(similarSocioDemo$ID_Customer, similarSocioDemo$Cod_Prod))
    similarClients_sameProducts <- similarClients_sameProducts[rownames(similarClients_sameProducts) %in% similarClients, ]

    # Select the last five contracted products
    if(length(contrProd) <= 5){
      for(j in 1:length(contrProd)){
        similarClients_sameProducts_aux <- similarClients_sameProducts[similarClients_sameProducts[, paste0(contrProd[j])] == 1, ]
        if(is.null(dim(similarClients_sameProducts_aux))){
          similarClients_sameProducts <- similarClients_sameProducts
        }else{
          similarClients_sameProducts <- similarClients_sameProducts_aux
        }
      }
    }else{
      for(j in (length(contrProd)-4):length(contrProd)){
        similarClients_sameProducts_aux <- similarClients_sameProducts[similarClients_sameProducts[, paste0(contrProd[j])] == 1,]
        if(is.null(dim(similarClients_sameProducts_aux))){
          similarClients_sameProducts <- similarClients_sameProducts
        }else{
          similarClients_sameProducts <- similarClients_sameProducts_aux
        }
      }
    }
    
    # similarSocioDemoSeq <- similarSocioDemo[(similarSocioDemo$ID_Customer %in% rownames(similarClients_sameProducts)) & ( similarSocioDemo$seq == seqPred), ]
    similarSocioDemoSeq <- similarSocioDemo[(similarSocioDemo$ID_Customer %in% rownames(similarClients_sameProducts)), ]
    if(nrow(similarSocioDemoSeq) < 10){
      similarSocioDemoSeq <- similarSocioDemo[(similarSocioDemo$ID_Customer %in% rownames(similarClients_sameProducts)), ]
    }
    similarSocioDemoSeq <- similarSocioDemo 
    prob <- as.matrix(sort(table(similarSocioDemoSeq$Cod_Prod), decreasing = TRUE))
    prob <- as.matrix(prob[!(rownames(prob) %in% contrProd),])
    if(nrow(similarSocioDemoSeq) < 10){
      similarSocioDemoSeq <- similarSocioDemo
    }
    prob <- as.matrix(sort(table(similarSocioDemoSeq$Cod_Prod), decreasing = TRUE))
    prob <- as.matrix(prob[!(rownames(prob) %in% contrProd),])
  }else{
    similarSocioDemoSeq <- similarSocioDemo[similarSocioDemo$seq > (seqPred-3), ]
    prob <- as.matrix(sort(table(similarSocioDemoSeq$Cod_Prod), decreasing = TRUE))
    prob <- as.matrix(prob[!(rownames(prob) %in% contrProd),])
  }

  # If last years...
  if(substr(LastProducts[LastProducts$ID_Customer == ID_Customer_test[i],]$Cod_Fecha,1, 4) == "2015"){
    similarSocioDemoSeq <- similarSocioDemoSeq[similarSocioDemoSeq$year >= 2013, ]
    prob <- as.matrix(sort(table(similarSocioDemoSeq$Cod_Prod), decreasing = TRUE))
    prob <- as.matrix(prob[!(rownames(prob) %in% contrProd),])
  }
  if(substr(LastProducts[LastProducts$ID_Customer == ID_Customer_test[i],]$Cod_Fecha,1, 4) == "2016"){
    similarSocioDemoSeq <- similarSocioDemoSeq[similarSocioDemoSeq$year >= 2014, ]
    prob <- as.matrix(sort(table(similarSocioDemoSeq$Cod_Prod), decreasing = TRUE))
    prob <- as.matrix(prob[!(rownames(prob) %in% contrProd),])
  }

  if(i == 1){
    predValues <- data.frame(ID_Customer = ID_Customer_test[i], Cod_Prod_1 = rownames(prob)[1], 
                             Cod_Prod_2 = rownames(prob)[2], Cod_Prod_3 = rownames(prob)[3], 
                             Cod_Prod_4 = rownames(prob)[4], Cod_Prod_5 = rownames(prob)[5])
  }else{
    pred <- data.frame(ID_Customer = ID_Customer_test[i], Cod_Prod_1 = rownames(prob)[1], 
                       Cod_Prod_2 = rownames(prob)[2], Cod_Prod_3 = rownames(prob)[3], 
                       Cod_Prod_4 = rownames(prob)[4], Cod_Prod_5 = rownames(prob)[5])
    predValues <- rbind(predValues, pred)
  }
}

# Write solution

Test_Mission <- predValues[, c("ID_Customer", "Cod_Prod_1")]
setnames(predValues, old=c("Cod_Prod_1"), new=c("Cod_Prod"))

write.table(Test_Mission, "./predictions/Test_Mission.txt", row.names=FALSE, sep="|")