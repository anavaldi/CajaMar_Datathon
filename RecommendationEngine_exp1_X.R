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
rm(trainSet)
test <- testSet
rm(testSet)

# Check
nrow(train[train$ID_Customer %in% test$ID_Customer,]) == 0

## Recommendation System Engine

# Recommend by Socio_Demo Var 
ID_Customer_test <- unique(test$ID_Customer)

for(i in 1:length(ID_Customer_test)){
  print(i)
  # Take customer to predict
  data <- test[test$ID_Customer == ID_Customer_test[i],]
  contrProd <- unique(data$Cod_Prod)
  # Select same customers
  similarClients <- train[train$Socio_Demo_01 == data$Socio_Demo_01[1]
                             & train$Socio_Demo_02 == data$Socio_Demo_02[1]
                             & train$Socio_Demo_03 == data$Socio_Demo_03[1]
                             & train$Socio_Demo_04 == data$Socio_Demo_04[1]
                             & train$Socio_Demo_05 == data$Socio_Demo_05[1], ] 
 
  # Compute most contracted products of these customers
  mostProdContr <- as.matrix(table(similarClients$Cod_Prod))
  mostProdContr <- as.matrix(mostProdContr[!(rownames(mostProdContr) %in% contrProd),])
  mostProdContr <- as.data.frame(sort(mostProdContr[,1], decreasing = TRUE))
  similarClients <- unique(similarClients$ID_Clients)
  colnames(mostProdContr) <- c("times")

  if(i == 1){
    predValues <- data.frame(ID_Customer = ID_Customer_test[i], Cod_Prod_1 = rownames(mostProdContr)[1], 
                             Cod_Prod_2 = rownames(mostProdContr)[2], Cod_Prod_3 = rownames(mostProdContr)[3], 
                             Cod_Prod_4 = rownames(mostProdContr)[4], Cod_Prod_5 = rownames(mostProdContr)[5])
  }else{
    pred <- data.frame(ID_Customer = ID_Customer_test[i], Cod_Prod_1 = rownames(mostProdContr)[1], 
                       Cod_Prod_2 = rownames(mostProdContr)[2], Cod_Prod_3 = rownames(mostProdContr)[3], 
                       Cod_Prod_4 = rownames(mostProdContr)[4], Cod_Prod_5 = rownames(mostProdContr)[5])
    predValues <- rbind(predValues, pred)
  }
}

# Write solution

Test_Mission <- predValues[, c("ID_Customer", "Cod_Prod_1")]
setnames(predValues, old=c("Cod_Prod_1"), new=c("Cod_Prod"))

write.table(Test_Mission, "./predictions/Test_Mission.txt", row.names=FALSE, sep="|")


