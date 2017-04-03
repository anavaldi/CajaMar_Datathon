###########################################################################
#
# CREATING FULLSEQ2
#
# Description: Building some new variables.
# Author: Hugo Casero
# Date: February 2017
###########################################################################

# lectura de datos
train <- read.csv("./data/orig/train2.txt", sep = "|", stringsAsFactors = FALSE)
test <- read.csv("./data/orig/test2.txt", sep = "|", stringsAsFactors = FALSE)

# crea las variables año y mes
train$year <- as.integer(substr(train$Cod_Fecha, start=1, stop=4))
test$year <- as.integer(substr(test$Cod_Fecha, start=1, stop=4))

train$month <- as.integer(substr(train$Cod_Fecha, start=6, stop=7))
test$month <- as.integer(substr(test$Cod_Fecha, start=6, stop=7))

# asigna una clase para indicar de donde vienen los datos dependiendo del archivo
# para posteriormente unir los datos en un gran dataset
train$class <- "train"
test$class <- "test"
full <- rbind(train, test)

# creaci?n variable secuencia
source("functions.R")
fullseq <- setSeqsL(full)

# creaci?n de variables dummy
require(dummies)
sd1 <- dummy(fullseq$Socio_Demo_01, sep="_")
sd2 <- dummy(fullseq$Socio_Demo_02, sep="_")
sd3 <- dummy(fullseq$Socio_Demo_03, sep="_")
sd4 <- dummy(fullseq$Socio_Demo_04, sep="_")
sd5 <- dummy(fullseq$Socio_Demo_05, sep="_")

fullseq <- cbind(fullseq, sd1, sd2, sd3, sd4, sd5)

write.csv(fullseq, "./data/fullseq2.csv", row.names=FALSE)
