###########################################################################
#
# CREATING FUNCTIONS FOR CAJAMAR CHALLENGE
#
# Description: Some functions for creating new variables
# Author: Hugo Casero
# Date: February 2017
###########################################################################


# removes outliers per year, for each of the product families
removeOutliersPerFamily <- function(data)
{
  funis <- unique(data$family)
  clean <- data.frame()
  for(i in 1:length(funis))
  {
    # boxplot(fullseq$year[fullseq$Prod_Family == funis[i]])
    d <- data[data$family == funis[i],]
    iqr <- IQR(d$year)
    q1 <- quantile(d$year)["25%"]
    minOut <- q1-1.5*iqr
    d <- d[d$year > minOut,]
    clean <- rbind(clean, d)
  }
  plot(data$family, data$year)
  return(clean)
}

# removes outliers per year, for each of the products
removeOutliersPerCodProd <- function(data)
{
  funis <- unique(data$Cod_Prod)
  clean <- data.frame()
  for(i in 1:length(funis))
  {
    # boxplot(fullseq$year[fullseq$Prod_Family == funis[i]])
    d <- data[data$Cod_Prod == funis[i],]
    iqr <- IQR(d$year)
    q1 <- quantile(d$year)["25%"]
    minOut <- q1-1.5*iqr
    d <- d[d$year > minOut,]
    clean <- rbind(clean, d)
  }
  #plot(data$Cod_Prod, data$year)
  return(clean)
}

# selecciona devuelve los ?ndices de los ?ltimos productos adquiridos de cada usuario
getLastProductsL <- function(dataSeq)
{
  ptm <- proc.time()
  indexes <- nrow(dataSeq)-match(unique(dataSeq$ID_Customer),rev(dataSeq$ID_Customer))+1
  print(proc.time() - ptm)
  return(indexes)
}

# a?ade la variable familia
# esta variable se usar? posteriormente con fines ?nicamente de limpieza
# no ser? usada con fines predictivos
addFamilyVar <- function(data)
{
  pf <- data$Cod_Prod
  pf <- sprintf("%04d", as.numeric(as.character(pf)))
  ps <- substr(pf, 1, 2)
  ps <- as.factor(as.numeric(ps))
  data$family <- ps
  plot(data$family, data$year)
  return(data)
}

# a?ade secuenciaci?n a la adquisici?n de productos
# indica el orden de las diferentes transacciones para cada usuario
setSeqsL <- function(train)
{
  trorder <- train[order(train$ID_Customer, train$Cod_Fecha),]
  trorder$seq <- 1
  prevId <- -1
  ptm <- proc.time()
  for(i in 1:nrow(trorder))
  {
    if(i != 1 && prevId == trorder$ID_Customer[i]){
      trorder$seq[i] <- prevSeq+1
    }
    
    prevSeq <- trorder$seq[i]
    prevId <- trorder$ID_Customer[i]
  }
  print(proc.time() - ptm)
  return(trorder)
}