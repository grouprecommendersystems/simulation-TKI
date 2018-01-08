# Sparse PCA: feature select + dimension reduction 
library(nsprcomp)
#args: feature matrix of items
select_feature <- function(items){
  set.seed(1)
  #nspc <- nsprcomp(items, k = 60, ncomp = 20, nneg = TRUE, scale. = TRUE)
  #(k=11,9) (11,7) 
  a <- nsprcomp(items, k = 11, ncomp = 7, nneg = TRUE, scale. = TRUE)
  #a <- nsprcomp(items, k = 90, ncomp = 90, nneg = TRUE, scale. = TRUE)
  b <- a$rotation[apply(a$rotation,1,sum)!=0,] # only keep rows which are not zeros
  items <- items[,which(apply(a$rotation,1,sum)!=0)]
  items <- items%*%b
  return (items)
}
#
library(elasticnet)
select_feature_02 <- function(items){
  K<-7
  a <- spca(items, K = K, type = "predictor", sparse = "varnum", para = rep(11,K))
  b <- a$loadings[apply(a$loadings,1,sum)!=0,] # only keep rows which are not zeros
  items <- items[,which(apply(a$loadings,1,sum)!=0)]
  items <- items%*%b
  return (items)
}

