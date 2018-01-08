#args: x = ratings, y = group_ratings, z = items
#return: utility matrix W (user x feature)
init_utility <- function(x,y,z){
  users <- unique(y[order(y[,2]),2])
  num_users <- length(users)
  num_features <- dim(z)[2]
  WU <- matrix(0,num_users,num_features)
  for(uid in users){
    #getitem index rated by each user
    rated_iidx <- x[x[,1]==uid,2]
    rated_f <- z[rated_iidx,] # rated_f = rated features
    tmp <- x[x[,1]==uid,3]*rated_f
    numerator <- apply(tmp,2,sum)
    denominator <- apply(tmp>0,2,sum)
    denominator[denominator==0]<-1
    wu <-(numerator/denominator)
    wu <- wu/sum(wu)
    WU[uid,] <- wu
  }
  return (WU)
}