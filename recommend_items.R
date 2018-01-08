#input: group, num_prop (proposed items), item-feature matrix, inferred_constraints, WU_init 

# group <- current_group
# num_prop <- num_prop_items
# fmat <- items
# infer_cons <- inferred_constraints
# user_profiles <- WU_init
library(cccp)
recommend_items <- function(group, num_prop, fmat, infer_cons, user_profiles){
  #fmat <- items
  fmat <- as.matrix(fmat)
  num_row <- dim(fmat)[1]
  num_col <- dim(fmat)[2]
  y <- seq(num_row)
  num_mem <- length(group)
  WUG <- matrix(0,num_mem,num_col)
  
  #solve optimization problem
  for(uidx in 1:num_mem){
    u <- group[uidx]
    wu_init <- user_profiles[u,]
    WUG[uidx,] <- solve_optimization_CCCP(num_prop,wu_init,infer_cons[[uidx]])
    user_profiles[u,] <- WUG[uidx,]
  }
  
  wg_new <- apply(WUG,2,mean)
  score <- fmat%*%wg_new
  res <- list(user_profiles, wg_new)
  names(res) <- c("WU_updated","wg")
  return (res)
}

solve_optimization_CCCP <- function(num_prop, wg, infer_cons){
  #inferredConstraints: matrix of constraints of 1 user
  
  # solve linear programming in form: max wg*w st. G*w <= h
  d <- length(wg) #d: dimension = #features
  #G <- rbind(inferredConstraints,-diag(d)) # constraints matrix
  G <- infer_cons
  h <- c(rep(0,nrow(G))) #h <- c(rep(0.001,nrow(G)))
  
  
  Fx <- diag(d)
  g <- c(rep(0,d))
  dx <- c(rep(0,d))
  firstConstraint <- nnoc(G,h)
  secConstraint <- socc(Fx,g,dx,1)
  #print(G)
  result <- cccp(q=-wg,cList = list(firstConstraint,secConstraint),optctrl = ctrl(trace=FALSE))
  solution <- c(result$pdv$x)/sum(c(result$pdv$x))
  
  if(length(solution[solution<0]) >0){
   cat("negative sol: ",solution[solution<0],fill = TRUE)
    solution <- wg
  } 
  return(solution)
}