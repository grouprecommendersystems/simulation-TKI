#input: item (feature matrix), BLD sets(ufeedback), num proposed items
infer_constraints <- function(fmat,ufeedback,num_prop){
  #number of constraints
  num_cons <- 0
  for(k in 1:2) {# B,L,N
    kp <- k + 1 #k' (k prime) points to Like 
    if(length(ufeedback[[kp]])==0 && kp==2){ #if Like is null
      kp <- k + 2 # then k' points to Dislike
    } 
    if(length(ufeedback[[kp]])==0 && kp==3)
      break
    num_cons <- num_cons + length(ufeedback[[k]])*length(ufeedback[[kp]])
  }
    
  
  num_feature <- dim(fmat)[2]
  u_constraint <- matrix(0,num_cons,num_feature)
  
  tmp <- 0
  for(k in 1:2){
    if(length(ufeedback[[k]])==0){
      next()
    }
    kp <- k + 1 
    if(length(ufeedback[[kp]])==0 && kp==2){
      kp <- k + 2
    } 
    if(length(ufeedback[[kp]])==0 && kp==3)
      break
    
    for(h in 1:length(ufeedback[[k]])){
      for(j in 1:length(ufeedback[[kp]])){
        tmp <- tmp +1
        u_constraint[tmp,] <- -fmat[ufeedback[[k]][h],] + fmat[ufeedback[[kp]][j],]
      }
    }
  }
  if(num_cons==0){
    return (matrix(0,1,num_feature))
  }
    
  else
    return (u_constraint)
}

#ufeedback <- u_feedback

