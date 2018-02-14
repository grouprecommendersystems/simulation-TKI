compute_loss_avg <- function(group, umat, gchoice){
  personal <- group %>%
    map_dbl(function(x){
      iidx_sorted <- order(umat[x,],decreasing = TRUE)
      umat[x,iidx_sorted[1]]
    })
  res <- abs(personal-umat[group,gchoice])
  #res <- personal-umat[group,gchoice]
  return (mean(res))
}

#compare to the new preference in group
compute_loss_avg_in_group <- function(group, umat, gchoice, fmat, WUG){
  mat <- as.matrix(WUG)%*%t(fmat)
  personal_in_group <- group %>%
    map_dbl(function(x){
      iidx_sorted <- order(mat[x,],decreasing = TRUE)
      mat[x,iidx_sorted[1]]
    })
  res <- abs(personal_in_group-mat[group,gchoice])
  return (mean(res))
}

#compute loss 2: return a vector of loss for each group member
compute_loss_indv <- function(group, umat, gchoice){
  personal <- group %>%
    map_dbl(function(x){
      iidx_sorted <- order(umat[x,],decreasing = TRUE)
      umat[x,iidx_sorted[1]]
    })
  res <- abs(personal-umat[group,gchoice])
  return (res)
}
compute_loss_indv_in_group <- function(group, umat, gchoice, fmat, WUG){
  mat <- as.matrix(WUG)%*%t(fmat)
  personal_in_group <- group %>%
    map_dbl(function(x){
      iidx_sorted <- order(mat[x,],decreasing = TRUE)
      mat[x,iidx_sorted[1]]
    })
  res <- abs(personal_in_group-mat[group,gchoice])
  return (res)
  
}
