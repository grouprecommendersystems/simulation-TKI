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

#compute loss 2: return a vector of loss for each group member
compute_loss_2 <- function(group, umat, gchoice){
  personal <- group %>%
    map_dbl(function(x){
      iidx_sorted <- order(umat[x,],decreasing = TRUE)
      umat[x,iidx_sorted[1]]
    })
  res <- abs(personal-umat[group,gchoice])
  #res <- personal-umat[group,gchoice]
  return (res)
}
