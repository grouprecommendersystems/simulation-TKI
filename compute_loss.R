compute_loss <- function(group, umat, gchoice){
  personal <- group %>%
    map_dbl(function(x){
      iidx_sorted <- order(umat[x,],decreasing = TRUE)
      umat[x,iidx_sorted[1]]
    })
  res <- abs(personal-umat[group,gchoice])
  return (mean(res))
}