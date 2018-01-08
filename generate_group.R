generate_group <- function(data, size, trial,fname){
  for(i in 1:trial){
    #sort(unique(data[,"userid"]))
    members <- sample(1:length(unique(data[,"userid"])), size)
    write.table(t(c(members)), file = fname, row.names=F, col.names=F, append=T,sep=" ")
  }
}


generate_group_type <- function(size,trial,fname){
  res <- vector("integer",size)
  for(i in 1:trial){
    for(j in 1:size){
      res[j] <- sample(1:5,1) # 5 corresponds to 5 conflicting styles
    }
    write.table(t(res), file = fname, row.names=F, col.names=F, append=T,sep=" ")
  }
}
