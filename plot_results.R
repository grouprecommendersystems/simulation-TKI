library(ggplot2) 
library(reshape2)
library(gridExtra)
library(dplyr)
#scores <- score_mixed
plot_data <- function(size, scores, num_cycles, is_homogeneous, data_type){
  if(is_homogeneous){
    idxs <- seq(from = size-1, to = dim(scores)[1], by=dim(scores)[1]/5) #5 is 5 styles
  }else{
    tmp <- (size-1)*5
    idxs <- (tmp-4):tmp
  }
  cycles <- 1:num_cycles
  mycolors <-c("red","blue","sea green","orange","purple")
  myshapes <-15:19
  mylabels <- list("compromise","compete", "accommodate",
                   "avoid", "collaborate")
  #"compromise","compete","accommodate","avoid","collaborate"
  df <- data.frame(cycles = 1:num_cycles, compromise = scores[idxs[1],], 
                   compete = scores[idxs[2],], 
                   accommodate =scores[idxs[3],],
                   avoid = scores[idxs[4],],
                   collaborate = scores[idxs[5],])
  df2 <- melt(data = df, id.vars = "cycles", variable.name = "type", value.name = "loss")
  
  # if(size==2){
  #   lim <- c(0.02,0.03)
  # }else if (size==3){
  #   lim <- c(0.025,0.035)
  # }else if (size==4 || size==5){
  #   lim <- c(0.04,0.052)
  # }
  
  if(is_homogeneous){
    title <- paste0("Groups of size ",size," (same style)")
  }else{
    title <- paste0("Groups of size ",size," (mixed styles)")
  }
  vertical_title = "Average utility loss"
  if(data_type==1){
    vertical_title <- paste0(vertical_title," (ORIGINAL)")
  }else{
    vertical_title <- paste0(vertical_title," (UPDATED)")
  }
  
  plot <- ggplot(data=df2, aes(x=cycles, y=loss, group=type, colour=type)) +
    geom_line(aes(linetype=type), # Line type depends on size
              size = 0.5,show.legend = FALSE) +       # Thicker line
    geom_point(aes(shape=type),   # Shape depends on size
               size = 2) +       # Large points
    scale_x_continuous(breaks=cycles) +
    scale_color_manual(values=mycolors, labels=mylabels) +
    scale_shape_manual(values=myshapes, labels=mylabels)+
    theme(legend.title=element_blank())+
    #theme(legend.position="none")+
    xlab("# of interaction cycles") +
    ylab(vertical_title)+
    # ylim(lim)+
    ggtitle(title)
  return (plot)
}

#Plot 1: Homogeneous groups
data_type <- c("_LOSS_OUT","_LOSS_IN")
for(k in seq_along(data_type)){
  score_types <- matrix(0,nrow=(length(group_sizes)*5),ncol=num_cycles)
  j_start <- 0
  for(i in 1:5){
    group_type <- styles[i]
    path <- paste0("results/",mname,"_",group_type,"_cycles_", num_cycles, "_gamma_", gamma, "_b_", b,sep = "")
    fname <- paste0(path,data_type[k],".txt", sep = "")
    # fname <- paste0(path,"_GCHOICE",".txt", sep = "")
    j <- j_start + 1
    j_start <- j + 3
    score_types[j:j_start,] <- as.matrix(read.table(fname))
    dim(as.matrix(read.table(fname)))
  }
  p <- list(NULL)
  for(size in group_sizes){
    p[[size-1]] <- plot_data(size,score_types,num_cycles,TRUE,k)
    show(p[[size-1]]) 
    #ggsave(p[[size-1]],filename=paste0("results/loss_g",size,".png",sep=""))
  }
}  




#Plot: Heterogeneous (mixed) results
plot_data_mixed <- function(size, scores, num_cycles, compare_styles, data_type){
  tmp <- (size-1)*5
  idxs <- (tmp-4):tmp
  
  cycles <- 1:num_cycles
  mycolors <-c("red","blue","sea green","orange","purple")
  myshapes <-15:19
  mylabels <- list("compromise","compete", "accommodate",
                   "avoid", "collaborate")
  mylabels <- mylabels[compare_styles]
  mycolors <- mycolors[compare_styles]
  
  #"compromise","compete","accommodate","avoid","collaborate"
  df <- data.frame(cycles = 1:num_cycles, compromise = scores[idxs[1],], 
                   compete = scores[idxs[2],], 
                   accommodate =scores[idxs[3],],
                   avoid = scores[idxs[4],],
                   collaborate = scores[idxs[5],])
  df <- select(df,cycles, compare_styles[1]+1, compare_styles[2]+1) 
  df2 <- melt(data = df, id.vars = "cycles", variable.name = "type", value.name = "loss")
  
  vertical_title = "Average utility loss"
  if(data_type==1){
    vertical_title <- paste0(vertical_title," (ORIGINAL)")
  }else{
    vertical_title <- paste0(vertical_title," (UPDATED)")
  }
  
  plot <- ggplot(data=df2, aes(x=cycles, y=loss, group=type, colour=type)) +
    geom_line(aes(linetype=type), # Line type depends on size
              size = 0.5,show.legend = FALSE) +       # Thicker line
    geom_point(aes(shape=type),   # Shape depends on size
               size = 2) +       # Large points
    scale_x_continuous(breaks=cycles) +
    scale_color_manual(values=mycolors, labels=mylabels) +
    scale_shape_manual(values=myshapes, labels=mylabels)+
    theme(legend.title=element_blank())+
    #theme(legend.position="none")+
    xlab("# of interaction cycles") +
    ylab(vertical_title)+
    # ylim(lim)+
    ggtitle(paste0("Groups of size ",size," (mixed styles)"))
  return (plot)
}

data_type <- c("_LOSS_OUT","_LOSS_IN")
for(k in seq_along(data_type)){
  path <- paste0("results/",mname,"_",group_type,"_cycles_", num_cycles, "_gamma_", gamma, "_b_", b,sep = "")
  fname <- paste0(path,data_type[k],".txt", sep = "")
  score_mixed <- read.table(fname)
  score_mixed <- as.matrix(score_mixed)
  score_mixed <- t(score_mixed)
  rownames(score_mixed) <- rep(1:5,4)
  #score_mixed <- score_mixed[apply(score_mixed,1,sum)!=0,]
  compare_styles <- unique(which((apply(score_mixed,1,sum)[1:5])!=0))
  
  p <- list(NULL)
  for(size in c(2,4)){
    p[[size-1]] <- plot_data_mixed(size,score_mixed,num_cycles,compare_styles,k)
    show(p[[size-1]]) 
    #ggsave(p[[size-1]],filename=paste0("results/loss_mixed_g",size,".png",sep=""))
  }
}







#combine
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(plot2)

p3 <- grid.arrange(arrangeGrob(plot2 + theme(legend.position="none"),
                               plot3 + theme(legend.position="none"),
                               nrow=1),
                   mylegend, nrow=2,heights=c(10, 1))
# 9.1 x 3.58 in image
ggsave(p3,filename=paste("results/Test_rank_group2_s123.eps",sep=""))

#PNG
ggsave(p3,filename=paste("results/Test_rank_group5_s123.png",sep=""))
