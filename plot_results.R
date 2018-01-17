library(ggplot2) 
library(reshape2)
library(gridExtra)
score_types <- matrix(0,nrow=(length(group_sizes)*5),ncol=num_cycles)
j_start <- 0
for(i in 1:5){
  group_type <- styles[i]
  fname <- paste0("results/loss_type_",group_type,"_cycles_", num_cycles, "_gamma_", gamma, "_b_", b, ".txt", sep = "")
  j <- j_start + 1
  j_start <- j + 3
  score_types[j:j_start,] <- as.matrix(read.table(fname))
  dim(as.matrix(read.table(fname)))
}

p <- list(NULL)
for(size in 2:5){
  p[[size-1]] <- plot_data(size,score_types,num_cycles)
  show(p[[size-1]]) 
  ggsave(p[[size-1]],filename=paste0("results/loss_g",size,".png",sep=""))
}

plot_data <- function(size, scores, num_cycles){
  i1 <- size - 1 #size is 2, or 3,..or 5
  i2 <- i1 + 4
  i3 <- i2 + 4
  i4 <- i3 + 4
  i5 <- i4 + 4
  cycles <- 1:num_cycles
  mycolors <-c("red","blue","sea green","orange","purple")
  myshapes <-15:19
  mylabels <- list("compromise (b.thuong)","compete (dau tranh)", "accommodate (nhuong bo)",
                   "avoid (ne tranh)", "collaborate (hop tac)")
  #"compromise","compete","accommodate","avoid","collaborate"
  df <- data.frame(cycles = 1:num_cycles, compromise = scores[i1,], 
                      compete = scores[i2,], 
                      accommodate =scores[i3,],
                      avoid = scores[i4,],
                      collaborate = scores[i5,])
  df2 <- melt(data = df, id.vars = "cycles", variable.name = "type", value.name = "loss")
  
  if(size==2){
    lim <- c(0.02,0.03)
  }else if (size==3){
    lim <- c(0.025,0.035)
  }else if (size==4 || size==5){
    lim <- c(0.04,0.052)
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
    ylab("Average utility loss")+
    # ylim(lim)+
    ggtitle(paste0("Groups of size ",size))
  return (plot)
}

# plot mixed results
score_mixed <- read.table(paste0("results/loss_type_","mixed","_cycles_", num_cycles, "_gamma_", gamma, "_b_", b, ".txt", sep = ""))
score_mixed <- as.matrix(score_mixed)
score_mixed <- t(score_mixed)
p <- list(NULL)
for(size in 2:5){
  p[[size-1]] <- plot_data(size,score_mixed,num_cycles)
  show(p[[size-1]]) 
  ggsave(p[[size-1]],filename=paste0("results/loss_mixed_g",size,".png",sep=""))
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
