library(ggplot2) 
library(reshape2)
library(gridExtra)

read_result <- function(size,k){
  for(type in styles){
    fname <- paste0("results/personal_",k,"_loss_type_",type,"_cycles_", num_cycles, "_gamma_", gamma, "_b_", b, ".txt", sep = "")
    mat <- as.matrix(read.table(fname))
    colnames(mat) <- c(rep("g2",2),rep("g3",2),rep("g4",2),rep("g5",2))
    start_range <- -1:2
    start <- start_range[size-1]+size
    end <- start+1
    
    if(k=="IN"){
      ytitle <- "Average utility loss (UPDATED)"
    }else{
      ytitle <- "Average utility loss (ORIGINAL)"
    }
    g <- mat[,start:end]
    cycles <- 1:num_cycles
    df <- data.frame(cbind(cycles=1:num_cycles,g))
    names(df) <- c("cycles","max avg utility","min avg utility")
    mylabels <- as.list(c("max utility","min utility"))
    df2 <- melt(data = df, id.vars = "cycles", variable.name = "utility", value.name = "loss")
    title <- paste0("Groups of size ",size," (style: ",type,")")
    
    if(size==2){
      lim <- c(0.045,0.12)
    }else if(size==3){
      lim <- c(0.05,0.18)
    }else{
      lim <- c(0.05,0.22)
    }
    
    plot <- ggplot(data=df2, aes(x=cycles, y=loss, group=utility, colour=utility)) +
      geom_line(aes(linetype=utility), # Line type depends on size
                size = 0.5,show.legend = TRUE) +       # Thicker line
      geom_point(aes(shape=utility),   # Shape depends on size
                 size = 2) +       # Large points
      scale_x_continuous(breaks=cycles) + 
      #scale_color_manual(values=mycolors, labels=mylabels) +
      # scale_shape_manual(values=myshapes, labels=mylabels)+
      theme(legend.title=element_blank())+
      #theme(legend.position="none")+
      xlab("# of interaction cycles") + 
      ylab(ytitle)+
      ylim(lim)+
      ggtitle(title)
    show(plot)
  }
}
data_type <- c("OUT","IN")
for(k in data_type){
  read_result(5,k)
}

