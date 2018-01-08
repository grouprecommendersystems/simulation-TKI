#Lookup utility value of best, like, neutral, dislike items for all users
#args: x = group ratings, y = utility matrix, ftype = feedback type: best/like/dislike
#return: list mapping utility value - feedback
find_feedback_utility <- function(x, y, ftype){
  #result <- list(NULL)
  ftype_list <- NULL
  ftype_idx <-NULL
  stopifnot(ftype=="best" || ftype=="like" || ftype=="dislike")
  for(uid in unique(x[,2])){
    feed_idx <- which(x[,2]==uid)
    ftype_idx <- x[feed_idx,][which(x[feed_idx,][,ftype]==1),1]
    if(length(ftype_idx)!=0){
      ftype_list <- c(ftype_list, y[uid,ftype_idx])  
    }
  }
  return(ftype_list)
}



#Counting utility value of each type feedback in a certain bin (0,0.1], (0.1,0.2]
#args: x = list of utility values for each type of feedback
#return: data frame mapping bin- #number_feedback
count_bin <- function(x,bins){
  result <- data.frame(NULL)
  if(!is.null(x)){
    tmp <- tapply(x,cut(x,bins),length)
    tmp[is.na(tmp)] <- 0
    result <-rbind(result, tmp)
  }else{
    result <-rbind(result, rep(0,10))
  }
 # names(result) <- c("(0,0.1]","(0.1,0.2]","(0.2,0.3]","(0.3,0.4]","(0.4,0.5]",
 #                        "(0.5,0.6]","(0.6,0.7]","(0.7,0.8]","(0.8,0.9]","(0.9,1]")
 names(result) <- c("(0,0.2]","(0.2,0.4]","(0.4,1]")
 # result <-cbind(label=c("best","like","neutral","dislike"),result)
  return (result)
}


#Compute probabilities of different types of feedback in different bins (0,0.1], (0.1,0.2]
#args: x = list containing the number of responses in each bins
#return: prob.
prob_bin_feedback <- function(x,bins){
  result <- list(NULL)
  result_df <-data.frame(NULL)
  for(k in 1:(length(bins)-1)){
    tmp <- rep(0,3) #sum of best, like, neutral, dislike
    tmp[1] <- tmp[1] + x[[1]][,k] #best
    tmp[2] <- tmp[2] + x[[2]][,k] #like
    tmp[3] <- tmp[3] + x[[3]][,k] #dislike
    denominator <- sum(tmp)
    if(denominator==0)
      denominator <- 1 # avoid zero division
    best_prob <- tmp[1]/denominator
    like_prob <- tmp[2]/denominator
    dislike_prob <- tmp[3]/denominator
    
    result_df[k,1] <- round(best_prob,2)
    result_df[k,2] <- round(like_prob,2)
    result_df[k,3] <- round(dislike_prob,2)
  
  }
  #names(result) <- bins[2:length(bins)]
  #rownames(result_df) <- bins[2:length(bins)]
  # rownames(result_df) <- c("(0,0.1]","(0.1,0.2]","(0.2,0.3]","(0.3,0.4]","(0.4,0.5]",
  #                          "(0.5,0.6]","(0.6,0.7]","(0.7,0.8]","(0.8,0.9]","(0.9,1]")
   rownames(result_df) <- c("(0,0.2]","(0.2,0.4]","(0.4,1]")
  colnames(result_df) <- c("best","like","dislike")
  
  return(result_df)
}

#draw pie chart
library(tidyr)
#arg: x = group_ratings
draw_pie_chart <- function(x){
  feedback_df <- data.frame(best=sum(x[,"best"]==1),
                            like=sum(x[,"like"]==1),
                            dislike=sum(x[,"dislike"]==1),
                            not_eval  = sum(x[,"best"]==0 & x[,"like"]==0 & x[,"dislike"]==0))
  feedback_df <- gather(feedback_df,type,val)
  
  #pie chart
  
  pct <- round(feedback_df$val/sum(feedback_df$val)*100)
  lbls <- paste(feedback_df$type, pct) # add percents to labels 
  lbls <- paste(lbls,"%",sep="") # ad % to labels 
  lbls <- paste(lbls,"(",feedback_df$val,")",sep="") # ad % to labels 
  pie(feedback_df$val,labels = lbls, col=rainbow(length(lbls)),
      main="Pie Chart ")
}