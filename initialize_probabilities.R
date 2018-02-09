library("dplyr")

#input: data (group_ratings), WU, 
#output: pf (probability of giving feedback)
init_eval_prob <- function(data){
  df_data <- as.data.frame(data)
  #probability of giving feedback item
  numerator <- sum(df_data[,"proposerid"]!=df_data[,"userid"] &
                     (df_data[,"best"]!=0 | 
                        df_data[,"like"]!=0 |
                        df_data[,"dislike"]!=0))
  
  tmp <- df_data %>% 
    group_by(groupid) %>%
    summarise(m_sum = sum(userid!=proposerid))
   denominator <- summarise(tmp,sum(m_sum))
   pf <-as.numeric(numerator / denominator)
   return (pf)
}

#input: x (group_ratings), y (utility_matrix), bins
#output: probabilty matrix 
init_BLD_prob <- function(x,y,bins,test_case_fixed=FALSE){
  #probability of evaluating as B, L, D given utility value
  
  if(!test_case_fixed){
    source("map_utility_feedback.R")
    utility_feedback <- list(best=find_feedback_utility(x, y, "best"),
                             like=find_feedback_utility(x, y, "like"),
                             dislike=find_feedback_utility(x, y,"dislike"))%>%
      lapply(count_bin,bins)
    probability_matrix <- prob_bin_feedback(utility_feedback,bins)  
  }else{
    # Test
    probability_matrix <-data.frame(c(0,0,1),c(0,1,0),c(1,0,0))
    rownames(probability_matrix) <- c("(0,0.2]","(0.2,0.4]","(0.4,1]")
    colnames(probability_matrix) <- c("best","like","dislike")
  }
  
  
 
  
  return (t(probability_matrix))
}