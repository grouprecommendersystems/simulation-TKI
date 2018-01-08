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
init_BLD_prob <- function(x,y,bins){
  #probability of evaluating as B, L, D given utility value
  source("map_utility_feedback.R")
  utility_feedback <- list(best=find_feedback_utility(x, y, "best"),
              like=find_feedback_utility(x, y, "like"),
              dislike=find_feedback_utility(x, y,"dislike"))%>%
    lapply(count_bin,bins)
  probability_matrix <- prob_bin_feedback(utility_feedback,bins)
  return (t(probability_matrix))
}