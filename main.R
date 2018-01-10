library(dplyr)
library(purrr)
#load data from files
ratings<-read.csv("data/reviews.csv",TRUE,",") # header = FALSE
ratings<-as.matrix(ratings)
colnames(ratings)<-c("userid","itemid","rating")

group_ratings<-read.csv("data/reviews_group.csv",TRUE,",")
group_ratings<-as.matrix(group_ratings)
colnames(group_ratings)<-c("itemid","userid","best","like","dislike","groupid","proposerid")

items<-read.csv("data/item_feature.csv",FALSE,",")
items<-as.matrix(items)

#mapping between item_name (item_id) and index
items_mapping <- items[,1] #test which(items[,1]==176552); which(items_mapping==176552)

#replace item_name in ratings with item index
item_idx = rep(0,nrow(ratings))
for(i in 1:nrow(ratings)){
  if(ratings[i,2] %in% items[,1]){
    item_idx[i] <- which(items[,1]==ratings[i,2])
  }else{
    cat("Item_id ",ratings[i,2]," is NOT in the list of items. Please check!!!",fill = TRUE)
  }
}
ratings[,2] <- item_idx
#replace item_name in GROUP_ratings with item index
item_idx <- rep(0,nrow(group_ratings))
for(i in 1:nrow(group_ratings)){
  if(group_ratings[i,1] %in% items[,1]){
    item_idx[i] <- which(items[,1] == group_ratings[i,1])
  }
}
group_ratings[,1] <- item_idx

#remove the item_id to work with item index
items <- items[,-1]

#execute feature selection code
source("select_feature.R")
items <- select_feature(items)
items[items<0.05]<- 0
items[items>=0.05]<- 1
#initialize user profile
source("initialize_utility.R")
WU <- init_utility(ratings,group_ratings,items)
utility_matrix <- WU%*%t(items)
# utility_matrix[which(utility_matrix > 1)]
# which(WU>1)
# max(items)

#initialize probabilities (BASELINE), based on data observation
#pp: prob. propose; pf: prob. feedback; pbld: p(best|WU), p(like|WU), p(dislike|WU)
source("initialize_probabilities.R")
bins <- c(0,0.2,0.4,1)
pf <- init_eval_prob(group_ratings)
pbld <- init_BLD_prob(group_ratings,utility_matrix,bins)

#experiments
source("run_experiment.R")
trial <- 100 # run 100 trials for each experiment
num_cycles <- 2
gamma <- 0.1
b <- 10
topk <- 20
group_sizes <- 2:5
recom <- run_exp(ratings,items,utility_matrix,WU,group_sizes,pf,pbld,trial,num_cycles,gamma,b,topk,bins)


