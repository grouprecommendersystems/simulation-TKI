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


# #input: rating, utlity_matrix, pf, pbld
#  for(size in 2:5){
#   #load group information 
#   file_name_01 <- paste0("data/group",size,".txt")
#   file_name_02 <- paste0("data/group_style",size,".txt")
#   if(file.exists(file_name_01) && file.exists(file_name_02)){
#     group_members <- read.table(file_name_01)
#     group_members <- as.matrix(group_members) 
#     group_styles <- read.table(file_name_02)
#     group_styles <- as.matrix(group_styles) 
#   }else{
#     generate_group(ratings,size,trial,file_name_01) #create random group member
#     generate_group_type(size,trial,file_name_02)
#   }
#   
#   for(cur in 1:trial){ # repeat 100 times
#     current_group <- group_members[cur,]
#     current_style <- group_styles[cur,]
#     res <- generate_propose_eval_prob(current_group,current_style,pf,gamma)
#     prob_prop_new <- res[1,]
#     prob_eval_new <- res[2,]
#     prop_items <- vector("integer",10) #list containing all proposed items of group (max=10)
#     num_prop_items <- 0
#     WU_updated <- list(NULL)
#     
#     for(t in 1:num_cycles){ #interaction length
#       if(t==1){
#         WU_updated[[t]] <- WU  
#       }else{
#         WU_updated[[t]] <- WU_updated[[t-1]]  
#       }
#       #1. randomly pick a user and he will make an item proposal with probability
#       is_proposed <- 0
#       idx_selected <-0
#       evals <-vector("integer",length(current_group)-1) #excluding the user who propose an item
#       while(is_proposed==0){
#         idx_selected <- sample(length(current_group),1)
#         ppu <- prob_prop_new[idx_selected]
#         is_proposed <- sample(c(0,1),1,prob=c(1-ppu,ppu))
#       }
#       user_proposed <- current_group[idx_selected]
#       while(sum(evals) == 0){ #make sure there is at least 1 evaluation
#         idx_remaining <- setdiff(seq_along(current_group),idx_selected)
#         pfu <- prob_eval_new[idx_remaining] #vector of pf for the remaining users
#         evals <- sapply(pfu,function(x){sample(c(0,1),1,prob=c(1-x,x))})
#       }
#       idx_evals <- idx_remaining[evals==1]
#       
#       #2. item proposed to the group
#       iidx_sorted <- order(utility_matrix[user_proposed,],decreasing = TRUE)
#       iidx_prop <- 1
#       while(iidx_sorted[iidx_prop] %in% prop_items){ #if the item (iidx_prop) is already proposed
#         iidx_prop <- iidx_prop + 1 #get next item
#       }
#       num_prop_items <- num_prop_items + 1
#       prop_items[num_prop_items]<-iidx_sorted[iidx_prop]
#       
#       
#       #3. how the others evaluate the proposed item, including the one proposing items
#       eval_users <- append(idx_evals,idx_selected,0) #containing index of users
#       inferred_constraints <-list(NULL)
#       for(uidx in 1:length(current_group)){
#         if(is.element(uidx,eval_users)){
#           conflict_type <- current_style[uidx]
#           u <- current_group[uidx]
#           u_util <- utility_matrix[u,prop_items] #vector of utlity values
#           #get feedback for all proposed item, given the utility
#           feedback <- map(u_util,generate_profiles,conflict_type,pbld,bins,b) %>%
#             map(function(x){
#               sample(c("best","like","dislike"),1,prob=c(x[1],x[2],x[3]))
#             })
#           
#           u_feedback <- list(best=NULL,like=NULL,dislike=NULL)
#           for(i in 1:num_prop_items){ 
#             if(u==idx_selected && i==num_prop_items){  #implicit feedback, proposed item -> best
#               u_feedback$best<-c(u_feedback$best,prop_items[i])
#             }else{
#               if(feedback[[i]]=="best"){
#                 u_feedback$best<-c(u_feedback$best,prop_items[i])
#               }else if(feedback[[i]]=="like"){
#                 u_feedback$like<-c(u_feedback$like,prop_items[i])
#               }else{
#                 u_feedback$dislike<-c(u_feedback$dislike,prop_items[i])
#               }
#             }
#           }#end-for 1:num_prop_items
#         }#end if user has at least 1 evaluation
#         inferred_constraints[[uidx]] <- infer_constraints(items,u_feedback,num_prop_items)
#         
#       }#end-for uidx in members 
#       rec <- recommend_items(current_group,num_prop_items,items,inferred_constraints,WU_updated[[t]])
#       WU_updated[[t]] <- rec$WU_updated
#       score <- items%*%as.matrix(rec$wg)
#       recom_idx <- seq_along(items)[order(score,decreasing =TRUE)][1:topk]  
#       
#       #4. evaluations
#       group_choice <- vector("integer",topk)
#       loss <- 
#       for(uidx in 1:length(current_group)){
#         for(i in recom_idx){
#           u <- current_group[uidx]
#           u_util <- utility_matrix[u,i]
#         }
#       }
#       
#         
#     }#end-for interaction length
#   }#end-for trials (validations)
# }#end-for group size


