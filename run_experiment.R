source("generate_group.R")
source("generate_profiles.R")
source("infer_constraints.R")
source("recommend_items.R")
#input: rating matrix (rmat), item feature matrix (fmat), utlity matrix (umat), 
#user profiles (profiles), group sizes (sizes)
#Ctrl+Alt+Shift+R
#' Title
#'
#' @param rmat (rating matrix)
#' @param fmat 
#' @param umat 
#' @param profiles 
#' @param sizes 
#' @param prob_feedback 
#' @param prob_BLD 
#' @param num_trials 
#' @param num_cyles 
#' @param gamma 
#' @param b 
#' @param topk 
#' @param bins 
#'
#' @return
#' @export
#'
#' @examples
run_exp <- function(rmat, fmat, umat, profiles, sizes, prob_feedback, prob_BLD, num_trials, num_cyles, gamma, b, topk, bins){
  recom_idxs <- list(NULL)
  for(size in sizes){
    #load group information 
    file_name_01 <- paste0("data/group",size,".txt")
    file_name_02 <- paste0("data/group_style",size,".txt")
    if(!file.exists(file_name_01)){
      generate_group(rmat,size,num_trials,file_name_01) #create random group member
    }
    if(!file.exists(file_name_02)){
      generate_group_type(size,num_trials,file_name_02)
    }
    group_members <- read.table(file_name_01)
    group_members <- as.matrix(group_members) 
    group_styles <- read.table(file_name_02)
    group_styles <- as.matrix(group_styles) 
    
    for(cur in 1:num_trials){ # repeat 100 times
      current_group <- group_members[cur,]
      current_style <- group_styles[cur,]
      res <- generate_propose_eval_prob(current_group,current_style,prob_feedback,gamma)
      prob_prop_new <- res[1,]
      prob_eval_new <- res[2,]
      prop_items <- vector("integer",10) #list containing all proposed items of group (max=10)
      num_prop_items <- 0
      WU_updated <- list(NULL)
      
      for(t in 1:num_cycles){ #interaction length
        if(t==1){
          WU_updated[[t]] <- profiles  
        }else{
          WU_updated[[t]] <- WU_updated[[t-1]]  
        }
        #1. randomly pick a user and he will make an item proposal with probability
        is_proposed <- 0
        idx_selected <-0
        evals <-vector("integer",length(current_group)-1) #excluding the user who propose an item
        while(is_proposed==0){
          idx_selected <- sample(length(current_group),1)
          ppu <- prob_prop_new[idx_selected]
          is_proposed <- sample(c(0,1),1,prob=c(1-ppu,ppu))
        }
        user_proposed <- current_group[idx_selected]
        while(sum(evals) == 0){ #make sure there is at least 1 evaluation
          idx_remaining <- setdiff(seq_along(current_group),idx_selected)
          pfu <- prob_eval_new[idx_remaining] #vector of pf for the remaining users
          evals <- sapply(pfu,function(x){sample(c(0,1),1,prob=c(1-x,x))})
        }
        idx_evals <- idx_remaining[evals==1]
        
        #2. item proposed to the group
        iidx_sorted <- order(umat[user_proposed,],decreasing = TRUE)
        iidx_prop <- 1
        while(iidx_sorted[iidx_prop] %in% prop_items){ #if the item (iidx_prop) is already proposed
          iidx_prop <- iidx_prop + 1 #get next item
        }
        num_prop_items <- num_prop_items + 1
        prop_items[num_prop_items]<-iidx_sorted[iidx_prop]
        
        
        #3. how the others evaluate the proposed item, including the one proposing items
        eval_users <- append(idx_evals,idx_selected,0) #containing index of users
        inferred_constraints <-list(NULL)
        for(uidx in 1:length(current_group)){
          if(is.element(uidx,eval_users)){
            conflict_type <- current_style[uidx]
            u <- current_group[uidx]
            u_util <- umat[u,prop_items] #vector of utlity values
            #get feedback for all proposed item, given the utility
            #TODO: break down into other functions for re-using
            feedback <- map(u_util,generate_profiles,conflict_type,prob_BLD,bins,b) %>%
              map(function(x){
                sample(c("best","like","dislike"),1,prob=c(x[1],x[2],x[3]))
              })
            
            u_feedback <- list(best=NULL,like=NULL,dislike=NULL)
            for(i in 1:num_prop_items){ 
              if(u==idx_selected && i==num_prop_items){  #implicit feedback, proposed item -> best
                u_feedback$best<-c(u_feedback$best,prop_items[i])
              }else{
                if(feedback[[i]]=="best"){
                  u_feedback$best<-c(u_feedback$best,prop_items[i])
                }else if(feedback[[i]]=="like"){
                  u_feedback$like<-c(u_feedback$like,prop_items[i])
                }else{
                  u_feedback$dislike<-c(u_feedback$dislike,prop_items[i])
                }
              }
            }#end-for 1:num_prop_items
          }#end if user has at least 1 evaluation
          inferred_constraints[[uidx]] <- infer_constraints(fmat,u_feedback,num_prop_items)
          
        }#end-for uidx in members 
        rec <- recommend_items(current_group,num_prop_items,fmat,inferred_constraints,WU_updated[[t]])
        WU_updated[[t]] <- rec$WU_updated
        score <- fmat%*%as.matrix(rec$wg)
        recom_idx <- seq_along(items)[order(score,decreasing =TRUE)][1:topk]  
        recom_idxs[[size-1]] <- recom_idx
      }#end-for interaction length
    }#end-for trials (validations)
  }#end-for group size
  names(recom_idxs) <- sizes
  return (recom_idxs)
}

