source("generate_group.R")
source("generate_profiles.R")
source("infer_constraints.R")
source("recommend_items.R")
source("compute_loss.R")
#input: rating matrix (rmat), item feature matrix (fmat), utlity matrix (umat), 
#user profiles (profiles), group sizes (sizes)
#Ctrl+Alt+Shift+R

#' Title
#'
#' @param rmat 
#' @param fmat 
#' @param umat 
#' @param profiles 
#' @param sizes 
#' @param gname 'one of 5 conflict resolution types
#' @param prob_feedback 
#' @param prob_BLD 
#' @param num_trials 
#' @param num_cycles 
#' @param gamma 
#' @param b 
#' @param topk 
#' @param bins 
#'
#' @return
#' @export
#'
#' @examples
run_exp <- function(rmat, fmat, umat, profiles, sizes, gname, prob_feedback, prob_BLD, 
                    num_trials, num_cycles, gamma, b, topk, bins, path, mixed_pair=0){
  recom_idxs <- list(NULL)
  loss <- matrix(0,length(sizes),num_cycles)
  loss_new <- matrix(0,length(sizes),num_cycles)
  loss_mixed <- list()
  loss_mixed_new <- list()
  num_constraint_group_mixed <- list()
  idx_size <- 1
  num_constraint_group <- matrix(0,length(sizes),num_cycles)
  user_profile_diff <- matrix(0,length(sizes),num_cycles)
  pos_group_choice <- matrix(0,length(sizes),num_cycles)
  for(size in sizes){
    #size <- 2
    #load group information
    file_name_01 <- paste0("data/group",size,"_trial",num_trials,".txt")
    file_name_02 <- paste0("data/group_style",size,"_trial",num_trials,".txt")
    if(!file.exists(file_name_01)){
      generate_group(rmat,size,num_trials,file_name_01) #create random group member
    }
    if(!file.exists(file_name_02)){
      generate_group_type(size,num_trials,file_name_02)
    }
    group_members <- read.table(file_name_01)
    group_members <- as.matrix(group_members) 
    #TODO: with different group style
    if(group_type=="mixed"){
      # group_styles <- read.table(file_name_02) #Mixed
      # group_styles <- as.matrix(group_styles)
      # group_styles <- matrix(rep(c(2,4),size), nrow=num_trials, ncol = size, byrow = TRUE)
      group_styles <- matrix(rep(c(2,mixed_pair),size), nrow=num_trials, ncol = size, byrow = TRUE)
      #group_styles <- matrix(rep(c(1:5),size), nrow=num_trials, ncol = size, byrow = TRUE)
    }else{
      code <- get_style_name(gname)
      group_styles <- matrix(code, nrow=num_trials, ncol = size)
    }
    count_mixed <- matrix(0, nrow=num_cycles, ncol=5) #ncol is number of resolution styles
    tmp_loss_mixed <- matrix(0, nrow=num_cycles, ncol=5) #ncol is number of resolution styles
    tmp_loss_mixed_new <- matrix(0, nrow=num_cycles, ncol=5) #ncol is number of resolution styles
    count_constraint_mixed <- matrix(0, nrow=num_cycles, ncol=5) #ncol is number of resolution styles
    
    for(cur in 1:num_trials){ # repeat 100 times
      #cur=1
      current_group <- group_members[cur,]
      current_style <- group_styles[cur,]
      res <- generate_propose_eval_prob(current_group,current_style,prob_feedback,gamma)
      prob_prop_new <- res[1,]
      prob_eval_new <- res[2,]
      num_prop_items <- 0
      who_what_prop <- data.frame(user=c(0),prop_item=c(0))
      WU_updated <- list(NULL)
      tmp_loss <- rep(-1,num_cycles)
      tmp_loss_new <- rep(-1,num_cycles)
      tmp_user_profile_diff <- vector("double",num_cycles)
      tmp_pos_group_choice <- vector("integer",num_cycles)
      inferred_constraints <- rep(list(NULL),num_cycles)
      num_constraints <- vector("integer",num_cycles)
      for(t in 1:num_cycles){ #interaction length
        #t=1
        if(t==1){
          WU_updated[[t]] <- profiles  
        }else{
          WU_updated[[t]] <- WU_updated[[t-1]]  
        }
        #1. randomly pick a user and he will make an item proposal with probability
        is_proposed <- vector("integer",1)
        idx_selected <- vector("integer",1)
        
        while(sum(is_proposed)==0){
          idx_selected <- sample(length(current_group),1) #at least 2 proposals
          ppu <- prob_prop_new[idx_selected] #vector of ppu for the 2 users
          is_proposed <- sapply(ppu,function(x){sample(c(0,1),1,prob=c(1-x,x))})
        }
        #idx_selected <- idx_selected[is_proposed==1]
        user_proposed <- current_group[idx_selected]
        
        evals <-vector("integer",length(current_group)-1) #excluding the user who propose an item
        while(sum(evals) == 0){ #make sure there is at least 1 evaluation
          idx_remaining <- setdiff(seq_along(current_group),idx_selected[1])
          pfu <- prob_eval_new[idx_remaining] #vector of pf for the remaining users
          evals <- sapply(pfu,function(x){sample(c(0,1),1,prob=c(1-x,x))})
       }
        idx_evals <- idx_remaining[evals==1]
        
        #2. item proposed to the group
        for(u in user_proposed){
          iidx_sorted <- order(umat[u,],decreasing = TRUE)  
          iidx_prop <- 1
          prop_items <- who_what_prop[who_what_prop!=0,"prop_item"]
          while(iidx_sorted[iidx_prop] %in% prop_items){ #if the item (iidx_prop) is already proposed
            iidx_prop <- iidx_prop + 1 #get next item
          }
          num_prop_items <- num_prop_items + 1
          if(num_prop_items==1){
            who_what_prop[num_prop_items,] <- c(u, iidx_sorted[iidx_prop])
          }else{
            who_what_prop <- rbind(who_what_prop,c(u, iidx_sorted[iidx_prop]))
          }
          #Only store the current proposed item for the current cycle
          # who_what_prop[num_prop_items,] <- c(u, iidx_sorted[iidx_prop])
        }
        
        #3. how the others evaluate the proposed item, including the one proposing items
        for(uidx in idx_evals){
          who_what_prop <- rbind(who_what_prop,c(current_group[uidx],0))
        }
        # inferred_constraints <-list(NULL)
        group_feedback <- list(NULL)
        for(uidx in 1:length(current_group)){
          conflict_type <- current_style[uidx]
          u <- current_group[uidx]
          #get feedback 
          group_feedback[[uidx]] <- generate_feedback(u,umat,who_what_prop,conflict_type,prob_BLD,bins,b,NULL)
          tmp <- infer_constraints(fmat,group_feedback[[uidx]],num_prop_items)
          # if(t==1){
          #   inferred_constraints[[t]][[uidx]] <- tmp
          # }
          # else{
          #   #if(sum(tmp!=0)>0)
          #     inferred_constraints[[t]][[uidx]] <- rbind(inferred_constraints[[t-1]][[uidx]],tmp)
          # }
          inferred_constraints[[t]][[uidx]] <- tmp
          # inferred_constraints[[uidx]] <- tmp
          if(sum(tmp!=0)>0){
            num_constraints[t] <- num_constraints[t] + dim(tmp)[1]
            count_constraint_mixed[t,conflict_type] <- count_constraint_mixed[t,conflict_type] + dim(tmp)[1] 
          }
          
        }#end-for uidx in members 
        num_constraints[t] <- num_constraints[t]/length(current_group)
       
        # rec <- recommend_items(current_group,num_prop_items,fmat,inferred_constraints,WU_updated[[t]])
        rec <- recommend_items(current_group,num_prop_items,fmat,inferred_constraints[[t]],WU_updated[[t]])
        WU_updated[[t]] <- rec$WU_updated
        score <- fmat%*%as.matrix(rec$wg)
        recom_idx <- seq_along(items)[order(score,decreasing =TRUE)][1:topk]
        recom_idxs[[size-1]] <- recom_idx
        
        #4. computing loss
        idx <- 1
        group_choice <- NULL
        while(idx<=topk){
          count <- 0
          group_choice <- recom_idx[idx]
          #if idx is not a satisfactory solution to all the members
          for(uidx in 1:length(current_group)){
            conflict_type <- current_style[uidx]
            u <- current_group[uidx]
            group_feedback[[uidx]] <- generate_feedback(u,umat,who_what_prop,conflict_type,prob_BLD,bins,b,recom_idx)
            if(is.element(group_choice,group_feedback[[uidx]]$best)#){
               || is.element(group_choice,group_feedback[[uidx]]$like)){
              count <- count + 1
            }else{
              break
            }
          }
          if(count==length(current_group)){
            tmp_pos_group_choice[t]<- idx
            break
          }else{
            idx <- idx + 1  
          }
          
        }
        if(!is.null(group_choice)){
          if(gname!="mixed"){
            tmp_loss[t] <- compute_loss_avg(current_group,umat,group_choice)
            tmp_loss_new[t] <- compute_loss_avg_in_group(current_group,umat, group_choice, fmat, WU_updated[[t]])
          }else{
            tmp <- compute_loss_indv(current_group,umat,group_choice)
            tmp_new <- compute_loss_indv_in_group(current_group,umat, group_choice, fmat, WU_updated[[t]])
            for(s in 1:5){
              if(sum(current_style==s)>0){
                tmp_loss_mixed[t,s] <- tmp_loss_mixed[t,s] + sum(tmp[current_style==s])
                count_mixed[t,s] <- count_mixed[t,s] + sum(current_style==s)
                tmp_loss_mixed_new[t,s] <- tmp_loss_mixed_new[t,s] + sum(tmp_new[current_style==s])
              }
            }
          }
        }
        #TEST: how much the updated utility vector is close to the original one
        for(u in current_group){
          #tmp_user_profile_diff[t] <- tmp_user_profile_diff[t]+ cosine(WU_updated[[t]][u,], profiles[u,])
          tmp_user_profile_diff[t] <- tmp_user_profile_diff[t]+ dist(rbind(WU_updated[[t]][u,], profiles[u,])) #euclidian
        }
        tmp_user_profile_diff[t] <- tmp_user_profile_diff[t] / length(current_group)
       
      }#end-for interaction length
      if(gname!="mixed"){
        loss[idx_size,] <- loss[idx_size,] + tmp_loss
        loss_new[idx_size,] <- loss_new[idx_size,] + tmp_loss_new
        num_constraint_group[idx_size,] <- num_constraint_group[idx_size,] + num_constraints
        user_profile_diff[idx_size,] <- user_profile_diff[idx_size,] + tmp_user_profile_diff
        pos_group_choice[idx_size,] <- pos_group_choice[idx_size,] + tmp_pos_group_choice
      }
      #print(paste0("Finish validation ",cur))
    }#end-for trials (validations)
    if(gname=="mixed"){
      count_mixed[count_mixed==0]<-1
      loss_mixed[[idx_size]] <- tmp_loss_mixed/count_mixed 
      loss_mixed_new[[idx_size]] <- tmp_loss_mixed_new/count_mixed
      num_constraint_group_mixed[[idx_size]] <- count_constraint_mixed/count_mixed
    }
    idx_size <- idx_size + 1
    print(paste0("Finish group ",size))
  }#end-for group size

  if(gname!="mixed"){
    # loss <- loss / num_trials
    # write.table(loss,file = paste0(path,"_LOSS_OUT",".txt", sep = ""))
    # loss_new <- loss_new / num_trials
    # write.table(loss_new, file = paste0(path,"_LOSS_IN",".txt", sep = ""))
    user_profile_diff <- user_profile_diff / num_trials
    write.table(round(user_profile_diff,3), file = paste0(path,"_PROFILE",".txt", sep = ""))
    # pos_group_choice <- pos_group_choice / num_trials
    # write.table(pos_group_choice, file = paste0(path,"_GCHOICE",".txt", sep = ""))
    # write.table(round(num_constraint_group/num_trials,0), file = paste0(path,"_NUM_CONSTRAINTS",".txt", sep = ""))
  }else{
    names(loss_mixed) <- sizes
    write.table(loss_mixed, file = paste0(path,"_LOSS_OUT",".txt", sep = ""))
    write.table(loss_mixed_new, file = paste0(path,"_LOSS_IN",".txt", sep = ""))
    write.table(num_constraint_group_mixed, file = paste0(path,"_NUM_CONSTRAINTS",".txt", sep = ""))
    
  }
  #return (loss)
  # names(recom_idxs) <- sizes
  # return (recom_idxs)
}


