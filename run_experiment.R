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
run_exp <- function(rmat, fmat, umat, profiles, sizes, gname, prob_feedback, prob_BLD, num_trials, num_cycles, gamma, b, topk, bins){
  recom_idxs <- list(NULL)
  loss <- matrix(0,length(sizes),num_cycles)
  loss_mixed <- list()
  idx_size <- 1
  for(size in sizes){
    #size <- 5
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
      #group_styles <- matrix(rep(c(2,1),size), nrow=num_trials, ncol = size, byrow = TRUE)
      group_styles <- matrix(rep(c(1:5),size), nrow=num_trials, ncol = size, byrow = TRUE)
    }else{
      code <- get_style_name(gname)
      group_styles <- matrix(code, nrow=num_trials, ncol = size)
    }
    count_mixed <- matrix(0, nrow=num_cycles, ncol=5) #ncol is number of resolution styles
    tmp_loss_mixed <- matrix(0, nrow=num_cycles, ncol=5) #ncol is number of resolution styles
    
    for(cur in 1:num_trials){ # repeat 100 times
      current_group <- group_members[cur,]
      current_style <- group_styles[cur,]
      res <- generate_propose_eval_prob(current_group,current_style,prob_feedback,gamma)
      prob_prop_new <- res[1,]
      prob_eval_new <- res[2,]
      num_prop_items <- 0
      who_what_prop <- data.frame(user=c(0),prop_item=c(0))
      WU_updated <- list(NULL)
      tmp_loss <- rep(-1,num_cycles)
      
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
        prop_items <- who_what_prop[who_what_prop!=0,"prop_item"]
        while(iidx_sorted[iidx_prop] %in% prop_items){ #if the item (iidx_prop) is already proposed
          iidx_prop <- iidx_prop + 1 #get next item
        }
        num_prop_items <- num_prop_items + 1
        if(num_prop_items==1){
          who_what_prop[num_prop_items,] <- c(user_proposed, iidx_sorted[iidx_prop])
        }else{
          who_what_prop <- rbind(who_what_prop,c(user_proposed, iidx_sorted[iidx_prop]))
        }
        
        #3. how the others evaluate the proposed item, including the one proposing items
        for(uidx in idx_evals){
          who_what_prop <- rbind(who_what_prop,c(current_group[uidx],0))
        }
        inferred_constraints <-list(NULL)
        group_feedback <- list(NULL)
        for(uidx in 1:length(current_group)){
          conflict_type <- current_style[uidx]
          u <- current_group[uidx]
          #get feedback 
          group_feedback[[uidx]] <- generate_feedback(u,umat,who_what_prop,conflict_type,prob_BLD,bins,b,NULL)
          inferred_constraints[[uidx]] <- infer_constraints(fmat,group_feedback[[uidx]],num_prop_items)
          
        }#end-for uidx in members 
        rec <- recommend_items(current_group,num_prop_items,fmat,inferred_constraints,WU_updated[[t]])
        WU_updated[[t]] <- rec$WU_updated
        score <- fmat%*%as.matrix(rec$wg)
        recom_idx <- seq_along(items)[order(score,decreasing =TRUE)][1:topk]  
        recom_idxs[[size-1]] <- recom_idx
        
        #4. computing loss
        idx <- 1
        group_choice <- NULL
        count <- 0
        while(count!=length(current_group) && idx<=topk){
          group_choice <- recom_idx[idx]
          #if idx is not a satisfactory solution to all the members
          for(uidx in 1:length(current_group)){
            conflict_type <- current_style[uidx]
            u <- current_group[uidx]
            group_feedback[[uidx]] <- generate_feedback(u,umat,who_what_prop,conflict_type,prob_BLD,bins,b,recom_idx)
            if(is.element(group_choice,group_feedback[[uidx]]$best) || 
               is.element(group_choice,group_feedback[[uidx]]$like)){
              count <- count + 1
            }else{
              break
            }
          }
          idx <- idx + 1
        }
        if(!is.null(group_choice)){
          if(gname!="mixed"){
            tmp_loss[t] <- compute_loss_avg(current_group,umat,group_choice)  
          }else{
            tmp <- compute_loss_2(current_group,umat,group_choice)
            for(s in 1:5){
              if(sum(current_style==s)>0){
                tmp_loss_mixed[t,s] <- tmp_loss_mixed[t,s] + sum(tmp[current_style==s])
                count_mixed[t,s] <- count_mixed[t,s] + sum(current_style==s)  
              }
            }
          }
        }
      }#end-for interaction length
      if(gname!="mixed"){
        loss[idx_size,] <- loss[idx_size,] + tmp_loss  
      }
      #print(paste0("Finish validation ",cur))
    }#end-for trials (validations)
    if(gname=="mixed"){
      count_mixed[count_mixed==0]<-1
      loss_mixed[[idx_size]] <- tmp_loss_mixed/count_mixed 
    }
    idx_size <- idx_size + 1
    print(paste0("Finish group ",size))
  }#end-for group size
  fname <- paste0("results/loss_type_",gname,"_cycles_", num_cycles, "_gamma_", gamma, "_b_", b, ".txt", sep = "")
  if(gname!="mixed"){
    loss <- loss / num_trials
    write.table(loss,file = fname)
  }else{
    names(loss_mixed) <- sizes
    write.table(loss_mixed,file = fname)
  }
  #return (loss)
  # names(recom_idxs) <- sizes
  # return (recom_idxs)
}


