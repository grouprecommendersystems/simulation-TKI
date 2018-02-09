#input: style_code 
#output: style_name
get_style_name <- function(code){
  styles <- data.frame(name=c("compromise","compete","accommodate","avoid","collaborate"),id=1:5)
  if(is.numeric(code)){
    return (as.character(styles[code,"name"]))  
  }
  if(is.character(code)){
    return (styles[styles[,"name"]==code,"id"])
  }
}

# input: groups (vector of members)
#       type (compromising, competing, accommodating, avoiding, collaborating)
#       prob_eval, gamma
# output: list (member x (prob_prop; prob_eval))
generate_propose_eval_prob <- function(group, group_type, prob_eval, gamma){
  # group <-current_group
  # group_type <- current_style
  # gamma <- 0.1
  # prob_eval <- 0.5
  
  prob_prop <- 1/length(group)
  #TODO: test with fixed value (all members are independent)
 #prob_prop <- 0.7
  
  style_names <- get_style_name(group_type)
  res <- sapply(style_names,function(x){
    if(x=="compromise"){
      pp <- prob_prop
      pf <- prob_eval
    }else if(x=="compete" ){
      pp <- prob_prop + gamma
      pf <- prob_eval + gamma
      # pp <- 1
      # pf <- 1
    }else if(x=="collaborate"){
      pp <- prob_prop + runif(1,gamma-0.05,gamma)
      pf <- prob_eval + runif(1,gamma-0.05,gamma)
    }else if(x=="accommodate"){
      #ensure that pp and pf is not zero or negative
      if(gamma < prob_prop && gamma <prob_eval){
        pp <- prob_prop - gamma
        pf <- prob_eval - gamma
      }else{
        pp <- prob_prop - 0.05
        pf <- prob_eval - 0.05
      }
    }else if(x=="avoid"){
      if(gamma < prob_prop && gamma <prob_eval){
        pp <- prob_prop - runif(1,gamma-0.05,gamma)
        pf <- prob_eval - runif(1,gamma-0.05,gamma)  
      }else{
        pp <- prob_prop - 0.05
        pf <- prob_eval - 0.05
      }
    }
    return (c(pp,pf))
  })
  
  #normalized: to sum to 1 
  #TODO: check if it is necessary to do this) ON & OFF
  # res[1,] <- res[1,]/sum(res[1,])
  # res[2,] <- res[2,]/sum(res[2,])
  
  return (res)
}

# input: x = a utility value of a user 
#       type (compromising, competing, accommodating, avoiding, collaborating)
#       prob_BLD_bin, bins, b (b^x), gamma
# output: prob_BLD_new 
generate_profiles <- function(x, type, prob_BLD, bins, b){
  position <- which(tapply(x,cut(x,bins),length)==1)
  if(length(position)==0)
    return (c(1,0,0))
  probs <- prob_BLD[,position] # vector of length 3 B, L, D
  
  style_name <- get_style_name(type)
  stopifnot(b!=0)
  x1 <- probs["dislike"]
  x2 <- probs["dislike"] + probs["like"]
  x3 <- probs["dislike"] + probs["like"] + probs["best"]
  if(style_name=="compromise"){
    pd <- probs["dislike"]
    pl <- probs["like"]
    pb <- probs["best"]
  }else{ #not the baseline
    if(style_name=="compete" ){
      if(b>1 && b!=0){
        #b <- 1/(b+5)
        b <- 1/(b)
      }
    }else if (style_name=="avoid"){
      if(b>1 && b!=0){
        b <- 1/b
      }
    }else if (style_name=="accommodate"){
      if(b<1 && b!=0){
        #b <- 1/(b-0.05)
        b <- 1/(b)
      }
    }else if(style_name=="collaborate"){
      if(b<1 && b!=0){
        b <- 1/b
      }
    }
    y1 <- (b^(x1)-1)/(b-1)
    pd <- y1
    y2 <- ((b^(x2)-1)/(b-1))
    pb <- 1 - y2
    pl <- y2 - y1
  }
  
  names(pb) <- "best"
  names(pl) <- "like"
  names(pd) <- "dislike"
  #print (c(pb,pl,pd))
  return (c(pb,pl,pd))
}


# actions_list<- who_what_prop
#' Title
#'
#' @param user 
#' @param umat utilty matrix
#' @param actions_list data frame in which a row is associated with a pair
#'  (who proposes and what is proposed). If what is proposed = 0 that means, the user just evaluates
#'  the item not propose
#' @param type : 5 conflict resolution types
#' @param prob_BLD : probability giving B, L, D feedback
#' @param bins 
#' @param b 
#' @param group_recoms: group recommendations 
#'
#' @return list containing items labeled B, L and D
#' @export
#'
#' @examples
generate_feedback <- function(user, umat, actions_list, type, prob_BLD, bins, b, group_recoms){
  u_feedback <- list(best=NULL,like=NULL,dislike=NULL)
  
  if(is.null(group_recoms)){
    prop_items <- actions_list[actions_list[,"prop_item"]!=0,"prop_item"]  
  }else{
    prop_items <- group_recoms
  }
  
  if(is.element(user,actions_list[,"user"]) || !is.null(group_recoms)){
    user_util_vals <- umat[user,prop_items] #vector of utlity values
    items_labels <- map(user_util_vals, generate_profiles, type, prob_BLD, bins, b) %>%
      map(function(x){
        sample(c("best","like","dislike"),1,prob=c(x[1],x[2],x[3]))
      })
    names(items_labels) <- prop_items
    for(item in prop_items){
      i <- which(actions_list[,"prop_item"]==item)
      if(length(i) > 0){
        if(user==actions_list[i,"user"]){  #implicit feedback, proposed item -> best
          u_feedback$best<-c(u_feedback$best,item)
        }
      }
      tmp <- items_labels[names(items_labels)==item]
      if(tmp[[1]]=="best"){
        if(!is.element(item,u_feedback$best))
          u_feedback$best<-c(u_feedback$best,item)
      }else if(tmp[[1]]=="like"){
        if(!is.element(item,u_feedback$like))
          u_feedback$like<-c(u_feedback$like,item)
      }else{
        if(!is.element(item,u_feedback$dislike))
          u_feedback$dislike<-c(u_feedback$dislike,item)
      }
      
    }
  }
  return (u_feedback)
}
