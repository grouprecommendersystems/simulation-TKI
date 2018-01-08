#input: style_code 
#output: style_name
get_style_name <- function(code){
  styles <- data.frame(name=c("compromise","compete","accommodate","avoid","collaborate"),id=1:5)
  return (as.character(styles[code,"name"]))
}
# input: groups (vector of members)
#       type (compromising, competing, accommodating, avoiding, collaborating)
#       prob_eval, gamma
# output: list (member x (prob_prop; prob_eval))
generate_propose_eval_prob <- function(group, group_type, prob_eval, gamma){
  # group <-current_group
  # group_type <- current_style
  prob_prop <- 1/length(group)
  # gamma <- 0.1
  # prob_eval <- 0.5
  style_names <- get_style_name(group_type)
  res <- sapply(style_names,function(x){
    if(x=="compromise"){
      pp <- prob_prop
      pf <- prob_eval
    }else if(x=="compete" || x=="collaborate"){
      pp <- prob_prop + gamma
      pf <- prob_eval + gamma
    }else if(x=="accommodate" || x=="avoid"){
      pp <- prob_prop - gamma
      pf <- prob_eval - gamma
    }
    return (c(pp,pf))
  })
  #normalized: to sum to 1 (TODO: check if it is necessary to do this)
  res[1,] <- res[1,]/sum(res[1,])
  res[2,] <- res[2,]/sum(res[2,])
  
  return (res)
}

# input: a utility value of user 
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
  if(style_name=="compete" || style_name=="avoid"){
    if(b>1 && b!=0){
      b <- 1/b
    }
  }else if(style_name=="accommodate" || style_name=="collaborate"){
    if(b<1 && b!=0){
      b <- 1/b
    }
  }
  y1 <- (b^(x1)-1)/(b-1)
  pd <- y1
  y2 <- ((b^(x2)-1)/(b-1))
  pb <- 1 - y2
  pl <- y2 - y1
  names(pb) <- "best"
  names(pl) <- "like"
  names(pd) <- "dislike"
  #print (c(pb,pl,pd))
  return (c(pb,pl,pd))
}