count_words <- function(x){
  if(length(x)==1){
    return(length(unlist(strsplit(x,split=" "))))
  } else if (length(x) > 1) {
    return(unlist(lapply(x,
                         FUN = function(y) return(length(unlist(strsplit(y,split=" ")))))))
  }
}