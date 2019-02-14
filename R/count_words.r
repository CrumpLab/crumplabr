count_words <- function(x){
  return(length(unlist(strsplit(x,split=" "))))
}