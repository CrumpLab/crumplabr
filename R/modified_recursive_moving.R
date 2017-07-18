modified_recursive_moving<-function(rts){
  xsize <- c(4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 20, 
             25, 30, 35, 50, 100)
  stds <- c(8.00, 6.2, 5.3, 4.8, 4.475, 4.25, 4.11, 
            4.00, 3.92, 3.85, 3.80, 3.75, 3.64, 3.595, 3.55, 
            3.54, 3.51, 3.5)
  restricted_rts<-rts
  stop=FALSE
  while(stop==FALSE){
    if(length(restricted_rts>=100)){
      sdc=3.5
    }else{
      sdc<-approx(xsize,stds,xout=length(restricted_rts))$y
    }
    temporary_exclusion<-max(restricted_rts)
    descriptives<-c(mean(restricted_rts[restricted_rts<temporary_exclusion]),sd(restricted_rts[restricted_rts<temporary_exclusion]))
    lower_cutoff<-descriptives[1]-(descriptives[2]*sdc)
    upper_cutoff<-descriptives[1]+(descriptives[2]*sdc)
    retained_rts<-restricted_rts
    if(min(restricted_rts)<lower_cutoff){
      retained_rts<-restricted_rts[restricted_rts>min(restricted_rts)]
    }
    if(max(restricted_rts)>upper_cutoff){
      retained_rts<-restricted_rts[restricted_rts<max(restricted_rts)]
    }
    
    #check for breaking loop
    if(length(retained_rts)==length(restricted_rts)){
      stop<-TRUE
      restricted_rts<-retained_rts
    } else if (length(restricted_rts)<4){
      stop<-TRUE
      restricted_rts<-retained_rts
    }else{
      restricted_rts<-retained_rts
    }
  }
  list(original_rts=rts,restricted=restricted_rts,prop_removed=(1-(length(restricted_rts)/length(rts))))
}