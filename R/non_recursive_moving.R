non_recursive_moving<-function(rts){
  xsize <- c(4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 20, 
             25, 30, 35, 50, 100)
  stds <- c(1.458, 1.68, 1.841, 1.961, 2.05, 2.12, 2.173, 
            2.22, 2.246, 2.274, 2.31, 2.326, 2.391, 2.41, 2.4305, 
            2.45, 2.48, 2.5)
  if(length(rts>=100)){
    sdc=2.5
  }else{
    sdc<-approx(xsize,stds,xout=length(rts))$y
  }
  mean_rts<-mean(rts)
  restricted_rts<-rts[rts > mean_rts - (sd(rts)*sdc) &
                        rts < mean_rts + (sd(rts)*sdc)]
  
  list(original_rts=rts,restricted=restricted_rts,prop_removed=(1-(length(restricted_rts)/length(rts))))
}