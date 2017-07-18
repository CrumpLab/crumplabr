\name{non_recursive_moving}
\alias{non_recursive_moving}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Non-recursive with moving criterion (Van Selst & Jolicoeur, 1994)
}
\description{
Uses the non-recursive procedure with moving criterion to eliminate outliers in reaction time data
}
\usage{
non_recursive_moving(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a vector of reaction times}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{

\item{original_rts}{returns a vector of original unrestricted sample}
\item{restricted}{returns a vector with retained RTs after outliers removed}
\item{prop_removed}{returns proportion of outliers removed}

}
\references{
Selst, M. V., & Jolicoeur, P. (1994). A solution to the effect of sample size on outlier elimination. The quarterly journal of experimental psychology, 47(3), 631-650.
}
\author{
Matthew Crump - Brooklyn College of CUNY
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

x<-c(11,2,3,2,3,23,4,5,4,100)
non_recursive_moving(x)$original_rts
non_recursive_moving(x)$restricted
non_recursive_moving(x)$prop_removed

## The function is currently defined as
non_recursive_moving<-function(rts){
  xsize <- c(4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 20, 
             25, 30, 35, 50, 100)
  stds <- c(1.458, 1.68, 1.841, 1.961, 2.05, 2.12, 2.173, 
            2.22, 2.246, 2.274, 2.31, 2.326, 2.391, 2.41, 2.4305, 
            2.45, 2.48, 2.5)
  if(length(restricted_rts>=100)){
    sdc=2.5
  }else{
    sdc<-approx(xsize,stds,xout=length(restricted_rts))$y
  }
  mean_rts<-mean(rts)
  restricted_rts<-rts[rts > mean_rts - (sd(rts)*sdc) &
                        rts < mean_rts + (sd(rts)*sdc)]
  
  list(original_rts=rts,restricted=restricted_rts,prop_removed=(1-(length(restricted_rts)/length(rts))))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~outlier }
\keyword{ ~non_recursive_moving }% __ONLY ONE__ keyword per line
