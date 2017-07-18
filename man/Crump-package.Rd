\name{Crump-package}
\alias{Crump-package}
\alias{Crump}
\docType{package}
\title{
Custom functions Crump
}
\description{
Custom functions for human cognition and performance lab.
Various helper functions for data analysis problems typical for cognitive psychology.
}
\details{
\tabular{ll}{
Package: \tab Crump\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2017-07-18\cr
License: \tab none\cr
}
This package currently contains

non_recursive_moving \cr
modified_recursive_moving \cr
stde\cr


}
\author{
Matthew Crump

Maintainer: Who to complain to <mcrump@brooklyn.cuny.edu>

}
\references{
Selst, M. V., & Jolicoeur, P. (1994). A solution to the effect of sample size on outlier elimination. The quarterly journal of experimental psychology, 47(3), 631-650.
}


\keyword{ package }
\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
x<-c(11,2,3,2,3,23,4,5,4,100)
non_recursive_moving(x)$original_rts
non_recursive_moving(x)$restricted
non_recursive_moving(x)$prop_removed
modified_recursive_moving(x)$original_rts
modified_recursive_moving(x)$restricted
modified_recursive_moving(x)$prop_removed
}
