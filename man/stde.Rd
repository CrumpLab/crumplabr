\name{stde}
\alias{stde}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Standard Error
}
\description{
Computes the standard error of the mean
}
\usage{
stde(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
x<-c(1,2,3,4,5,4,3,4,)
stde(x)

## The function is currently defined as
function (x) 
sd(x)/sqrt(length(x))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ stde }
\keyword{ standard }% __ONLY ONE__ keyword per line
