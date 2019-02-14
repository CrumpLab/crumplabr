\name{pipe_print}
\alias{pipe_print}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
pipe_print
}
\description{
Print something in a pipe, and let the data pass through to the next part of the pipe
}
\usage{
pipe_print(x)
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
df %>%
 pipe_print(df) %>%
 summarise()


## pipe_print <- function(data) {print(data); data}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ pipe_print }

