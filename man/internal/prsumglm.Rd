\name{prsumglm}
\alias{prsumglm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Prints results of glm fit
}
\description{
Prints results of glm's fitted to CES data. Not for public consumption, called by \code{summary.ces.glmfit}
}
\usage{
prsumglm(x, mtype)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Results of a glm fit possibly from \code{index}
}
  \item{mtype}{
Model type for statistical testing, currently either NULL< 'trend' or 'constant'
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
Rob Robinson
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
