\name{ann.model.counts}
\alias{ann.model.counts}
\alias{annc.model.counts}
\alias{annt.model.counts}
\alias{annsm.model.counts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate indices of number of birds caught on CES sites
}
\description{
These functions use \code{glm()} to model counts of birds caught as a function of year and CES site. Both site and year are treated as (fixed) factors. Year is usually considered to be annual but a linear trend or comparison with a block of recent previous years is also possible.
}
\usage{
ann.model.counts(x, year=-1, offset=TRUE, cl=0.95)
annc.model.counts(x, compare=1, offset=TRUE, cl=0.95)
annt.model.counts(x, year=-1, trend=100, offset=TRUE, cl=0.95)
annt.model.counts(x, offset=TRUE, cl=0.95)
}
\arguments{
  \item{x}{
  A CES data object usually constructed by extract.data()
}
  \item{year}{
  Reference year (i.e. the year when the index equals 1)
}
  \item{offset}{
  Logical, include the correction for missing visits when calculating indices?
}
  \item{cl}{
  Width of confidence limits. 0.95 is default, 0.83 limits don't overlap with (approximate) probability 0.95, so may be useful when comparing values of the index in specific years.
}
  \item{compare}{
  The number of most recent previous years with which to compare the most recent year. For example in 2008 setting compare=5 will set the index value averaged over the years 2003-07 to equal 1, and the 2008 value can then easily be compared to this. Earlier years are modelled as (fixed-effect) annual factors.
}
  \item{trend}{
  The number of most recent years through which to fit a linear trend through the index values; earlier years are modelled as (fixed-effect) annual factors. For example in 2008 setting trend=5 will fit a line through the years 2004-2008; earlier years are modelled as (fixed-effect) annual factors. If the number of years is greater than the number of years for which there are data, then a simple linear trend is fitted.
}
}
\details{
The basic modelling functions, which will generally be called through the wrapper function index(), since otherwise the 'ces' class may not be set, which may cause these functions to fail or behave unpredictably.
}
\value{
  A list containing the following items
  \item{model}{An lm object containing the model fit}
  \item{parms}{A data frame with the (year) parameter estimates and back-transformed annual indices}
  \item{test}{Details of a significance test for the comparison with previous years or of the linear trend}
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
\code{\link{index}}, \code{\link{ann.model.prod}}
}
\examples{
# Should not normally be called directly, use \code{\link{index}}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
