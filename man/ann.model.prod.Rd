\name{ann.model.prod}
\alias{ann.model.prod}
\alias{annc.model.prod}
\alias{annt.model.prod}
\title{
Calculate indices of productivity caught on CES sites
}
\description{
These functions use \code{glm()} to model the productivity of birds caught as a function of year and CES site. Productivity is calculated as the proportion of juveniles caught. Both site and year are treated as (fixed) factors, but a linear trend or comparison with a block of recent previous years is also possible.
}
\usage{
ann.model.prod(x, year=-1, offset=TRUE, cl=0.95)
annc.model.prod(x, compare=1, offset=TRUE, cl=0.95)
annt.model.prod(x, year=-1, trend=100, offset=TRUE, cl=0.95)
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
  \item{offset}{
  Width of confidence limits. 0.95 is default, 0.83 limits don't overlap with probability 0.95, so may be useful when comparing values of the index in specific years.
}
  \item{compare}{
  The number of most recent previous years with which to compare the most recent year. For example in 2008 setting compare=5 will set the index value averaged over the years 2003-07 to equal 1, and the 2008 value can then easily be compared to this. Earlier years are modelled as (fixed) annual factors.
}
  \item{trend}{
  The number of most recent years through which to fit a linear trend through the index values; earlier years are modelled as (fixed-effect) annual factors. For example in 2008 setting trend=5 will fit a line through the years 2004-2008; earlier years are modelled as (fixed-effect) annual factors. If the number of years is greater than the number of years for which there are data, then a simple linear trend is fitted.
}
}
\details{
The basic modelling functions, should generally be called through the wrapper function index(), since otherwise the 'ces' class may not be set, which will cause these functions to fail or behave unpredictably.
Productivity is calculated as the proportion of juveniles caught using a binomial glm (that is with a logit link), the index is however presented as the number of juveniles caught per adult. This is achieved by using the inverse-log to back-transform the index values rather than the inverse-logistic transform, see Robinson et al. (2007). Additionally, missing visits are accounted for by an offset as described in that paper.
Note this is probably not a good measure (even relatively) of the number of young birds hatched on a particular site, rather it is an indication of productivity over some wider area since juveniles may disperse from a very young age.
}
\value{
  A list containing the following items
  \item{model}{An lm object containing the model fit}
  \item{parms}{A data frame with the parameter estimates and back-transformed indices}
  \item{test}{Details of a significance test for the comparison with previous years or of the linear trend}
}
\references{
Robinson, R.A., Freeman, S.N., Balmer, D.E. & Grantham, M.J. 2007. Cetti's Warbler: analysis of an expanding population. Bird Study 54:230-235.
}
\author{
Rob Robinson
}
\note{
Should not normally be called directly use \code{\link{index}}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{index}}, \code{\link{ann.model.counts}}, \code{\link{calc.offset}}
}
\examples{
%
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
