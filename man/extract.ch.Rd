\name{extract.ch}
\alias{extract.ch}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get adult capture history data for an individual species 
}
\description{
 Creates a list with individual capture histories for birds from a CES object created by \code{readces} for a survival analysis using \code{mark.ces}
}
\usage{
extract.ch(x, species=0, late=FALSE, group=NA, exclude=NULL, min.n=0, plots=NULL)
}
\arguments{
  \item{x}{
a CES object created by \code{readces}
}
  \item{species}{
a numeric Euring species code (including final zero which would indicate race)  
}
  \item{late}{
should late visits be considered in site selection? In UK (at least) most adults are caught in first 6 visits, so defaults to FALSE (ie a site is included if, say, 4 of the first 6 visits are made). Set to TRUE to require late visits to be considered also, which will results in more site-year combinations being considered as 'missing'
}
  \item{group}{
a character string indicating a grouping variable for survival, such as 'sex'. Group levels should be alphabetic only (i.e. no digits (0-9) or symbols please). 
}
  \item{exclude}{
one or more levels of 'group' to exclude, for example, '-' for unsexed birds   
}
  \item{min.n}{
minimum total number of captures for a site to be included, may be useful with large datasets to aid convergence or memory allocation
}
  \item{plots}{
an object containing plot coverage data created by \code{extract.coverage}. 
}
}
\details{
Creates a '10' style capture hisory for each ringed bird (if a bird moves site it is treated as a new individual). Missing visits are preserved and handled by MARK. The additional capture within the first season is entered by shifting the first capture back a 'year' and entering a 1 subsequently (ie in the first actual year of capture) if the bird is recaught within that year, 0 otherwise. Birds initially caught as juveniles are included if they are subsequently caught as adults (with the juvenile captures removed). 
}
\value{
A list of five items
  \item{chdata}{A data frame with columns: ring, ch and site}
  \item{begin.time}{A four digit integer indicaing the first year from which observations come}
  \item{years}{An integer indicating the number of years from which obserations come}
  \item{group}{a list consisting of name - character string indicating a grouping variable for survival or NA, and levels - a vector of the group levels}
  \item{spp.name}{Name of the species in whichever language has been set by \code{setceslang}}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Rob Robinson
}
\note{
The matching of site names assumes that the order in whch they appear in x is the original one, avoid sorting this dataframe. If histories are extracted by group, the \code{\link{mark.ces}} will always estimate survival by group, to avoid this you will need to re-extract the data without a grouping variable.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{mark.ces}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(ukdata)
plots <- extract.coverage(ukdata)
robin.dat <- extract.ch(ukdata, species=10990, plots=plots)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
