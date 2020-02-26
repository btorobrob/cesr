\name{extract.coverage}
\alias{extract.coverage}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Identify CES sites covered in each year
}
\description{
  Creates a list which identifies which CES sites are covered in each year and the number of missing visits. 
}
\usage{
extract.coverage(cesdata, early=NA, late=NA, min.visits=1, all.visits=0, exclude=list(years=NULL, sites=NULL))
}
\arguments{
  \item{x}{
  A CES object created by \code{readces}
}
  \item{early, late}{
  Numeric vectors of length 2, the first giving the number of visits in the first/second period, the second the number of visits required in each for the site to be included. They will be reversed if necessary.
}
  \item{all.visits}{
  The number of visits that should be made in the season. If missing will default to the highest visit number in the dataset.
}
  \item{min.visits}{
  The minimum number of visits that should be made in the season. An alternative to specifying early/late.
}
  \item{exclude}{
  A list with one or two components listing sites or years that should be excluded from consideration.
}
}
\details{
  Creates a list containing information on coverage of sites in each year. For a site to be considered covered in a given year a certain number of visits (min.visits) must be made. Most adults will be caught earlier in the season, most juveniles later; thus an additional constraint is imposed in that a certain number (y) of the first x visits, and similarly of later visits, should also be made. This can be specified using early=c(x,y), and similarly late=c(x,y); for example, in Britain, four of the first six and four of the last six visits are required so these would be specified as c(6,4). This has the advantage that if a site is only covered in the early period of the year, say, it can still be used for calculating adult abundance.  
}
\value{
A list containing three objects
  \item{sites}{A dataframe consisting of site details including: habitat, latitude, longitude, net length and first/last year of operation. 
}
  \item{years}{A dataframe consisting of a site by year matrix and identifying whether early and late periods were covered.
}
  \item{coverage}{A dataframe identifying number of visits in each period and whether each site/year should be included in the trend calculations.
}
  \item{missing.visits}{A dataframe identifying which visits are missing across sites and years
}
  \item{all.visits}{An integer specifying the number of visits that should be made.
}
  \item{min.visits}{An integer specifying the minimum number of visits that should be made.
}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Rob Robinson
}
\note{
The function calculates coverage from the catch data, thus if no birds are caught on a visit that visit will not be registered; hopefully this will be an infrequent occurrence!
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(ukdata)
sites.cov<-extract.coverage(ukdata)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
