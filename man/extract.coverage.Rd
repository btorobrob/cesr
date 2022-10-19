\name{extract.coverage}
\alias{extract.coverage}
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
  \item{cesdata}{
  A CES object created by \code{readces}
}
  \item{early, late}{
  Numeric vectors of length 2, the first giving the number of visits in the first/second period, the second the number of visits required in each for the site to be included. They will be reversed if necessary.
}
  \item{min.visits}{
  The minimum number of visits that should be made in the season. An alternative to specifying early/late.
}
  \item{all.visits}{
  The number of visits that should be made in the season. If missing will default to the highest visit number in the dataset with a warning (see details).
}
  \item{exclude}{
  A list with one or two components listing sites or years that should be excluded from consideration.
}
}
\details{
  Creates a list containing information on coverage of sites in each year. For a site to be considered covered in a given year a certain number of visits (min.visits) must be made. Most adults will be caught earlier in the season, most juveniles later; thus an additional constraint is imposed in that a certain number (y) of the first x visits, and similarly of later visits, should also be made. This can be specified using early=c(x,y) and late=c(x,y); for example, in Britain, four of the first six and four of the last six visits are required so these would be specified as c(6,4). This has the advantage that if a site is only covered in the early period of the year, say, it can still be used for calculating adult abundance. Although normally the specification is likely to be the same for bother periods, it doesn't have to be (e.g. if you have an odd number of visits), and the periods can overlap if required.
  
  It is wise to specify all.visits explicitly, if not the highest visit number will be taken from the dataset. This can cause problems if, say, most sites do nine visits, but one site has done ten in one year, meaning that only that one season is identified as 'complete' and greatly inflating the number of 'missing' visits. The function will try to warn if it thinks this has happened.
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
\author{
Rob Robinson
}
\note{
The function calculates coverage from the catch data, thus if no birds are caught on a visit that visit will not be registered; hopefully this will be an infrequent occurrence!
}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
data(ukdata)
sites.cov<-extract.coverage(ukdata)
}
\keyword{ manip }
