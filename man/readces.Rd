\name{readces}
\alias{readces}
\alias{readBTO}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read in CES data
}
\description{
Reads in CES data from a text file in EuroCES format
}
\usage{
readces(file, visits='std', fill.sex=FALSE, group.race=FALSE)
readBTO(file, species)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
A valid filename
}
  \item{visits}{
Either 'std' to include only standard (numbered) visits, or a (character) vector listing allowed visit identifiers.  
}
  \item{species}{
A numeric Euring species code
}
  \item{fill.sex}{
Should all encounters be populated with the most frequent sex recorded?
}
  \item{group.race}{
Aggregate records of different races (ie those where the last digit of the Euring code is not zero)?
}
}
\details{
For details of the file specification see the CES pages of the Euring website, the function will attempt to match column names (on the first four characters); a warning will be generated if it detects anything amiss (not guaranteed!). 
}
\value{
A dataframe with the following columns: countryID, site, habitat, visit, day, month, year, NetLength, scheme, ring, spp (species code), sex, age, race, lat, long. Latitude and longitude will be in decimal degrees. Race will usually be the same as spp (i.e. a Euring number), but may be different if a race was identified in the data file (e.g. as for Motacilla alba alba/yarrelli). The function does some basic error-checking and will try to give informative messages where it detects a problem. Where records are deleted or values set to NA (missing) this will be stated.  
}
\references{
Euring CES pages: \link{https://euring.org/research/ces-europe/}
}
\author{
Rob Robinson
}
\note{
The CES programs expect birds aged as either juvenile (3) or adult (4), the function will recode ages where it can, records with other ages (0, 1, 2) will be deleted.

readBTO() is provided for reading in BTO '.all' fixed format files, variables not included in this format (eg habitat, NetLength) are simply set to NA.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## Not run:
ukdata <- readces("d:/data/ces/ukdata.csv")
## End (not run)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
