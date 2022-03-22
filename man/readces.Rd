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
readces(file, visits='std', fill.sex=FALSE, group.race=TRUE)
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
Aggregate records of different races (i.e. those where the last digit of the Euring code is not zero)?
}
}
\details{
For details of the file specification see the CES pages of the Euring website, the function will attempt to match similar column names; a warning will be generated if it detects anything amiss (not guaranteed!). Some of these can be safely ignored (e.g. the 'surprising species' one) if everything appears to be in order.
}
\value{
A dataframe with (at a minimum) the following columns: countryID, site, habitat, visit, day, month, year, netlength, scheme, ring, species, sex, age, race, julian, lat, long. 

Latitude and longitude will be in decimal degrees. Race will usually be the same as species (i.e. a Euring number), but may be different where particular races were identified in the data file (e.g. as for Motacilla alba alba/yarrelli). The column 'julian' gives the day of year from 1 Jan (including 29 Feb in leap years). The function does some basic error-checking and will try to give informative messages where it detects a problem. Where records are deleted or values set to NA (missing) a warning will be given.  
}

\references{
Euring CES pages: https://euring.org/research/ces-europe/
}
\author{
Rob Robinson
}
\note{
The CES programs expect birds aged as either juvenile (3) or adult (4), the function will recode ages where it can, 2 is also allowed; records with other ages (0, 1) will be deleted.

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
