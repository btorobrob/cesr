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
readces(file, visits='std', fix=FALSE, group.race=TRUE)
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
  \item{fix}{
Should presumed errors be fixed? See details.
}
  \item{group.race}{
Aggregate records of different races (i.e. those where the last digit of the Euring code is not zero)?
}
}
\details{
For details of the file specification see the CES pages of the Euring website, the function will attempt to match similar column names; a warning will be generated if it detects anything amiss (not guaranteed!). Some of these can be safely ignored (e.g. the 'surprising species' one) if everything appears to be in order.

If fix=TRUE then the function will change records as follows: 
- sex of all encounters for an individual will be assigned the commonest sex recorded for that species (if the sex on encounter is recorded as unknown, or the other sex).
}
\value{
A dataframe with (at a minimum) the following columns: countryID (where the scheme is), sitename(as referred by the scheme), site (a sequential numeric identifier), habitat, visit (number), day, month, year, netlength, scheme (of the ring), ring, species, sex, age, race, julian, lat, long. Unrecognised columns in the datafile will be removed. 

Latitude and longitude are in decimal degrees. Race will usually be the same as species (i.e. a Euring number), unless particular races were identified in the data file (e.g. as for Motacilla alba alba/yarrelli, see group.race=). The column 'julian' gives the day of year from 1 Jan (including 29 Feb in leap years). The function does some basic error-checking and will try to give informative messages where it detects a problem. Where records are deleted or values set to NA (missing) a warning will be given.  
}

\references{
Euring CES pages: https://euring.org/research/ces-europe/
}
\author{
Rob Robinson
}
\note{
The CES programs expect birds aged as either juvenile (3) or adult (4), the function will recode ages where it can, 2 is also allowed; records with other ages (0, 1) will be deleted. Expect some warnings, most are there for information only- they do not necessarily indicate an error. 

readBTO() is provided for reading in BTO '.all' fixed format files, variables not included in this format (eg habitat, NetLength) are simply set to NA.
}

\examples{
\dontrun{
ukdata <- readces("d:/data/ces/ukdata.csv")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IO }
