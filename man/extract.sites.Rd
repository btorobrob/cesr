\name{extract.sites}
\alias{extract.sites}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extracts CES site information
}
\description{
Generates a table of site information
}
\usage{
extract.sites(cesdata, exclude=NULL)
}
\arguments{
  \item{cesdata}{
  A CES object created by \code{readces}
}
  \item{exclude}{
  A vector of sitenames to be excluded
}
}
\details{
Currently mainly used to generate data for \code{\link{map.ces}}.
}
\value{
A dataframe with site (the internal numeric identifier), sitename, habitat, lat and long (in decimal degrees), netlength, number of years operated (nyears), first and last year of operation, whether the site is current, and the number of missing years in the data run.
}
\author{
Rob Robinson}
\examples{
data(ukdata)
sites <- extract.sites(ukdata)
}
\keyword{ manip }
