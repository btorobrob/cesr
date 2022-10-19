\name{check_rings}
\alias{check_rings}
\title{
Check ringing details
}
\description{
Currently just checks for rings used on multiple species
}
\usage{
check_rings(cesdata)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cesdata}{
A CES data object
}
}
\details{
Checks where multiple species are associated with a single number number. Scheme is included as part of the ring number.
}
\value{
A dataframe containing a column of ring numbers and the species that they were ringed as; NULL if none are found. Only those rings which have apparently been put on more than one species are included.
}
\author{
Rob Robinson
}
\keyword{ utilities }
