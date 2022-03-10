\name{spp_check}
\alias{spp_check}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Check for rings used on multiple species
}
\description{
}
\usage{
spp_check(cesdata)
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
\references{
%% ~put references to the literature/web site here ~
}

\author{
Rob Robinson
}
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line