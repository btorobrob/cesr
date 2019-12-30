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
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
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
\references{
%% ~put references to the literature/web site here ~
}
\author{
Rob Robinson}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(chaff)
sites <- extract.sites(chaff)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
