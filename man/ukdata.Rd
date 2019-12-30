\name{ukdata}
\alias{ukdata}
\docType{data}
\title{
A sample CES dataset
}
\description{
A sample CES dataset from the UK of two species (Erithacus rubecula and Turdus philomelos) from 19 sites created by \code{readces}
}
\usage{data(ukdata)}
\format{
  A data frame with 4,879 observations on the following 15 variables.
  \describe{
    \item{\code{countryID}}{a factor with levels \code{GBT} - the scheme identifier using Euring three-letter codes}
    \item{\code{siteID}}{a site identifier, numeric in this case, may be character for other schemes }
    \item{\code{habitat}}{a factor with levels \code{} \code{DS} \code{RB} \code{WD} \code{WS} - for Dry/Thorn Scrub, Reedbed, Woodland and Wet Scrub respectively}
    \item{\code{visit}}{numeric - 1:12 for UK }
    \item{\code{day}}{numeric}
    \item{\code{month}}{numeric - 1:12}
    \item{\code{year}}{numeric - four digits}
    \item{\code{NetLength}}{numeric - length of net erected (in metres)}
    \item{\code{scheme}}{a factor with levels \code{GBT} - of ring, allows for foreign controls}
    \item{\code{ring}}{a factor with levels \code{012083} \code{012098} \code{012100} ... }
    \item{\code{species}}{numeric - Euring numeric, aggregates races so will end in zero}
    \item{\code{sex}}{a factor with levels \code{F} \code{M} \code{U}}
    \item{\code{age}}{numeric - either 3 (juvenile) or 4 (adult)}
    \item{\code{sitename}}{a site identifier, numeric in this case, may be character for other schemes, identical to siteID }
    \item{\code{site}}{numeric - a site identifier, with sites numbered 1:n }
    \item{\code{race}}{numeric - Euring code as in orginal datafile, may specify races}
    \item{\code{lat}}{numeric - latitude in decimal degrees}
    \item{\code{long}}{numeric - longitude in decimal degrees, East of the Greenwich meridian is positive}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(ukdata)
summary(ukdata)
plot(ukdata)

}
\keyword{datasets}
