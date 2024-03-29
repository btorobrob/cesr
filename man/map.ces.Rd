\name{map.ces}
\alias{map.ces}
\title{
Map CES Sites
}
\description{
Plot CES sites on a simple outline map
}
\usage{
map.ces(cesobj, type='c', xlim=c(-20,30), ylim=c(35,70), pch=21, col=c('white','red'), file=NULL, width=640, height=480, lwd=1, cex=1, units='px', mar = c(1, 1, 1, 1), ...)
}
\arguments{
  \item{cesobj}{
A CES object with site information, usually generated by extract.sites()
}
  \item{type}{
How to identify sites: currently 'c' for current/non-current, 'n' for number of years operating, 'h' for habitat, and 's' for site-specific colours
}
  \item{xlim}{
Minimum and maximum longitude bounds, note R will make this 'pretty'
}
  \item{ylim}{
Minimum and maximum latitude bounds, note R will make this 'pretty'
}
  \item{pch}{
A numeric value (21-25) identifying symbol type, see ?pch, but 21=circles, 22=squares
}
  \item{col}{
A vector of colors to fill points, see type
}
  \item{lwd}{
if pch=21:25, weight of point borders, useful for highlighting e.g. current sites 
}
  \item{cex}{
size of points
}
  \item{file}{
A valid filename to plot the map to, identified by extension, current options recognised are .png, .jpg (or .jpeg), .tif (or .tiff) and .ps
}
  \item{width, height}{
size of map (normally in pixels)
}
  \item{units}{
Units for the size, use 'mm' if outputting to pdf/postscript file
}
  \item{...}{
Additional arguments passed to \code{map}, such as \code{border} and \code{lty} that control the color and line type of the polygon borders if \code{fill=TRUE} is specified.
}

}
\details{
The default is to color points by whether they are current (second element of col) or historic - the latest year present is taken to represent the 'current' year. If type='n' then the plot will depend on the number of colors, two will separate sites below/above the median number of years operated; three the bottom quartile, the middle 50 percent and the upper quartile (approximately) and four will split by quartile. Any expansion of 'Current', 'No Years', 'Habitat' should work (only the first letter is significant and case is not important).
}
\value{
None
}
\author{
Rob Robinson
}
\examples{
data(ukdata)
sites<-extract.sites(ukdata)
map.ces(sites)
}
\keyword{ hplot }
