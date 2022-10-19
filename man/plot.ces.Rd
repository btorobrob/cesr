\name{plot.ces}
\alias{plot.ces}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot CES Objects
}
\description{
Methods to quickly plot CES objects of different types.
}
\usage{
\method{plot}{ces}(x, sites = FALSE, graph = "X", N=20, sitelist=NULL, col=c('red', 'blue'), ...)
}
\arguments{
  \item{x}{
A CES object
}
  \item{sites}{
Logical. If TRUE, for objects creates by \code{extract.data()}, plots a summary of number of sites recording a species, if FALSE plots summary of number of captures
}
  \item{graph}{
Graph type for plotting results of \code{index()}: currently 'a' for adults, 'j' for juveniles, 'p' for productivity and 'x' for all three, letters may be lower or upper case.
}
  \item{N}{
Integer. If plotting an object created by \code{readces()} the number of species/sites to be included.
}
  \item{sitelist}{
Character vector. If plotting an object created by \code{extract.ch()} sites to be included, otherwise all are.
}
  \item{col}{
Character vector. If plotting an object created by \code{extract.ch()} colors to plot non-capture and capture occasions respectively.
}
  \item{...}{
Additional arguments, see details
}
}
\details{
Plots an appropriate summary graph (or graphs) for a ces object. 

For 'raw' data (created by \code{readces}), a summary of captures by year, visit, site and species (for the top N sites/species only) with bars indicating the inter-quartile range across years. 

For 'plot' data (created by \code{extract.coverage}), the number of sites operating in each year, and the number of sites operating for a particular number of years.

For 'count' data (created by \code{extract.data}), the number of adults and juveniles caught of the species in each year (black bars indicate the number of expected birds had there been no missing visits) and the cumulative number of birds caught where sites are ranked from those catching the most to fewest.

For abundance/productivity results (created by \code{index}) graphs of the annual values and parameter estimates; a single trend can be produced using the 'graph=' argument, but use \code{plot.trend()} for prettier output. Will also accept 'ylab' and 'title'.

For capture history ('ch') data  (created by \code{extract.ch}), a plot of the capture histories, grouped by site, each line represents a group of individuals with the same encounter history, colored by whether they were caught or not on each occasion; darker lines indicate more frequent histories. Also accepts xlab, ylab, cex (on the y-axis, possibly to fit more site labels in) and main.  
}
\value{
None
}

\author{
Rob Robinson, the ch plot was inspired by one of Marc Kery's.
}
\note{
In the ch plots, because birds are possibly caught twice in their first year, 'Occasion' (the x-axis) is not quite the same as Year. 
}

\seealso{
\code{\link{plot.trend}}
}
\examples{
data(ukdata)
plots <- extract.coverage(ukdata)
robin.dat <- extract.data(ukdata, species=10990, plots=plots)
plot(robin.dat)
}
\keyword{ hplot }
