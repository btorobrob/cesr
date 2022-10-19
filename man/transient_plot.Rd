\name{transient_plot}
\alias{transient_plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Visualise capture frequency
}
\description{
CES sites may have an excess of individuals that are caught only once, and so potentially transient to the site. This function plots individuals by whether they were captured subsequently or not.
}
\usage{
transient_plot(cesobj, col=c('blue', 'red'), border='gray10', leg.size=0.8, ...)
}

\arguments{
  \item{cesobj}{
A CES object created by \code{extract.ch}
}
  \item{col}{
Character vector. Colors to plot re-captured and not re-captured individuals respectively.
}
  \item{border}{
Character. Color for the border of the bars.
}
  \item{leg.size}{
Numeric. Size of the legend text.
}
  \item{...}{
Additional parameters passed to \code{legend}.
}
}
\details{
CES sites may have an excess of individuals that are caught only once, and so potentially transient to the site. This function plots individuals by whether they were captured subsequently or not, specifically whether they were captured twice in their first year (and so likely to be resident), captured only once in their first year (but caught again in a later year, and so also resident), and individuals caught only once. These latter may be transients (individuals passing through, or living on the very edge of the catching area, so with low capture probability) or resident individuals that happened not to be recaptured. Two barplots are produced, one classifying individuals by their year of first capture, and the second by site. Transients have the otential to bias survival estimates low as, once they leave the site (and so can no longer be captured), the models see them as dead. The function to calculate survival (\code{mark.ces}) tries to estimate the proportion of these individuals in the sample.
}
\value{
A list with three elements. (i) years - the matrix necessary to draw the years barplot, (ii) sites - the matrix necessary to draw the sites barplot, and (iii) the species name.
}
\author{
Rob Robinson
}
\keyword{ utilities }
