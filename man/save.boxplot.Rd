\name{save.boxplot}
\alias{save.boxplot}

\title{
Save CES boxplot dataset
}

\description{
Creates a datafile containing summary boxplot data to use as reference for site-specific plots}

\usage{
save.boxplot(data=NULL, annual=TRUE, species=TRUE, year=0, filename='bxp', ...)
}

\arguments{
  \item{data}{
a CES dataset created by \code{readces}}
  \item{annual}{
logical, if TRUE create boxplot summary data by year}
  \item{species}{
logical, if TRUE create boxplot summary data by species}
  \item{year}{
numeric, if species=TRUE, the year for which to create the summary}
  \item{filename}{
a character string to use as the filestem, see details}
  \item{\dots}{
further arguments passed to \code{siteplot}, possibly one or more of effort=, age=, subset= }
}

\details{
Saves the necessary data to create a boxplot summarising site-specific captures using \code{siteplot}. If annual=TRUE, the filename will expand to 'filename_year.RData', if species=TRUE then the data will be saved to 'filename_speciesxxxx.RData', where xxxx is the year. If year is not specified then the last year present in the dataset will be assumed. Data may be reloaded by using \code{load.boxplot}.
}

\value{
None. Used for its side effect of creating one or two (.RData) files.}

\author{
Rob Robinson}

\seealso{
\code{\link{siteplot}}, \code{\link{load.boxplot}}
}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x) 
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
