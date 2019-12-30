\name{load.boxplot}
\alias{load.boxplot}

\title{
Load CES boxplot dataset
}

\description{
Loads a datafile containing summary boxplot data to use as reference for site-specific plots
}

\usage{
load.boxplot(filename=NULL, name='bxpdata')
}

\arguments{
  \item{filename}{
the name of the file containing the summarised data, typically ending in 'year.Rdata' or '_speciesxxxx.RData'
}
  \item{name}{
the name of the object to store the boxplot data in
}
}

\value{
A list with three components: 
  \item{ds}{the (subsetted) dataset}
  \item{ds.bxp}{data for the boxplot in summary form}
  \item{ds.attr}{some useful attributes}
}

\author{
Rob Robinson}

\note{
No error checking is done so make sure you load the right file!
}

\seealso{
\code{\link{save.boxplot}}, \code{\link{siteplot}}
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
