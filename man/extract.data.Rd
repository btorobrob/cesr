\name{extract.data}
\alias{extract.data}
\title{
Get data on an individual species 
}
\description{
 Creates dataframes with capture data for adult and juvenile birds from a CES object created by \code{readces}
}
\usage{
extract.data(cesdata, species=0, plots=NULL, min.yrs=1)
}
\arguments{
  \item{cesdata}{
a CES object created by \code{readces}.
}
  \item{species}{
Euring species code (including the final zero which would indicate race).  
}
  \item{plots}{
an object containing plot coverage data created by \code{extract.coverage}. 
}
  \item{min.yrs}{
sites covered in fewer than min.yrs will be ignored. 
}
}

\value{
A list of up to four items
  \item{spp }{Euring species code}
  \item{spp.name }{Name of the species in whichever language has been set by \code{setceslang}}
Either or, more usually, both of the following may be present
  \item{ad.data }{data frame with summary of adult captures}
  \item{jv.data }{data frame with summary of juvenile captures}
}
\author{
Rob Robinson
}
\examples{
data(ukdata)
plots <- extract.coverage(ukdata)
robin.dat <- extract.data(ukdata, species=10990, plots=plots)
}
\keyword{ manip }
