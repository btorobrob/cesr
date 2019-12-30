\name{extract.data}
\alias{extract.data}
\title{
Get data on an individual species 
}
\description{
 Creates dataframes with capture data for adult and juvenile birds from a CES object created by \code{readces}
}
\usage{
extract.data(x, species, plots)
}
\arguments{
  \item{x}{
a CES object created by \code{readces}
}
  \item{species}{
Euring species code (including the final zero which would indicate race)  
}
  \item{plots}{
an object containing plot coverage data created by \code{extract.coverage}. 
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A list of up to four items
  \item{spp }{Euring species code}
  \item{spp.name }{Name of the species in whichever language has been set by \code{setceslang}}
Either or, more usually, both of the following may be present
  \item{ad.data }{data frame with summary of adult captures}
  \item{jv.data }{data frame with summary of juvenile captures}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Rob Robinson
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(ukdata)
plots <- extract.coverage(ukdata)
robin.dat <- extract.data(ukdata, species=10990, plots=plots)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
