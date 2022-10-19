\name{writeces}
\alias{writeces}
\title{
Write CES data to a file 
}
\description{
Provides a wrapper function to write CES data objects to a comma-separated text file 
}
\usage{
writeces(cesobj, file = "", na.string = ".", age = 0, ads = FALSE, jvs = FALSE, prod = FALSE, verbose=TRUE)
}
\arguments{
  \item{cesobj}{
a CES object
}
  \item{file}{
A valid filename
}
  \item{na.string}{
Character(s) to use to represent missing values in the output file
}
  \item{age}{
Euring age code - 3 for juvenile or 4 for adult
}
  \item{ads, jvs, prod}{
logical, if TRUE write out data for adult/juvenile abundance/productivity only.
}
  \item{verbose}{
logical, if TRUE write out individual visit data writing plot data.
}
}
\details{
Write an object of class CES to a comma-separated text file. Use argument 'age' to just write juvenile or adult data. If outputting the results of a model fit from \code{index}, normally one will want to summarise results for both age-classes and productivity measure, but the facility is provided to do this for one age-class  only if desired (by setting ads, jvs or prod equal to TRUE respectively).
%%  ~~ If necessary, more details than the description above ~~
}
\author{
Rob Robinson
}
\examples{
\dontrun{
data(ukdata)
robin.dat=extract.data(ukdata,species=10990)
writeces(robin.dat, file="d:\\data\\robin.csv")
}
}
\keyword{ IO }
