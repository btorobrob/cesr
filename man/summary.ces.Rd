\name{summary.ces}
\alias{summary.ces}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summarizing CES objects
}
\description{
Methods to summarize CES objects
}
\usage{
summary(cesobj, ads=FALSE, jvs=FALSE, prod=FALSE, age=0, sp.order='alpha', df=FALSE, nrow=6, silent=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cesobj}{
A CES object
}
  \item{ads, jvs, prod}{
logical, if TRUE and x contains index results, print summary statistics for adult/juvenile abundance/productivity only.
}
  \item{age}{
Euring age code - either 3 juveniles or 4 for adults
}
  \item{sp.order}{
Should species be displayed in order of the 'alpha'bet, decreasing 'count' or, if neither specified, 'taxon'omic (Voous) order.
}
  \item{df}{
logical, if TRUE and x is CES data object created by \code{readces}, print traditional column summary statistics.
}
  \item{nrow}{
integer, if x is a species summary object, print this many rows, ordered by highest count.
}
  \item{silent}{
logical, if TRUE suppress printing of the species counts. May be useful if you just want to generate a table of species counts for further manipulation.
}
}
\details{
summary() will call the appropriate method as follows: 
If given an object created by \code{readces} it will print a table of number of each species caught (only those of 'age' if that argument is given) in 'sp.order'
If given an object created by \code{extract.data} it will provide a simple summary of the number of birds caught (or only those of 'age' if that argument is given) 
If given an object created by \code{extract.coverage} it will provide a summary of the number of plots operated in each year and the number of years plots have been operating
If given an object created by \code{index} it will provide summary statistics of the fit

Normally one will want to summarise results for both age-classes and productivity measure, but the facility is provided to do this for only one age-class if desired.
}
\value{
Invisibly, a dataframe (or a list of dataframes) containing the numerical data.
}
\references{
%% ~put references to the literature/web site here ~
}

\author{
Rob Robinson
}
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(ukdata)
summary(ukdata)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
