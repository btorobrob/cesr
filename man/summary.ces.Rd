\name{summary.ces}
\alias{summary.ces}
\alias{summary.ces.ch}
\alias{summary.ces.counts}
\alias{summary.ces.glmfit}
\alias{summary.ces.markfit}
\alias{summary.ces.plots}
\alias{summary.ces.sites}
\alias{summary.ces.spp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summarizing CES objects
}
\description{
These functions provides summary \code{methods} for objects with class CES
}
\usage{
summary.ces.ch(x)
summary.ces.counts(x)
summary.ces.glmfit(x, ads, jvs, prod)
summary.ces.markfit(x)
summary.ces.plots(x)    
summary.ces.sites(x)
summary.ces.spp(x, age=0, sp.order='alpha', df=FALSE, nrow=6, silent=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A CES object
}
  \item{age}{
Euring age code - either 3 juveniles or 4 for adults
}
  \item{sp.order}{
If creating a summary of species caught should they be displayed in order of the 'alpha'bet, decreasing 'count' or, if neither specified, 'taxon'omic (Voous) order.
}
  \item{ads}{
logical, if TRUE print summary statistics for adult abundance only.
}
  \item{jvs}{
logical, if TRUE print summary statistics for juvenile abundance only.
}
  \item{prod}{
logical, if TRUE print summary statistics for productivity only.
}
  \item{df}{
logical, if TRUE print traditional column summary statistics.
}
  \item{nrow}{
integer, if x is a species summary object, print this many rows, ordered by highest count.
}
  \item{silent}{
logical, if TRUE suppress printing of the species counts. May be useful if you just want to generate a table for further manipulation.
}
}
\details{
summary() will call the appropriate method as follows: 
If given an object created by \code{readces} it will print a table of number of each species caught (only those of 'age' if that argument is given) in 'sp.order'
If given an object created by \code{extract.data} it will provide a simple summary of the number of birds caught (or only those of 'age' if that argument is given) 
If given an object created by \code{extract.coverage} it will provide a summary of the number of plots operated in each year and the number of years plots have been operating
If given an object created by \code{index} (or one of the model functions it calls) it will provide summary statistics of the fit

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
