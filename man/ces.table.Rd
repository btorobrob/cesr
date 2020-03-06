\name{ces.table}
\alias{ces.table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create a table of CES results
}
\description{
Runs the index() function over multiple species to create a simple table of results
}
\usage{
ces.table(cesobj, species=NA, columns=c("A-0", "P-0", "S-0"), plots=NULL, min.n=100, conf.lim=TRUE, ndigits=2, year=-1, visit.corr=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cesobj}{
a CES dataset created by \code{readces().}
}
  \item{species}{
a vector of numeric Euring codes. If missing species with more than min.n records will be extracted from the supplied dataset.
}
  \item{columns}{
specification for the trends required, see details.
}
  \item{plots}{
a dataset of CES sites created by \code{extract.coverage()}. If missing, one will be created including all sites and years.
}
  \item{min.n}{
if species is missing, the minimum number of records for a species to be included in the analysis.
}
  \item{conf.lim}{
logical, include (95\%) confidence limits in the table output?
}
  \item{ndigits}{
the number of decimal places to round the results to.
}
  \item{year}{
the reference year for comparisons, if not the first.
}
  \item{visit.corr}{
logical, correct counts for missing visits or not? 
}

}
\details{
This function runs index() multiple times to create a table of trends for each species. Each row of the table represents a species, each column a trend that is specified by the 'columns' argument. This is a three letter combination as follows: the first character is the initial letter specifying the measure to be calculated A(dult) or J(uvenile) numbers, P(roductivity) or S(urvival). The second character is either a hyphen ('-') to indicate a comparison with previous years, or a slash ('/') to indicate a trend. This is followed by a number to indicate either the number of previous years with which to compare the current year, or the number of years (counting back from the last year) over which to calculate the trend. Thus, 'A/10' calulates the trend in number adult numbers over the previous ten years, and 'S-5' compares the survival in the current year with the previous five years. The default is to compare to the previous year for adult numbers, productivity and survival. The entries in the table will either be a percentage change from the previous period, or the estimate of the slope (i.e. per year change) on either the log (numbers) or logit (productivity and survival) scale.
}
\value{
a list with two components: 'table' is a character matrix with each of the table entries; 'results' is a list containing the output of \code{index} for each table entry.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Rob Robinson}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{index}}
}
\examples{
## Not run:
data(ukdata)
# compare adult numbers in the current yaer to the previous year, 
# survival with the previous five years and a 10-year trend in productivity
results <- ces.table(ukdata, columns=c('A-0', 'S-5', 'P/10'))
## End (not run)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
