\name{calc.offset}
\alias{calc.offset}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate offset for productivity models
}
\description{
Calculates an offset to be used in models of productivity (i.e. juvenile:adult ratios) to account for missing visits. This function is not intended for public use but is included for clarity to demonstrate the calculation.
}
\usage{
calc.offset(x)
}
\arguments{
  \item{x}{
  A dataframe with columns of numbers of adults and juveniles actually caught and the numbers expected had all visits been completed.
}
}
\details{
At some sites visits are missed, as there is a seasonal bias to the extent to which adults (more early in the season) and juveniles (more late in the season) may be caught it is desirable to correct for these missing visits. This is basically done by reference to numbers caught in other years at that site in which all visits were completed. The correction is then simply the ratio of total number caught over all visits to the number caught on the equivalent visits that were made. If no years with complete visits are made at that site, then all sites are used in the comparison. The correction factor is:
  x$offset <- log( (x$jvcaps*x$adexcaps) / (x$adcaps*x$jvexcaps) )
where jvcaps and adcaps are the actual number of juvenile and adult birds caught and jvexcaps and adexcaps are the number expected to have been caught had the missing visits been completed, based on numbers caught on those visits in preious years. The calculation of the offset for the productivity models is complicated slightly by the fact that no adults or no juveniles may be have been caught, in which case the ratio of expected to actual juvenile or adult captures (respectively) is used.
}
\value{
	the original data frame with the column 'offset' added
}
\references{
\itemize{
\item {"Peach, W.J., Buckland, S.T. & Baillie, S.R. (1996) The use of constant effort mist-netting to measure between-year changes in abundance and productivity of common passerines. Bird Study 43:142-156.}
}
}
\author{
Rob Robinson
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{ann.model.prod}}
}
\examples{
Not called directly
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
