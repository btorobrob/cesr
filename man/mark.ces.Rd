\name{mark.ces}
\alias{mark.ces}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Estimate adult survival from CES data.
}
\description{
  Fit CJS-with-transients model to estimate annual adult survival from CES data.
}
\usage{
  mark.ces(cesobj, exclude=NULL, type='+', trend=0, constant=0, compare=0, cleanup=TRUE)
}
\arguments{
  \item{cesobj}{
    A CES object with capture histories, typically created by \code{\link{extract.ch}}
  }
  \item{exclude}{
    A vector of sites to exclude from the survival modelling
  }
  \item{type}{
    How to model group-specific survivals, either additively '+' (the default) or in interaction ':', this requires the groups to have been specified in \code{\link{extract.ch}}
  }
  \item{trend}{
  An integer to indicate how many years to calculate a linear trend over. The last year of the trend will always be the last year in the dataset. If trend is greater than the number of years available then a simple linear trend will be fitted.
}
  \item{constant}{
  An integer indicating the last n years over which survival will be modelled as constant.
}
  \item{compare}{
  An integer indicating the how many n previous years to compare the last year's estimate to.
}
  \item{cleanup}{
    MARK creates a series of output files for each survival model fitted. If cleanup=FALSE, these will be stored in the sub-directory 'markfiles', if cleanup=TRUE they will be deleted. x$model$output indicates which files are associated with the survival model stored in x.
  }
}
\details{
  Calls MARK to fit a CJS-with-transients model for data extracted by \code{\link{extract.ch}}, see which for details of data inclusion. Currently, fits the a model in which probability of residency id constant across sites and years, survival is year-dependent and recapture rates are site dependent (with that within the first year being a fixed proportion of the annual rate). 
  
  If a grouping variable was specified when the data were extracted, survivals will be modelled in relation to this variable, either additively or interactively, according to 'type'.
  
  To fit a time-constant model ('phi(.)' in Mark notation) set 'constant' to be a number equal to the number of capture intervals in the data. If a number larger than that is given for 'constant'  or 'compare' then it will be adjusted accordingly (to n or n-1).
}
\value{
  A list consisting of model (the output returned by RMark), its AIC and (corrected) number of paramters (npar), and dataframes called residency, survival, recapture and recap1 with the parameter estimate, standard error and upper and lower confidence limits (as ucl, lcl) for each parameter.  
}
\author{
  Rob Robinson, with thanks to Christian Kamplicher
}
\note{
  Requires MARK (http://www.phidot.org/software/mark/downloads/index.html) to be installed (and the MarkPath variable to be set if it is not in the default location, see ?mark)
}

\examples{
\dontrun{
data(ukdata)
plots <- extract.coverage(ukdata)
robin.dat <- extract.ch(ukdata, species=10990, plots=plots)
robin.res <- mark.ces(robin.dat)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
\keyword{ regression }
