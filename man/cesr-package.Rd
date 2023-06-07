\name{cesr-package}
\alias{cesr-package}
\alias{cesr}
\docType{package}
\title{
Trend analysis of Constant Effort Site ringing data
}
\description{
Facilities for reading EuroCES data in Euring format to produce annual indices of productivity, abundance and survival. Primarily designed to analyse data from schemes with 9-12 visits per year following Peach et al. (1996, 1998) and Robinson et al. (2007). 
}
\details{
\tabular{ll}{
Package: \tab cesr\cr
Type: \tab Package\cr
Version: \tab 1.0.2\cr
Date: \tab 2023-06-07\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
}
Constant Effort Site (CES) ringing data provide a valuable way to monitor changes in abundance, productivity and survival through standardised capture and recapture of individuals.  \code{cesr} provides some facilities to produce annual trends in abundance and productivity from these data.  In order to do this \code{cesr} requires CES data organised in a plain text file according to the agreed Euring format (details of which can be found on the Euring website (http://www.euring.org/internal/ces_research_group/euro_ces_meeting_2008_notes.pdf).  

The key steps are:
(i) to read in the data (with \code{\link{readces}}); 

(ii) generate a table of which plots are covered in which year (with \code{\link{extract.coverage}});

(iii) extract data for the species to be analysed (with \code{\link{extract.data}}), and 

(iv) generate the annual abundance/productivity indices (with \code{\link{index}}). 

Adult survival probabilities can be estimated by first using \code{\link{extract.ch}} then \code{\link{mark.ces}}. Results can be plotted using \code{\link{plot_trend}}. More details on how to do each of these can be found in the introductory manual, type  

vignette('cesr-intro')

to view it.
}
\author{
Maintainer: Rob Robinson <rob.robinson@bto.org>
}
\references{
\itemize{
\item "Peach, W.J., Buckland, S.T. & Baillie, S.R. (1996) The use of constant effort mist-netting to measure between-year changes in the abundance and productivity of common passerines. Bird Study 43:142-156."
\item "Peach, W.J., Baillie, S.R. & Balmer, D.E. (1998) Long-term changes in the abundance of passerines in Britain and Ireland as measured by constant effort mist-netting. Bird Study 45:257-275."
\item "Robinson, R.A., Freeman, S.N., Balmer, D.E. & Grantham, M.J. (2007) Cetti's Warbler: anatomy of an expanding population. Bird Study 54:230-235."
\item "Robinson, R.A., Julliard, R & Saracco, J.F. (2009) Constant effort: studying avian population processes using standardized ringing. Ringing & Migration 24:199-204."
}
}
\keyword{ package }
\examples{                                               
\dontrun{
# A sample session might look as follows:
setwd('d:/data/ces')  # set the working directory which contains the data file 
setceslang('English')  # which language for species names? 

alldat <- readces('netherlands.csv')  # read in the data
plot.nl <- extract.coverage(alldat)  # determine which sites covered in which years
sonth.nl <- extract.data(alldat, species=12000,plots=plot.nl)  # extract data for Song Thrush
st.fit0 <- index(sonth.nl, year=2008)  # fit an annual index
st.fit1 <- index(sonth.nl, trend=30)  # fit a simple linear trend

writeces(st.fit0, 'sonthdata.txt')  # write annual indices to a file
}
}