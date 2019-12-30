\name{plot.trend}
\alias{plot.trend}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot CES trend
}
\description{
Plots an estimated trend from CES data either to the screen or to a file.
}
\usage{
plot.trend(x, type, group=NULL, file = NULL, width=480, height=480, units='px', ylab='', ylim=c(0,0), xlim=c(0,0), line=NA, lty=1, annual=FALSE, pch=19, p.col='black', ...)
}
\arguments{
  \item{x}{
A CES object
}
  \item{type}{
Which trend to plot? Can be either 'Adult' or 'Juvenile' Abundance, 'Productivity' or 'Survival'. Only the first letter needs to be given, in either lower or upper case.
}
  \item{group}{
If survival trends have been calculated by group, the group level to be plotted.
}
  \item{file}{
A valid file name, ending in either '.png', '.jpg', '.jpeg', '.tif', '.tiff', '.ps' or '.pdf' to produce a file of the appropriate type. If file=NULL (the default) the trend will be plotted to the screen.
}
  \item{width}{
The width of the device.
}
  \item{height}{
The height of the device.
}
  \item{units}{
The units in which height and width are given. Can be px (pixels, the default), in(ches), cm or mm.
}
  \item{ylab}{
an optional label for the y-axis, if the default is not appropriate.
}
  \item{ylim}{
optionally, limits for the y-axis. The default is the minimum value for lower and upper confidence limits, use this to, for example, set the axis for survival probabilities to (0,1).
}
  \item{xlim}{
optionally, limits for the x-axis. If there are more than 10 years a sensible range should be chosen, with major ticks every 5 years, otherwise there will be a tick per year.
}
  \item{line}{
if required, the value at which a horizontal reference line will be drawn.
}
  \item{lty}{
if a reference lis to be drawn the line type to use.
}
  \item{annual}{
logical, plot the annual estimates too?
}
  \item{pch}{
integer, if annual=TRUE, symbol to use in plotting points.
}
  \item{pcol}{
color to use in plotting the annual points.
}
\item{...}{
additional arguments for controlling the plot, such as col or lwd.
}
}
\details{
Plots a single trend to a file, or to the screen if no file is not specified. PNG format is small and lossless and so good for images of this type. Invisibly returns a dataframe with the annual values used to create the graph. If you wish a finer degree of control over the appearance of the graph, best import this data to your favourite graphing package, or \code{\link{plot}} this data directly in R.
}
\value{
If assigned to an object, a dataframe with the following columns: 'years', 'parm', 'se', 'index', 'lcl', 'ucl'. 
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
