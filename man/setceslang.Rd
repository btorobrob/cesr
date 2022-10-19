\name{setceslang}
\alias{setceslang}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Set language for printing species names
}
\description{
Allows species names in outputs to be rendered in one of 14 languages.
}
\usage{
setceslang(lang)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lang}{
A string indicating a valid language, see details below. If no language is set most functions will default to using the scientific name}
}
\details{
Valid languages are "Latin", "Danish", "Dutch", "English", "Finnish", "French", "German", "Italian", "Norwegian", "Polish", "Portuguese", "Spanish", "Swedish". This is set by a call to options, use \code{options()$ceslang} to check which language is currently set.}
\value{
Sets global option 'ceslang'}
\author{
Rob Robinson
}
\note{
Names are case sensitive, that is they need to be capitalised. 
}

\section{Warning }{
Names are 'sticky', that is when an object is created, for example with \code{extract.data}, cesr uses the language set at the time and this cannot easily be changed subsequently.
} 

\examples{
setceslang("English")
}
\keyword{ sysdata }
