.onLoad <-
function(lib, pkg){
  write("Welcome to cesr 0.41, use help(cesr) to get started","")
  utils::data(cesnames, package="cesr")
  setceslang('Latin')
  
  repos <- ifelse( is.null(getOption('repos')), 'http://cran.rstudio.com', getOption('repos') )
  
  # required packages because not on CRAN, the order matters...
#  pkgnames=c('reshape','data.table','RMark')
#  for( i in 1:length(pkgnames) ){
#    pkg <- pkgnames[i]
#    if( suppressMessages(require(pkg, quietly=TRUE, character.only=TRUE))==FALSE ){      
#      suppressPackageStartupMessages(install.packages(pkg, repos=repos, quiet=TRUE))
#      suppressMessages(require(pkg, quietly=TRUE, character.only=TRUE))
#    }
#  }
}

  