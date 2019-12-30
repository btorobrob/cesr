plot.ces.plots <-
function(x){

  if ( class(x)[1] == 'ces' ){
    if ( class(x)[2] == 'plots' ){
      if (.Platform$OS.type == "windows") {  # /dev/null has different names, just sweeps up output from summary
        sink('NUL')
      } else {
        sink('/dev/null')
      }
      x <- summary(x)
      sink() # restart normal outputs
    } else if ( class(x)[2]=='plot.summary' ) {
      x <- x
    } else 
      stop("No site information\n")
  } else 
    stop("Please supply a ces object\n")
  
  cumtot <- cumsum(x$cumtots$years)

  op <- par(mfcol=c(2,2), mar=c(2,4,1,1), oma=c(1,1,2,1))
  barplot(table(x$years$year), ylab="No. Sites", xlab="", las=1)
  plot(x=x$cumtots$years$year, y=x$cumtots$years$Freq, type='l', ylab="Cumulative Sites", xlab="Year", las=1)
  barplot(x$plots, ylab="", xlab="", las=1)
  plot(x=c(0,attr(x$cumtots$length,'names')), y=c(0,x$cumtots$length), type='l', xlab="No. Years Operated", ylab="", las=1)
  tot.sites <- data.table::uniqueN(x$sites$site)
  title(main=sprintf("Number of CE sites (Total=%d)",x$total), outer=TRUE)
  
  par(op)
  invisible(x)
}

