plglm <-
function(x, ylab='') {

  x <- x$parms
  
  # check for infinite values and warn
  if( any(x$ucl == Inf, na.rm=TRUE) ){
    x$ucl[x$ucl == Inf] <- max(x$ucl[x$ucl != Inf])
    warning(paste('Boundary estimates plotted for', ylab), call.=FALSE)    
  }
  
  ylim <- c(min(x$lcl, na.rm=TRUE), max(x$ucl, na.rm=TRUE))
  plot(x=x$years, y=x$index, type='l', xlab="", ylab=ylab, ylim=ylim, las=1)
  lines(x=x$years, y=x$lcl, lty=2)
  lines(x=x$years, y=x$ucl, lty=2)
  points(x=x$years, y=x$annual, pch=19, cex=0.5)

  x0 <- x$years
  y0 <- x$parm
  yus <- y0 + x$se 
  yls <- y0 - x$se 
  ylim <- c(min(yls, na.rm=TRUE), max(yus, na.rm=TRUE))
  plot(x=x$years, y=x$parm, type='p', xlab="", ylab="log Index", ylim=ylim, las=1)
  suppressWarnings(arrows(x0, y0, x0, yus, angle=90, length=0.05))  # issues warning when trying to add
  suppressWarnings(arrows(x0, y0, x0, yls, angle=90, length=0.05))  # arrows to refyear with se=0

}

