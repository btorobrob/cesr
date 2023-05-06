plot_trend <-
function(x, type='', group=NULL, file=NULL, width=480, height=480, units='px', main=FALSE, lty=c(1,2), lcol='black', lwd=1, line=NA, rlty=3, rlwd=1, rcol='black', annual=FALSE, pch=19, pcol='black', pcex=1, ...){  
    
  if( type=='' & class(x)[2]=='markfit' ) 
    type <- 'Survival' # survival objects should only be one thing, so can get way with assuming

  select <- tolower(substr(type, 1, 1))
  
  if( ! select %in% c('a', 'j', 'p', 's') )
    stop('Invalid type: please specify A(dult), J(uvenile) abundance, P(roductivity) or adult S(urvival)')
  
  if( length(lty) == 1 )
    lty <- rep(lty, 2)
  if( length(lwd) == 1 )
    lwd <- rep(lwd, 2)
  if( length(lcol) == 1 )
    lcol <- rep(lcol, 2)
  else if( length(lcol) > 2 )
    lcol <- lcol[1:2]
  
  ftype <- 'stdio'
  ## start the device driver
  if( !is.null(file) ){
    ftype <- tolower(unlist(strsplit(file,'\\.'))[2])        
      if( ftype == 'png' ) {
        png(file, width, height, units)
      } else if( ftype=='jpg' | ftype=='jpeg' ){
        jpeg(file, width, height, units)
      } else if( ftype=='tif' | ftype=='tiff' ){
        tiff(file, width, height, units)
      } else if( ftype=='ps' ){
        if( units=='px' ){
          width <- width / 300  
          height <- height / 300
        } else if( units=='mm' ) {
          width <- width / 25.4
          height <- height / 25.4
        }
        if( height < 2 | width < 2)
          warning('figure dimensions unexpectedly small, check the units?')
        postscript(file, width=width, height=height)      
      } else if( ftype=='pdf' ){
        if( units=='px' ){
          width <- 1 + width/300  # add an inch because setting region size
          height <- 1 + height/300
        } else if( units=='mm' ) {
          width <- width/25.4
          height <- height/25.4
        }
        if( height < 2 | width < 2)
          warning('figure dimensions unexpectedly small, check the units?')
        pdf(file, width=width, height=height)      
      } else if( ftype=='svg' ){
        if( units=='px' ){
          width <- width/300  
          height <- height/300
        } else if( units=='mm' ) {
          width <- width/25.4
          height <- height/25.4
        }
        if( height < 2 | width < 2)
          warning('figure dimensions unexpectedly small, check the units?')
        svg(file, width=width, height=height)      
      } else {
        wtext <- paste('unrecognised file type:', ftype)
        warning(wtext, call.=FALSE)
        ftype <- 'stdio'
      }
  }
  
  ## plot the graph
  if( select == 'a' ) {
    res <- x$ad.results$parms
    ylab <- "Adult Abundance"
  } else if( select == 'j' ) {
    res <- x$jv.results$parms
    ylab <- "Juvenile Abundance"
  } else if( select == 'p' ) {
    res <- x$pr.results$parms
    ylab <- "Productivity"
  } else if( select == 's' ) {
    if( is.null(group) ){
      res <- x$survival
    } else {
      res <- x$survival[x$survival$group==group, ]
    }
    res$index <- res$estimate
    ylab <- "Adult Survival"
  }

  # check the dots for useful plotting arguments
  plot.args <- list(...)
  plot.args$x <- res$years
  plot.args$y <- res$index
  if( !any(grep("ylim", names(plot.args))) )
    plot.args$ylim <- range(res[ , c('index', 'lcl', 'ucl')], na.rm=TRUE)
    # include index in case a single site is fitted (so cl's would be NaN)
  if( !any(grep("xlim", names(plot.args))) )
    plot.args$xlim <- c(min(res$years), max(res$years))
  if( !any(grep("ylab", names(plot.args))) )
    plot.args$ylab <- ylab
  if( !any(grep("xlab", names(plot.args))) )
    plot.args$xlab <- ""
  if( !any(grep("las", names(plot.args))) )
    plot.args$las <- 1
  if ( main == TRUE )
    plot.args$main <- x$spp.name
  else if( main != FALSE )
    plot.args$main <- main
  plot.args$lwd <- lwd[1]
  plot.args$type <- 'l'
  
  do.call(plot, plot.args)

  xlim <- plot.args$xlim
  if( (xlim[2]-xlim[1]) <= 10 ) # if only a few years have a tick per year...
    xtx <- seq(xlim[1], xlim[2], 1)
  else { # ...else major ticks every five years
    if( (xlim[1]%%5) %in% c(1,2) )
      xlim[1] <- xlim[1]-(xlim[1]%%5) # round down if more than 2 minor ticks
    if( (xlim[2]%%5) > 2 )
      xlim[2] <- xlim[2]-(xlim[2]%%5)+5 # round up if more than 2 minor ticks
    xtx <- seq(xlim[1]-(xlim[1]%%5), xlim[2]+5, 5)
  }
  
  cex.axis <- ifelse(any(grep("cex.axis", names(plot.args))), plot.args$cex.axis, 1)
  axis(1, at=xtx, cex.axis=cex.axis) # add in the major ticks
  axis(1, at=seq(xlim[1],xlim[2],1), labels=FALSE, tcl=par("tcl")*0.5) # now the minor ones
  lines(x=res$years, y=res$lcl, lty=lty[2], col=lcol[2], lwd=lwd[2])
  lines(x=res$years, y=res$ucl, lty=lty[2], col=lcol[2], lwd=lwd[2])
  if( annual == TRUE ){
    if( length(which(names(res)=='annual')) > 0 )
      points(res$years, res$annual, pch=pch, col=pcol)
    else
      points(res$years, res$index, pch=pch, col=pcol, cex=pcex)
  }
  if( !is.na(line) )
    abline(h=line, lty=rlty, col=rcol, lwd=rlwd)
  
  ## close the device driver
  if( ftype != 'stdio' )
    dev.off()

  invisible(res)
}
    
      
      