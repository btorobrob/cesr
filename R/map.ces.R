map.ces <-
function(cesobj, type='c', xlim=c(-20,30), ylim=c(35,70), pch=21, col=c('white','red'), 
         file=NULL, width=640, height=480, lwd=1, cex=1, units='px', mar=c(1,1,1,1), ...){
    
  # for pch: 21=circle, 22=square, 23=diamond, 24=up-triangle, 25=down-triangle
  
  oldpar <- par(list('pin', 'usr', 'mar'))
  on.exit(par(oldpar))
  
  if( class(cesobj)[1]!='ces' )
       stop("No ces data\n")
  if( class(cesobj)[2]=='data' ){
    x <- cesobj[!duplicated(paste0(cesobj$countryID, "_", cesobj$sitename)), ]
    x$current <- 1 # plot all sites same colour
  } else if( class(cesobj)[2]=='plots' ){
    x <- cesobj$sites
  } else if( class(cesobj)[2]=='sites' ){
    x <- cesobj
  } else {
    stop("No map data to plot\n")
  }
  #
  
  if( tolower(substr(type,1,1) == 'c') ) {  # Current vs non-current sites 
    col <- rep(col, length.out=2)
    x$col <- ifelse(x$current==0,col[1],col[2])
  } else if( tolower(substr(type,1,1) == 'n') ) {  # Colour by number of years
    if( class(cesobj)[2]=='data' )
      stop("need to suppy CES sites information\n")
    lq <- as.numeric(quantile(x$nyears,0.25))
    mq <- median(x$nyears)
    hq <- as.numeric(quantile(x$nyears,0.75))
    if (length(col) == 1) {
      x$col <- col
    } else if (length(col) == 2){
      x$col <- ifelse(x$nyears<median(x$nyears), col[1], col[2])
    } else if (length(col) == 3) {
      x$col <- col[2]
      x$col[x$nyears<lq] <- col[1]
      x$col[x$nyears>hq] <- col[3]
    } else {
      x$col <- ifelse(x$nyears<mq, col[2], col[3])
      x$col[x$nyears<lq] <- col[1]
      x$col[x$nyears>hq] <- col[4]
    }
  } else if( tolower(substr(type,1,1) == 'h') ) {  # Color by habitat
    nlvl <- length(levels(x$habitat))
    if (length(col) < nlvl) 
      stop(sprintf("please supply at least %d colors", nlvl))
    x$col<-col[as.numeric(x$habitat)]  # Pick colors by habitat factor level
  } else if( tolower(substr(type,1,1) == 's') ) { # Color by site
    x$col <- col
  } else
    x$col <- col[1]

  ftype <-'stdio'
  if ( !is.null(file) ){
    ftype <- tolower(unlist(strsplit(file,'\\.'))[2])        
    if ( ftype=='png' ) {
      png(file, width, height, units)
    } else if ( ftype=='jpg' | ftype=='jpeg' ){
      jpeg(file, width, height, units)
    } else if ( ftype=='tif' | ftype=='tiff' ){
      tiff(file, width, height, units)
    } else if ( ftype=='ps' ){
      if ( units=='px' ){
        width <- 1 + width/300  # add an inch because setting region size
        height <- 1 + height/300
      } else if ( units=='mm' ) {
        width <- width/25.4
        height <- height/25.4
      }
      postscript(file, width=width, height=height, title='Map of CES sites')
    } else {
      wtext <- paste('unrecognised file type:', ftype)
      warning(wtext)
      ftype <- 'stdio'
    }
  }

  par(usr=c(xlim, ylim))
  maps::map(xlim=xlim, ylim=ylim, mar=mar, ...)
  points(x=x$long, y=x$lat, pch=pch, bg=x$col, cex=cex, lwd=lwd)

  if( ftype != 'stdio' )
    dev.off()
  
}