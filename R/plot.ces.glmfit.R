plot.ces.glmfit <-
function(x, graph='X', ...){

  args <- as.list(substitute(list(...)))[-1L]

  ngraph <- 0
  if( length(x$ad.results) == 3 ) 
    ngraph <- ngraph + 1
  if( length(x$jv.results) == 3 ) 
    ngraph <- ngraph + 1
  if( length(x$pr.results) == 3 ) 
    ngraph <- ngraph + 1
  if( ngraph == 0 )
    stop("No CES data to plot\n")

  op <- par(no.readonly=TRUE) # get current par settings and store
  
  # print graphs for all three parameters
  if( graph == 'X' ) {
    dataset <- NULL # only return a single dataset
    
    par(mfrow=c(ngraph,2), mar=c(2,4,1,1), oma=c(1,1,2,1)) 
    if( length(x$ad.results)==3 ){
      plglm(x=x$ad.results, ylab="Rel. Abundance: Adults", ... )
    }
    if ( length(x$jv.results)==3 ){
      plglm(x=x$jv.results, ylab="Rel. Abundance: Juvs", ... )
    }
    if ( length(x$pr.results)==3 ){
      plglm(x=x$pr.results, ylab="Productivity Index", ... )
    }
    title(main=sprintf("Annual indices for %s",x$spp.name), outer=TRUE)
    
  }
  
  # print just a single graph
  else{
    ylab <- ifelse(length(which(names(args)=='ylab')) == 0, NA, args$ylab)
    title <- ifelse(length(which(names(args)=='title')) == 0, NA, args$title)
    
    if ( tolower(substr(graph,1,1)) == 'a' ) {  
      dataset <- x$ad.results
      if( is.na(ylab) )
        ylab <- "Rel. Abundance: Adults"
      if( is.na(title) )
        title <- gettextf("Annual Index of Adult Abundance for %s", x$spp.name)
    } else if ( tolower(substr(graph,1,1)) == 'j' ) {  
      dataset <- x$jv.results
      if( is.na(ylab) )
        ylab <- "Rel. Abundance: Juvs"
      if( is.na(title) )
        title <- gettextf("Annual Index of Juvenile Abundance for %s", x$spp.name)
    } else if ( tolower(substr(graph,1,1)) == 'p' ) {  
      dataset <- x$pr.results
      if( is.na(ylab) )
        ylab <- "Productivity Index"
      if( is.na(title) )
        title <- gettextf("Annual Index of Productivity for %s", x$spp.name)
    }
 
    par(mfrow=c(1,1), mar=c(2,4,1,1), oma=c(1,1,2,1)) 
    plglm(dataset, ylab=ylab) 
    title(main=title, outer=TRUE)
    
  }

  par(op)
  if( !is.null(dataset) )
    invisible(dataset$parms)
  
}

