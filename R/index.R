index <-
function(x, year=-1, smooth=FALSE, trend=0, compare=0, verbose=FALSE, visit.corr=TRUE, cl=0.95){

  if ( !class(x)[1] == 'ces' | !class(x)[2] == "counts" )
    stop("No ces capture information\n")
  
  if ( year == -1 ) 
    year <- max(x$ad.data$year, x$jv.data$year)
  
  # check in case there are no captures in a given year
  if ( (length(x$ad.data)>0 & length(x$ad.data[x$ad.data==year])==0) |
       (length(x$jv.data)>0 & length(x$jv.data[x$jv.data==year])==0) ){ 
    oldyear <- year
    if ( length(x$ad.data) > 0 )
      t1 <- as.numeric(names(table(x$ad.data$year[x$ad.data$include]))) 
    if ( length(x$jv.data) > 0 )
      t2 <- as.numeric(names(table(x$jv.data$year[x$jv.data$include])))
    if ( exists('t1', inherits=FALSE) ){
      if ( exists('t2', inherits=FALSE) ){
        year <- max(t1[t1%in%t2])
      } else {
        year <- max(t1)
      }
    } else {
      year <- max(t2)
    }
    wmessage <- paste('no captures in', oldyear, 'for one age-class, so setting', year, 'to 1 instead', sep=' ')
    warning(wmessage, call.=FALSE)
  }  
  
  if( (length(table(x$ad.data$site)) == 1) |  (length(table(x$jv.data$site)) == 1) )
    warning('Only one site detected', immediate.=TRUE, call.=FALSE)
  
  if ( smooth == TRUE ) {
    
    x=x # placeholder
    
  } else {
    
    if ( trend > 0 & compare > 0 )
      stop("Specify only one of 'trend' or 'compare'\n")
    
    ad.res <- numeric(0)
    jv.res <- numeric(0)
    
    if ( trend > 0 ) {
      mtype <- list(type='trend', refyear=year, nyrs=trend)
      if ( nrow(x$ad.data) > 0 )
        ad.res <- annt.model.counts(x$ad.data, year, trend, offset=visit.corr, cl=cl)
      if ( nrow(x$jv.data) > 0 ) 
        jv.res <- annt.model.counts(x$jv.data, year, trend, offset=visit.corr, cl=cl)
      if ( nrow(x$jv.data)>0 & nrow(x$ad.data)>0 )
        pr.res <- annt.model.prod(x, year, trend, offset=visit.corr, cl=cl)
    } else if ( compare > 0 ) {
      mtype <- list(type='constant', refyear=year, nyrs=compare)
      if ( nrow(x$ad.data) > 0 )
        ad.res <- annc.model.counts(x$ad.data, compare, offset=visit.corr, cl=cl)
      if ( nrow(x$jv.data) > 0 )
        jv.res <- annc.model.counts(x$jv.data, compare, offset=visit.corr, cl=cl)
      if ( nrow(x$jv.data)>0 & nrow(x$ad.data)>0 )
        pr.res <- annc.model.prod(x, compare, offset=visit.corr, cl=cl)
    } else {
      mtype <- list(type='annual', refyear=year, nyrs=0)
      if ( nrow(x$ad.data) > 0 )
        ad.res <- ann.model.counts(x$ad.data, year, offset=visit.corr, cl=cl)
      if ( nrow(x$jv.data) > 0 )
        jv.res <- ann.model.counts(x$jv.data, year, offset=visit.corr, cl=cl)
      if ( nrow(x$jv.data)>0 & nrow(x$ad.data) > 0 )
        pr.res <- ann.model.prod(x, year, offset=visit.corr, cl=cl)
    }
  }  
  
  if ( !exists('ad.res') )
    ad.res <- NA
  if ( !exists('jv.res') )
    jv.res <- NA
  if ( !exists('pr.res') )
    pr.res <- NA
  
  res <- list(ad.results=ad.res, jv.results=jv.res, pr.results=pr.res, 
              model.type=mtype, spp=x$spp, spp.name=x$spp.name, limits=cl)
  class(res) <- c('ces','glmfit')
  
  if ( verbose == TRUE ){
    writeces.glmfit(res, file=stdout())
  } else {
    summary(res)
  }
  
  invisible(res)
}

