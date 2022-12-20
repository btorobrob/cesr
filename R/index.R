index <-
function(cesdata, year=-1, begin=0, smooth=FALSE, trend=0, compare=0, verbose=FALSE, visit.corr=TRUE, cl=0.95){

  if ( !class(cesdata)[1] == 'ces' | !class(cesdata)[2] == "counts" )
    stop("No ces capture information\n")
  
  # get data for sites covered in more than one year
  ad.data <- cesdata$ad.data[cesdata$ad.data$nyears > 1, ]
  jv.data <- cesdata$jv.data[cesdata$jv.data$nyears > 1, ]
  
  if ( year == -1 ) 
    year <- max(ad.data$year, jv.data$year, na.rm=TRUE)
  
  if( begin > min(ad.data$year, jv.data$year, na.rm=TRUE) ){ # truncate the start of the data
    ad.data <- ad.data[ad.data$year >= begin, ]
    jv.data <- jv.data[jv.data$year >= begin, ]
  }
  
  # check in case there are no captures in a given year
  if ( (length(ad.data)>0 & length(ad.data[ad.data==year])==0) |
       (length(jv.data)>0 & length(jv.data[jv.data==year])==0) ){ 
    oldyear <- year
    if ( length(ad.data) > 0 )
      t1 <- as.numeric(names(table(ad.data$year[ad.data$include]))) 
    if ( length(jv.data) > 0 )
      t2 <- as.numeric(names(table(jv.data$year[jv.data$include])))
    if ( exists('t1', inherits=FALSE) ){
      if ( exists('t2', inherits=FALSE) ){
        year <- max(t1[t1 %in% t2], na.rm=TRUE)
      } else {
        year <- max(t1, na.rm=TRUE)
      }
    } else {
      year <- max(t2, na.rm=TRUE)
    }
    wmessage <- paste('no captures in', oldyear, 'for one age-class, so setting', year, 'to 1 instead', sep=' ')
    warning(wmessage, call.=FALSE)
  }  
  
  if( (length(table(ad.data$site)) == 1) |  (length(table(jv.data$site)) == 1) )
    warning('Only one site detected', immediate.=TRUE, call.=FALSE)
  
  if ( smooth == TRUE ) {
    mtype <- list(type='smooth', refyear=year)

    if ( nrow(ad.data) > 0 )
      ad.res <- annsm.model.counts(ad.data, offset=visit.corr, cl=cl)
    if ( nrow(jv.data) > 0 ) 
      jv.res <- annsm.model.counts(jv.data, offset=visit.corr, cl=cl)
    if ( nrow(jv.data)>0 & nrow(ad.data)>0 ){
      data <- list(ad.data=ad.data, jv.data=jv.data)
      pr.res <- annsm.model.prod(data, offset=visit.corr, cl=cl)
    }  

  } else {
    
    if ( trend > 0 & compare > 0 )
      stop("Specify only one of 'trend' or 'compare'\n")
    
    ad.res <- numeric(0)
    jv.res <- numeric(0)
    
    if ( trend > 0 ) {
      mtype <- list(type='trend', refyear=year, nyrs=trend)
      if ( nrow(ad.data) > 0 )
        ad.res <- annt.model.counts(ad.data, year, trend, offset=visit.corr, cl=cl)
      if ( nrow(jv.data) > 0 ) 
        jv.res <- annt.model.counts(jv.data, year, trend, offset=visit.corr, cl=cl)
      if ( nrow(jv.data)>0 & nrow(ad.data)>0 ){
        data <- list(ad.data=ad.data, jv.data=jv.data)
        pr.res <- annt.model.prod(data, year, trend, offset=visit.corr, cl=cl)
      }
    } else if ( compare > 0 ) {
      mtype <- list(type='constant', refyear=year, nyrs=compare)
      if ( nrow(ad.data) > 0 )
        ad.res <- annc.model.counts(ad.data, compare, offset=visit.corr, cl=cl)
      if ( nrow(jv.data) > 0 )
        jv.res <- annc.model.counts(jv.data, compare, offset=visit.corr, cl=cl)
      if ( nrow(jv.data)>0 & nrow(ad.data)>0 ){
        data <- list(ad.data=ad.data, jv.data=jv.data)
        pr.res <- annc.model.prod(data, compare, offset=visit.corr, cl=cl)
      }
    } else {
      mtype <- list(type='annual', refyear=year, nyrs=0)
      if ( nrow(ad.data) > 0 )
        ad.res <- ann.model.counts(ad.data, year, offset=visit.corr, cl=cl)
      if ( nrow(jv.data) > 0 )
        jv.res <- ann.model.counts(jv.data, year, offset=visit.corr, cl=cl)
      if ( nrow(jv.data)>0 & nrow(ad.data) > 0 ){
        data <- list(ad.data=ad.data, jv.data=jv.data)
        pr.res <- ann.model.prod(data, year, offset=visit.corr, cl=cl)
      }
    }
  }  
  
  if ( !exists('ad.res', inherits=FALSE) )
    ad.res <- NA
  if ( !exists('jv.res', inherits=FALSE) )
    jv.res <- NA
  if ( !exists('pr.res', inherits=FALSE) )
    pr.res <- NA
  
  res <- list(ad.results=ad.res, jv.results=jv.res, pr.results=pr.res, 
              model.type=mtype, spp=cesdata$spp, spp.name=cesdata$spp.name, limits=cl)
  class(res) <- c('ces','glmfit')
  
  if ( verbose == TRUE ){
    writeces.glmfit(res, file=stdout())
  } else {
    summary(res)
  }
  
  invisible(res)
}

