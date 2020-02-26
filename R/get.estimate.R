get.estimate <- 
function(x, mtype, ndigits=2, cl=TRUE){
  
  if( mtype == 'annual' ){
    
    parms <- x[[1]]$parms

    year0 <- x$model.type$refyear
    fyear <- parms$years[1]
    
    if( year0 == fyear ) {     # check if ref year is first year, so need to reverse comparison
      fyear <- max(parms$years, na.rm=TRUE)
      delta_ind <- parms[parms$years==fyear, c('index', 'lcl', 'ucl')]
    } else {
      delta_ind <- 1 / (parms[parms$years==fyear, c('index', 'lcl', 'ucl')])
      delta_ind <- delta_ind[c(1, 3, 2)] # need to reverse lcl, ucl
    }
    delta_ind[delta_ind < 1] <- round(-100 * (1-delta_ind[delta_ind < 1]), ndigits)
    delta_ind[delta_ind >= 1] <- round(100 * (delta_ind[delta_ind >= 1]-1), ndigits)
    delta_ind[delta_ind > 1000] <- '>1000%'
    if( cl )
      entry <- paste0(delta_ind[1], ' (', delta_ind[2], ', ', delta_ind[3], ')')
    else 
      entry <- as.character(delta_ind[1])

  } else if( mtype =="constant" ){
    
    parms <- x[[1]]$test
    
    est <- round(100 * exp(parms$slope), ndigits)
    lcl <- round(100 * exp(parms$slope - parms$slope.se), ndigits)
    ucl <- round(100 * exp(parms$slope + parms$slope.se), ndigits)
    
    if( cl )
      entry <- paste0(est, ' (', lcl, ', ', ucl, ')')
    else
      entry <- as.character(est)
    
  } else if( mtype == "trend" ){
     
    parms <- x[[1]]$test
    est <- round(parms$slope, ndigits)
    
    if( cl ){
      lcl <- round(parms$slope-1.96*parms$slope.se, ndigits)
      ucl <- round(parms$slope+1.96*parms$slope.se, ndigits)
      entry <- paste0(est, ' (', lcl, ', ', ucl, ')')
    } else
      entry <- as.character(est)
  }            

   return(entry)
}
      
