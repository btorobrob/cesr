get.estimate <- 
function(x, mtype, base=0, ndigits=2, cl=TRUE, change=FALSE){
  
  fmt <- paste0("%.", ndigits, "f")
  if( mtype == 'annual' | mtype == "compare" ){
    
    parms <- x[[1]]$parms
    tt <- x[[1]]$test$tsig

    year0 <- x$model.type$refyear
    fyear <- parms$years[1]
    
    if( parms$index[parms$years==year0] != 1 )
      parms[ , c('index', 'lcl', 'ucl')] <- parms[ , c('index', 'lcl', 'ucl')] / parms$index[parms$years==year0]
      
    cat(paste(mtype, ';', year0, ';', fyear))
    if( year0 == fyear ) { ## check if ref year is first year, so need to reverse comparison
      fyear <- max(parms$years, na.rm=TRUE)
      delta_ind <- parms[parms$years==fyear, c('index', 'lcl', 'ucl')]
    } else {
      delta_ind <- 1 / (parms[parms$years==fyear, c('index', 'lcl', 'ucl')])
      delta_ind <- delta_ind[c(1, 3, 2)] # need to reverse lcl, ucl
    }
    if( base==1 ){
      delta_ind[delta_ind < 1] <- round(-1 * (1-delta_ind[delta_ind < 1]), ndigits)
      delta_ind[delta_ind >= 1] <- round(1 * (delta_ind[delta_ind >= 1]-1), ndigits)
      delta_ind[delta_ind > 1000] <- '>10x'
    } else if( base==100 ){
      delta_ind[delta_ind < 1] <- round(-100 * (1-delta_ind[delta_ind < 1]), ndigits)
      delta_ind[delta_ind >= 1] <- round(100 * (delta_ind[delta_ind >= 1]-1), ndigits)
      delta_ind[delta_ind > 1000] <- '>1000%'
    }
    if( cl )
      entry <- paste0(sprintf(fmt,delta_ind[1]), ' (', sprintf(fmt,delta_ind[2]), ', ', sprintf(fmt,delta_ind[3]), ')')
    else 
      entry <- as.character(sprintf(fmt,delta_ind[1]))

  } else if( mtype =="constant" ){
    
    parms <- x[[1]]$test
    tt <- parms$tsig
    
    if( base == 0 ){
      est <- round(exp(parms$slope)-1, ndigits)
      lcl <- round(exp(parms$slope - 1.96 * parms$slope.se)-1, ndigits)
      ucl <- round(exp(parms$slope + 1.96 * parms$slope.se)-1, ndigits)
    } else if( change ){
      est <- round(base * exp(parms$slope) - base, ndigits)
      lcl <- round(base * exp(parms$slope - 1.96 * parms$slope.se) - base, ndigits)
      ucl <- round(base * exp(parms$slope + 1.96 * parms$slope.se) - base, ndigits)
    } else {
      est <- round(base * exp(parms$slope), ndigits)
      lcl <- round(base * exp(parms$slope - 1.96 * parms$slope.se), ndigits)
      ucl <- round(base * exp(parms$slope + 1.96 * parms$slope.se), ndigits)
    }

    if( cl )
      entry <- paste0(sprintf(fmt,est), ' (', sprintf(fmt,lcl), ', ', sprintf(fmt,ucl), ')')
    else
      entry <- as.character(sprintf(fmt, est))

  } else if( mtype == "trend" ){
     
    parms <- x[[1]]$test
    base <- max(base, 1) # just in case base=0
    
    est <- base * round(parms$slope, ndigits)
    tt <- parms$tsig

    if( cl ){
      lcl <- base * round(parms$slope - 1.96 * parms$slope.se, ndigits)
      ucl <- base * round(parms$slope + 1.96 * parms$slope.se, ndigits)
      entry <- paste0(sprintf(fmt,est), ' (', sprintf(fmt,lcl), ', ', sprintf(fmt,ucl), ')')
    } else
      entry <- sprintf(fmt, est)
  }            

  sig.star <- ifelse(tt>0.1, "_", ifelse(tt>0.05, ".", ifelse(tt>0.01, "+", "*")))

  return(paste(entry, sig.star))
}
      
