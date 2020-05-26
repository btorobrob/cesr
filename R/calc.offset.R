calc.offset <-
function(x) {   # Done as separate function more for clarity than anything else
  
  # calculate offset for missing visits, but need check for cases where no ad or juv captures
  x$jvzero <- ( x$jvcaps == 0 ) 
  x$adzero <- ( x$adcaps == 0 )
  x$offset <- log( (x$jvcaps*x$adexcaps) / (x$adcaps*x$jvexcaps) ) # correction factor

  # now fill in NaN because no ads or juvs caught
  x$offset[x$jvzero] <- log(x$adexcaps[x$jvzero]/x$adcaps[x$jvzero]) #  no juvs caught, so use adult corr factor
  x$offset[x$adzero] <- log(x$jvexcaps[x$adzero]/x$jvcaps[x$adzero]) #  no ads caught, so use juv corr factor
  x$offset[x$jvzero & x$adzero] <- 0   # no birds caught, should be excluded anyway?
  
  return(x)
}

