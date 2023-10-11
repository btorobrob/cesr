prsumglm <-
function(x, mtype){

  year0 <- as.numeric(mtype$refyear)
  dev <- deviance(x$model)
  rdf <- df.residual(x$model)
  if( mtype$type == "random" )
    disp <- dev / rdf ## not sure this is wise?
  else
    disp <- suppressWarnings(summary(x$model)$dispersion)  # suppress warning about zero weights in binomial glm
  
  if( is.na(disp) )
    cat('Model Fit: not estimated, did you fit a single site?\n')
  else
    cat(sprintf("Model Fit: Dispersion parameter is %4.2f (resid. deviance %5.1f on %4.0f d.f.)\n", disp, dev, rdf))

  fyear <- as.numeric(x$parms$years[1])
  if ( year0 == fyear ) {     # check if ref year is first year, so need to reverse comparison
    fyear <- max(x$parms$years)
    delta_ind <- x$parms$index[x$parms$years==fyear]
    if( length(delta_ind) == 1){
      if (delta_ind < 1 ) {
        delta_ind <- -100 * (1-delta_ind )
      } else {
        delta_ind <- 100 * (delta_ind-1)
      }
    } else{
      delta_ind <- NA
    }
  } else {
    # get the actual index value because for gams its not guaranteed to be 1
    delta_ind <- x$parms$index[which(x$parms$years==year0)]/x$parms$index[x$parms$years==fyear]
    if( length(delta_ind) == 1 ){
      if (delta_ind < 1 ) {
        delta_ind <- -100 * (1-delta_ind)
      } else {
        delta_ind <- 100 * (delta_ind-1)                                                             
      }
    } else {
      delta_ind <- NA
    }
  }
  
  if ( mtype$type == 'annual' ){
    if( !is.na(delta_ind) & delta_ind > 1000 )
      cat(sprintf("Change between %d and %d: >1000%%\n", fyear, year0))
    else
      cat(sprintf("Change between %d and %d: %4.1f%%\n", fyear, year0, delta_ind))
    refyear <- mtype$refyear
    last.rank <- x$parms$rank[which(x$parms$years==refyear)]
    if( last.rank == 1 )
      cat(sprintf("%d was the highest ranked year\n", refyear))
    else if( last.rank == 2 )
      cat(sprintf("%d was the second highest ranked year\n", refyear))
    else if( last.rank == 3 )
      cat(sprintf("%d was the third highest ranked year\n", refyear))
    else if( last.rank == max(x$parms$rank) )
      cat(sprintf("%d was the lowest ranked year\n", refyear))
    else if( last.rank == (max(x$parms$rank)-1) )
      cat(sprintf("%d was the second lowest ranked year\n", refyear))
    else if( last.rank == (max(x$parms$rank)-2) )
      cat(sprintf("%d was the third lowest ranked year\n", refyear))
  }
  else if ( mtype$type == 'trend' )
    cat(sprintf("Slope for past %d years is %5.3f \u00B1 %5.3f (t=%4.3f, P=%4.3f) \n", 
                x$test$nyrs, x$test$slope, x$test$slope.se, x$test$tval, x$test$tsig))
  else if ( mtype$type == 'constant' ) 
    cat(sprintf("Last year is %3.1f%% that in previous % d years: Estimate=%4.2f \u00B1 %4.2f (t=%4.3f, P=%4.3f) \n",
                100*exp(x$test$slope), x$test$nyrs, x$test$slope, x$test$slope.se, x$test$tval, x$test$tsig))
  else if ( mtype$type == 'smooth' ){
    r2 <- x$test$r2 * 100
    edf <- x$test$edf
    if( !is.na(delta_ind) & delta_ind > 1000 )
      cat(sprintf("Change %d to %d: >1000%%; smooth explains %4.1f%% of variance with %4.2f df\n", fyear, year0, r2, edf))
    else
      cat(sprintf("Change %d to %d: %4.1f%%; smooth explains %4.1f%% of variance with %4.2f df\n", fyear, year0, delta_ind, r2, edf))
  }
  else if ( mtype$type == 'random' ){
    vc <- x$test
    siteR2 <- floor(vc$var.s / (vc$var.s + vc$var.y + vc$var.sy) * 100)
    yearR2 <- floor(vc$var.y / (vc$var.s + vc$var.y + vc$var.sy) * 100)
    if( delta_ind > 1000 )
      cat(sprintf("Change %d to %d: >1000%%; variance due to site: %d%% (%5.3f \u00B1 %5.3f), to year: %d%% (%5.3f \u00B1 %5.3f)\n", 
                  fyear, year0, delta_ind, siteR2, vc$var.s, vc$se.s, yearR2, vc$var.y, vc$se.y))
    else
      cat(sprintf("Change %d to %d: %4.1f%%; variance due to site: %d%% (%5.3f \u00B1 %5.3f), to year: %d%% (%5.3f \u00B1 %5.3f)\n", 
                  fyear, year0, delta_ind, siteR2, vc$var.s, vc$se.s, yearR2, vc$var.y, vc$se.y))
  }
  
  cat('\n')
  invisible(x$parms)
}

