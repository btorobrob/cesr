prsumglm <-
function(x, mtype){

  year0 <- mtype$refyear
  disp <- suppressWarnings(summary(x$model)$dispersion)  # suppress warning about zero weights in binomial glm
  dev <- x$model$deviance
  rdf <- x$model$df.residual
  if( is.na(disp) )
    cat('Model Fit: not estimated, did you fit a single site?\n')
  else
    cat(sprintf("Model Fit: Dispersion parameter is %4.2f (resid. deviance %5.1f on %5.0f d.f.)\n", disp, dev, rdf))
  fyear <- x$parms$years[1]
  if ( year0 == fyear ) {     # check if ref year is first year, so need to reverse comparison
    fyear <- max(x$parms$years)
    delta_ind <- x$parms$index[x$parms$years==fyear]
    if (delta_ind < 1 ) {
      delta_ind <- -100 * (1-delta_ind )
    } else {
      delta_ind <- 100 * (delta_ind-1)
    }
  } else {
    delta_ind <- 1/x$parms$index[x$parms$years==fyear]
    if (delta_ind < 1 ) {
      delta_ind <- -100 * (1-delta_ind)
    } else {
      delta_ind <- 100 * (delta_ind-1)                                                             
    }
  }
  if ( mtype$type == 'annual' )
    cat(sprintf("Change between %d and %d: %4.1f%%\n", fyear, year0, delta_ind))
  else if ( mtype$type == 'trend' )
    cat(sprintf("Slope for past %d years is %4.2f +/- %4.2f (t=%4.3f, P=%4.3f) \n", 
                 x$test$nyrs, x$test$slope, x$test$slope.se, x$test$tval, x$test$tsig))
  else if ( mtype$type == 'constant' ) 
    cat(sprintf("Last year is %3.1f%% that in previous % d years: Estimate=%4.2f +/- %4.2f (t=%4.3f, P=%4.3f) \n",
                100*exp(x$test$slope), x$test$nyrs, x$test$slope, x$test$slope.se, x$test$tval, x$test$tsig))
  cat('\n')
  return(x$parms)
}

