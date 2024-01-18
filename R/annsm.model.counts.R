annsm.model.counts <-
function(x, offset=TRUE, cl=0.95){

  x$offset <- 0
  if ( offset ) 
    x$offset <- ifelse (x$corrcaps > x$totcaps, log(x$totcaps/x$corrcaps), 0)

  if( length(table(x$site)) > 1 )
    x.gam <- gam(totcaps ~ as.factor(site) + s(year), family="quasipoisson", offset=offset, data=x)
  else
    x.gam <- gam(totcaps ~ s(year), family="quasipoisson", offset=offset, data=x)
  
  nyrs <- max(x$year) - min(x$year) + 1
  newdata <- as.data.frame(cbind(site=rep(min(as.numeric(x.gam$xlevels[[1]])), nyrs)))
  newdata$year <- c(min(x$year):max(x$year))
  
  ann.vals <- ann.model.counts(x, year)$parms$parm
  
  x.pred <- predict(x.gam, newdata, se.fit=TRUE)
  x.pred$fit <- x.pred$fit + (mean(ann.vals) - mean(x.pred$fit))  
  
  years <- c(min(x$year):max(x$year))
  res <- cbind(years, data.frame(cbind(parm=x.pred$fit,se=x.pred$se))) # necessary to stop factor conversion!
  res$index <- exp(res$parm)
  res$annual <- exp(ann.vals)
  
  cl.int <- qnorm(1-((1-cl)/2))
  res$lcl <- exp(res$parm - cl.int * res$se)
  res$ucl <- exp(res$parm + cl.int * res$se)
  
  params <- summary(x.gam)
  
  list(model=x.gam, parms=res,
       test=list(type='smooth',edf=params$edf,scale=params$scale,r2=params$dev.expl))
  
}

