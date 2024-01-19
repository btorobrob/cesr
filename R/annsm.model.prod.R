annsm.model.prod <-
function(x, offset=TRUE, cl=0.95){

  cesdata <- x  # we're going to need this later, annoyingly
  year <- ifelse(year==-1, min(x[[1]]$year, na.rm=TRUE), year)
  
  ad <- x$ad.data[ , c('site','year','totcaps','corrcaps') ]
  names(ad)<-c('site','year','adcaps','adexcaps')
  jv <- x$jv.data[ , c('site','year','totcaps','corrcaps') ]
  names(jv)<-c('site','year','jvcaps','jvexcaps')
  # merge - sites with no ads caught provide no info on productivity
  x <- merge(ad, jv, by=c('site', 'year'), all.x=TRUE)
  x[is.na(x)] <- 0 # for sites where no ads or jvs caught in a year
  x$totcaps <- x$adcaps + x$jvcaps
  x <- x[x$totcaps>0, ]     # no birds caught so doesn't contribute to model fit

  if (offset) {
    x <- calc.offset(x)
  } else {
    x$offset <- 0
  }

  if( length(table(x$site)) > 1 )
    x.gam <- mgcv::gam(as.matrix(cbind(jvcaps,adcaps)) ~ as.factor(site) + s(year), family="quasibinomial", offset=offset, data=x)
  else
    x.gam <- mgcv::gam(as.matrix(cbind(jvcaps,adcaps)) ~ s(year) + yeart, family="quasibinomial", offset=offset, data=x)
    
  nyrs <- max(x$year) - min(x$year) + 1
  newdata <- as.data.frame(cbind(site=rep(min(as.numeric(x.gam$xlevels[[1]])), nyrs)))
  newdata$year <- c(min(x$year):max(x$year))

  ann.vals <- ann.model.prod(list(ad.data=cesdata$ad.data, jv.data=cesdata$jv.data), year)$parms$parm
    
  x.pred <- predict(x.gam, newdata, se.fit=TRUE)
  x.pred$fit <- x.pred$fit + (mean(ann.vals) - mean(x.pred$fit))  
  
  years <- c(min(x$year):max(x$year))
  res <- cbind(years, data.frame(cbind(parm=x.pred$fit,se=x.pred$se))) # necessary to stop factor conversion!
  res$index <- exp(res$parm)         # NOTE: log back-transform rather than logistic!! gives no jv per ad
  res$annual <- exp(ann.vals)

  cl.int <- qnorm(1-((1-cl)/2))
  res$lcl <- exp(res$parm - cl.int * res$se)
  res$ucl <- exp(res$parm + cl.int * res$se)
  
  params <- summary(x.gam)
  
  list(model=x.gam, parms=res,
       test=list(type='smooth',edf=params$edf,scale=params$scale,r2=params$dev.expl))
}

