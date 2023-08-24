annt.model.counts <-
function(x, year=-1, trend=100, offset=TRUE, cl=0.95){

  x$offset <- 0
  if ( offset ) 
    x$offset <- ifelse (x$corrcaps > x$totcaps, log(x$totcaps/x$corrcaps), 0)

  nyrs <- max(x$year) - min(x$year) + 1

  year <- ifelse(year==-1, min(x$year, na.rm=TRUE), year)

  if ( nyrs > trend ) {
    ybreak <- max(x$year) - trend
    yearf <- ifelse ( x$year > ybreak, 0, x$year )
    yeart <- ifelse ( x$year > ybreak, x$year-ybreak, 0 )
    if( length(table(x$site)) > 1 )
      x.lm <- glm(totcaps ~ as.factor(site) + as.factor(yearf) + yeart, family="quasipoisson", offset=offset, data=x)
    else
      x.lm <- glm(totcaps ~ as.factor(yearf) + yeart, family="quasipoisson", offset=offset, data=x)
    
    yearf1 <- c(min(yearf[yearf>0]):max(yearf), rep(0, trend))
    yeart1 <- c(rep(0, length(yearf1)-trend), c(1:trend))
    newdata <- as.data.frame(cbind(yearf=yearf1, site=rep(min(as.numeric(x.lm$xlevels[[1]])), nyrs)))
    newdata$yeart <- c(rep(0, length(yearf1)-trend), c(1:trend))  # do separately to avoid factor conversion
  }
  else {
    yeart <- x$year # for compatability with above
    
    if( length(table(x$site)) > 1 )
      x.lm <- glm(totcaps ~ as.factor(site) + yeart, family=quasipoisson, data=x)
    else
      x.lm <- glm(totcaps ~ yeart, family=quasipoisson, data=x)
    
    newdata <- as.data.frame(cbind(site=rep(min(as.numeric(x.lm$xlevels[[1]])), nyrs)))
    newdata$yeart <- c(min(yeart):max(yeart))
  }
  
  ann.vals <- ann.model.counts(x, year)$parms$parm
  
  x.pred <- predict(x.lm, newdata, se.fit=TRUE)
  x.pred$fit <- x.pred$fit + (mean(ann.vals) - mean(x.pred$fit))  
  
  years <- c(min(x$year):max(x$year))
  res <- cbind(years, data.frame(cbind(parm=x.pred$fit,se=x.pred$se))) # necessary to stop factor conversion!
  res$index <- exp(res$parm)
  res$annual <- exp(ann.vals)
  
  cl.int <- qnorm(1-((1-cl)/2))
  res$lcl <- exp(res$parm - cl.int * res$se)
  res$ucl <- exp(res$parm + cl.int * res$se)
  
  parno <- grep('^yeart',names(coef(x.lm)))
  slope <- coef(x.lm)[parno]
  slope.se <- sqrt(diag(vcov(x.lm)))[parno]
  tval <- slope/slope.se
  tsig <- 2*pt(abs(tval), x.lm$rank, lower.tail=FALSE)
  
  list(model=x.lm, parms=res,
        test=list(type='trend',nyrs=trend,slope=slope,slope.se=slope.se,tval=tval,tsig=tsig))
}

