annt.model.prod <-
function(x, year=-1, trend=100, offset=TRUE, cl=0.95){

  cesdata <- x  # we're going to need this later, annoyingly
  year <- ifelse(year==-1, min(x[[1]]$year, na.rm=TRUE), year)
  
  ad <- x$ad.data[ , c('site','year','totcaps','corrcaps') ]
  names(ad)<-c('site','year','adcaps','adexcaps')
  jv <- x$jv.data[ , c('site','year','totcaps','corrcaps') ]
  names(jv)<-c('site','year','jvcaps','jvexcaps')
  x <- merge(ad, jv, by=c('site', 'year'), all.x=TRUE)
  x[is.na(x)] <- 0
  x$totcaps <- x$adcaps + x$jvcaps
  x$totexcaps <- x$adexcaps + x$jvexcaps
  x <- x[x$totcaps>0, ]     # no birds caught so doesn't contribute to model fit

  if (offset) {
    x <- calc.offset(x)
  } else {
    x$offset <- 0
  }

  nyrs<-max(x$year)-min(x$year)+1

  if ( nyrs > trend ) {
    ybreak <- max(x$year) - trend
    x$yearf <- ifelse ( x$year > ybreak, 0, x$year )
    x$yeart <- ifelse ( x$year > ybreak, x$year-ybreak, 0 )
    if( length(table(x$site)) > 1 )
      x.lm <- glm(as.matrix(cbind(jvcaps,adcaps)) ~ as.factor(site) + as.factor(yearf) + yeart, family="quasibinomial", offset=offset, data=x)
    else
      x.lm <- glm(as.matrix(cbind(jvcaps,adcaps)) ~ as.factor(yearf) + yeart, family="quasibinomial", offset=offset, data=x)
    
    yearf1 <- c(min(x$yearf[x$yearf>0]):max(x$yearf),rep(0,trend))
    yeart1 <- c(rep(0,length(yearf1)-trend),c(1:trend))
    newdata <- as.data.frame(cbind(yearf=yearf1,site=rep(min(as.numeric(x.lm$xlevels[[1]])),nyrs)))
    newdata$yeart <- c(rep(0,length(yearf1)-trend),c(1:trend))  # do separately to avoid factor conversion
  }
  else {
    x$yeart <- x$year # for compatibility with above
    if( length(table(x$site)) > 1 )
      x.lm <- glm(as.matrix(cbind(jvcaps,adcaps)) ~ as.factor(site) + yeart, family=quasibinomial, offset=offset, data=x)
    else
      x.lm <- glm(as.matrix(cbind(jvcaps,adcaps)) ~ yeart, family=quasibinomial, offset=offset, data=x)

    newdata <- as.data.frame(cbind(site=rep(min(as.numeric(x.lm$xlevels[[1]])),nyrs)))
    newdata$yeart <- c(min(x$yeart):max(x$yeart))
  }

  ann.vals <- ann.model.prod(list(ad.data=cesdata$ad.data, jv.data=cesdata$jv.data), year)$parms$parm
    
  x.pred <- predict(x.lm, newdata, se.fit=TRUE)
  x.pred$fit <- x.pred$fit + (mean(ann.vals) - mean(x.pred$fit))  

  years <- c(min(x$year):max(x$year))
  res <- cbind(years, data.frame(cbind(parm=x.pred$fit,se=x.pred$se))) # necessary to stop factor conversion!
  res$index <- exp(res$parm)         # NOTE: log back-transform rather than logistic!! gives no jv per ad
                                     #       rather simply ppn jvs
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

