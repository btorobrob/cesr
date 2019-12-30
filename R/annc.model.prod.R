annc.model.prod <-
function(x, compare=1, offset=TRUE, cl=0.95){

  data <- x  # we're going to need this later, annoyingly
  ad.new <- x$ad.data[ , c('site', 'year', 'totcaps', 'corrcaps') ]
  names(ad.new) <- c('site', 'year', 'adcaps', 'adexcaps')
  jv.new <- x$jv.data[ , c('site', 'year', 'totcaps', 'corrcaps') ]
  names(jv.new) <- c('site', 'year', 'jvcaps', 'jvexcaps')
  x <- merge(ad.new, jv.new)
  x$totcaps <- x$adcaps + x$jvcaps
  x$totexcaps <- x$adexcaps + x$jvexcaps
  x <- x[x$totcaps>0, ]     # no birds caught so doesn't contribute to model fit

  if ( class(x$year) == "factor")
    x$year <- as.numeric(levels(x$year))  
  nyrs <- max(x$year) - min(x$year) + 1
  if ( compare >= nyrs ) 
    stop("Not enough years to compare with\n")

  ybreak <- max(x$year) - compare - 1
  yearf <- ifelse ( x$year > ybreak, 1, x$year )
  yearf[x$year==max(x$year)] <- max(x$year)
  x$yearf <- relevel(factor(yearf), 1)
 
  if (offset) {
    x <- calc.offset(x)
  } else {
    x$offset <- 0
  }

  if( length(table(x$site)) > 1 )
    x.lm <- glm(as.matrix(cbind(jvcaps,totcaps)) ~ as.factor(site) + as.factor(yearf) - 1, family=quasibinomial, offset=offset, data=x)
  else
    x.lm <- glm(as.matrix(cbind(jvcaps,totcaps)) ~ as.factor(yearf) - 1, family=quasibinomial, offset=offset, data=x)
  
  if( (compare+1) < nyrs )
    yearf1 <- c(min(x$year):ybreak, rep(1,compare), max(x$year))
  else
    yearf1 <- c(rep(1,(nyrs-1)), max(x$year))
  newdata <- as.data.frame(cbind(yearf=yearf1, site=rep(min(x$site), nyrs)))

  x.pred <- predict(x.lm, newdata, se.fit=TRUE)
  x.pred$fit <- x.pred$fit - x.pred$fit[length(x.pred$fit)-1]

  years <- c(min(x$year):max(x$year))
  res <- cbind(years, data.frame(cbind(parm=x.pred$fit,se=x.pred$se))) # necessary to stop factor conversion!
  res$index <- exp(res$parm)           # NOTE: log back-transform rather than logistic!! gives no jv per ad
                                     #       rather simply ppn jvs
  res$annual <- ann.model.prod(data)$parms$index
  res$annual <- res$annual / mean(res$annual[yearf1==1]) # match the estimates
  
  cl.int <- qnorm(1-((1-cl)/2))
  res$lcl <- exp(res$parm - cl.int * res$se) 
  res$ucl <- exp(res$parm + cl.int * res$se)
  
  parno <- length(coef(x.lm))
  slope <- coef(x.lm)[parno]
  slope.se <- sqrt(diag(vcov(x.lm)))[parno]
  tval <- slope/slope.se
  tsig <- 2*pt(abs(tval), x.lm$rank, lower.tail=FALSE)
  
  list(model=x.lm, parms=res, 
        test=list(type='constant',nyrs=compare,slope=slope,slope.se=slope.se,tval=tval,tsig=tsig))
}

