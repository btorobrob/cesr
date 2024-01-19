ann.model.prod <-
function(x, year=-1, offset=TRUE, cl=0.95){

  ad <- x$ad.data[ , c('site','year','totcaps','corrcaps') ]
  names(ad) <- c('site','year','adcaps','adexcaps')
  jv <- x$jv.data[ , c('site','year','totcaps','corrcaps') ]
  names(jv) <- c('site','year','jvcaps','jvexcaps')
  x <- merge(ad, jv, by=c('site','year'), all.x=TRUE) # exclude sites with no ad coverage
  x[is.na(x)] <- 0 # for sites where no ads or jvs caught in a year
  x$totcaps <- x$adcaps + x$jvcaps
  x$totexcaps <- x$adexcaps + x$jvexcaps
  x <- x[x$totcaps>0, ]     # no birds caught so doesn't contribute to model fit
    
  if( year==-1 )
    year <- max(x$year)
  x$year <- relevel(factor(x$year), which(levels(factor(x$year))==year))
 
  if (offset) {
    x <- calc.offset(x)
  } else {
    x$offset <- 0
  }

  if( length(table(x$site)) > 1 )
    x.lm <- glm(as.matrix(cbind(x$jvcaps,x$adcaps)) ~ as.factor(site)+year, family="quasibinomial", offset=offset, data=x)
  else
    x.lm <- glm(as.matrix(cbind(x$jvcaps,x$adcaps)) ~ year, family="quasibinomial", offset=offset, data=x)
  
  years <- names(coef(x.lm))
  parm <- coef(x.lm)
  se <- sqrt(diag(vcov(x.lm)))
  res <- cbind(years, data.frame(cbind(parm,se))) # necessary to stop factor conversion!
  res <- res[min(grep('^year',res$years)):length(res$years),]
  row.names(res) <- NULL
  res$years <- as.numeric(gsub('year', '', res$years))
  res <- rbind(res, c(year, 0, 0))
  res <- res[do.call(order, list(res$years)), ]
  res$index <- exp(res$parm)           # NOTE: log back-transform rather than logistic!! gives no jv per ad
                                       #       rather simply ppn jvs
  
  cl.int <- qnorm(1-((1-cl)/2))
  res$lcl <- exp(res$parm - cl.int * res$se) 
  res$ucl <- exp(res$parm + cl.int * res$se)
  res$rank <- rank(-res$index)
  
  list(model=x.lm, parms=res, test='none')
}

