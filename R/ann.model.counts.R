ann.model.counts <-
function(x, year=-1, offset=TRUE, cl=0.95){

  if( year==-1 )
    year <- max(x$year)
  x$year <- relevel(factor(x$year), which(levels(factor(x$year))==year))
 
  if ( offset ) {
    x$offset <- ifelse(x$corrcaps > x$totcaps, log(x$totcaps/x$corrcaps), 0 )
  } else {
    x$offset <- 0
  }
 
  if( length(table(x$site)) > 1 ) 
    x.lm <- glm(totcaps ~ as.factor(site) + year, family=quasipoisson, offset=offset, data=x)
  else
    x.lm <- glm(totcaps ~ year, family=quasipoisson, offset=offset, data=x)
  

  years <- names(coef(x.lm))
  parm <- coef(x.lm)
  se <- sqrt(diag(vcov(x.lm)))
  res <- cbind(years, data.frame(cbind(parm, se))) # necessary to stop factor conversion!
  res <- res[min(grep('^year',res$years)):length(res$years), ]
  row.names(res) <- NULL
  res$years <- as.numeric(gsub('year', '', res$years))
  res <- rbind(res, c(year, 0, 0))
  res <- res[do.call(order, list(res$years)), ]
  res$index <- exp(res$parm)
  
  cl.int <- qnorm(1-((1-cl)/2))
  res$lcl <- exp(res$parm - cl.int * res$se)
  res$ucl <- exp(res$parm + cl.int * res$se)
  list(model=x.lm, parms=res, test='none')
}

