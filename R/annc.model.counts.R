annc.model.counts <-
function(x, compare=1, offset=TRUE, cl=0.95){

  nyrs <- max(x$year) - min(x$year) + 1

  x$offset <- 0
  if (offset) 
    x$offset <- ifelse(x$corrcaps > x$totcaps, log(x$totcaps/x$corrcaps), 0)

  ybreak <- max(x$year) - compare - 1
  yearf <- ifelse ( x$year > ybreak, 1, x$year )
  yearf[x$year==max(x$year)] <- max(x$year)
  yearf <- relevel(factor(yearf), 1)
  
  if( length(table(x$site)) > 1 ){
    x.lm <- glm(totcaps ~ as.factor(site) + as.factor(yearf) - 1, family="quasipoisson", offset=offset, data=x)
    term.col <- 2
  } else {
    x.lm <- glm(totcaps ~ as.factor(yearf), family="quasipoisson", offset=offset, data=x)
    term.col <- 1
  }
  
  if( (compare+1) < nyrs )
    yearf1 <- c(min(x$year):ybreak, rep(1,compare), max(x$year))
  else
    yearf1 <- c(rep(1,(nyrs-1)), max(x$year))
  newdata <- as.data.frame(cbind(yearf=yearf1, site=rep(min(as.numeric(x.lm$xlevels[[1]])), nyrs)))
  
  x.pred <- predict(x.lm, newdata, se.fit=TRUE, type="terms")

  years <- c(min(x$year):max(x$year))
  res <- cbind(years, data.frame(cbind(parm=x.pred$fit[ ,term.col], se=x.pred$se[ ,term.col]))) # necessary to stop factor conversion!
  res$index <- exp(res$parm)
  res$annual <- ann.model.counts(x)$parms$index
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

