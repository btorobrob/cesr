annr.model.prod <-
function(x, year=-1, offset=TRUE, cl=0.95){

  ad <- x$ad.data[ , c('site','year','totcaps','corrcaps') ]
  names(ad) <- c('site','year','adcaps','adexcaps')
  jv <- x$jv.data[ , c('site','year','totcaps','corrcaps') ]
  names(jv) <- c('site','year','jvcaps','jvexcaps')
  x <- merge(ad, jv, by=c('site','year')) # exclude sites with no ad/jv coverage, so no all=
  x$totcaps <- x$adcaps + x$jvcaps
  x <- x[x$totcaps>0, ]     # no birds caught so doesn't contribute to model fit
    
  if( year==-1 )
    year <- max(x$year)
  if( length(table(x$site)) < 6 ){
    wmessage <- "Fewer than 6 sites found, the model may struggle to converge"
    warning(wmessage, call.=FALSE, immediate.=TRUE)
  }
  
  if (offset) {
    x <- calc.offset(x)
  } else {
    x$offset <- 0
  }

  x.lm <- lme4::glmer(as.matrix(cbind(x$jvcaps,x$adcaps)) ~ (1|year)+(1|site)+(1|site:year), family="binomial", offset=offset, data=x)
  
  years <- as.numeric(row.names(ranef(x.lm)[[3]]))
  parm <- ranef(x.lm)[[3]][ , 1]
  se <- sqrt(c(attr(ranef(x.lm)[[3]], 'postVar')))
  res <- cbind(years, data.frame(cbind(parm, se))) # necessary to stop factor conversion!
  row.names(res) <- NULL
  if( res$parm[nrow(res)] > 0 )
    res$parm <- res$parm - res$parm[nrow(res)]
  else 
    res$parm <- res$parm + res$parm[nrow(res)]
  
  res$index <- exp(res$parm)           # NOTE: log back-transform rather than logistic!! gives no jv per ad
                                       #       rather simply ppn jvs
  cl.int <- qnorm(1-((1-cl)/2))
  res$lcl <- exp(res$parm - cl.int * res$se) 
  res$ucl <- exp(res$parm + cl.int * res$se)

  vc <- VarCorr(x.lm)
  var.comp <- list(var.s=vc[[2]][[1]], se.s=as.numeric(attr(vc[[2]], "stddev")),
                   var.y=vc[[3]][[1]], se.y=as.numeric(attr(vc[[3]], "stddev")),
                   var.sy=vc[[1]][[1]], se.sy=as.numeric(attr(vc[[1]], "stddev")))

  return(list(model=x.lm, parms=res, test=var.comp))
}

