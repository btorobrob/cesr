annr.model.counts <-
function(x, year=-1, offset=TRUE, cl=0.95){

  if( year==-1 )
    year <- max(x$year)
  if( length(table(x$site)) < 6 ){
    wmessage <- "Fewer than 6 sites found, the model may struggle to converge"
    warning(wmessage, call.=FALSE, immediate.=TRUE)
  }

  x$offset <- 0
  if ( offset ) 
    x$offset <- ifelse(x$corrcaps > x$totcaps, log(x$totcaps/x$corrcaps), 0 )

  x.lm <- lme4::glmer(totcaps ~ (1|year)+(1|site)+(1|site:year), family="poisson", offset=offset, data=x)
  
  years <- as.numeric(row.names(lme4::ranef(x.lm)[[3]]))
  parm <- lme4::ranef(x.lm)[[3]][ , 1]
  se <- sqrt(c(attr(lme4::ranef(x.lm)[[3]], 'postVar')))
  res <- cbind(years, data.frame(cbind(parm, se))) # necessary to stop factor conversion!
  row.names(res) <- NULL
  if( res$parm[nrow(res)] > 0 )
    res$parm <- res$parm - res$parm[nrow(res)]
  else 
    res$parm <- res$parm + res$parm[nrow(res)]
  
  res$index <- exp(res$parm)
  
  cl.int <- qnorm(1-((1-cl)/2))
  res$lcl <- exp(res$parm - cl.int * res$se)
  res$ucl <- exp(res$parm + cl.int * res$se)
  
  vc <- lme4::VarCorr(x.lm)
  var.comp <- list(var.s=vc[[2]][[1]], se.s=as.numeric(attr(vc[[2]], "stddev")),
                   var.y=vc[[3]][[1]], se.y=as.numeric(attr(vc[[3]], "stddev")),
                   var.sy=vc[[1]][[1]], se.sy=as.numeric(attr(vc[[1]], "stddev")))

  return(list(model=x.lm, parms=res, test=var.comp))
}

