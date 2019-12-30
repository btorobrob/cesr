summary.ces.glmfit <-
function(x, ads=FALSE, jvs=FALSE, prod=FALSE){

  if ( !class(x)[1] == 'ces' | !class(x)[2] == 'glmfit' )
    stop("No information to summarise\n")
  if ( ads == FALSE & jvs==FALSE & prod == FALSE ){
    ads <- jvs <- prod <- TRUE
  }

  res <- list()
  if ( ads==TRUE & length(x$ad.results) > 0 ){ 
    cat(sprintf('Adult Abundance (%s)\n',x$spp.name))
    res$adults <- prsumglm(x$ad.results, x$model.type)
  }
  if ( jvs==TRUE & length(x$jv.results) > 0 ){    
    cat(sprintf('Juvenile Abundance (%s)\n',x$spp.name))
    res$juveniles <- prsumglm(x$jv.results, x$model.type)
  }
  if ( prod==TRUE & length(x$ad.results) & length(x$jv.results) > 0 ){ 
    cat(sprintf('Productivity (%s)\n',x$spp.name))
    res$productvity <- prsumglm(x$pr.results, x$model.type)
  }
  
  invisible(res)
  
}

