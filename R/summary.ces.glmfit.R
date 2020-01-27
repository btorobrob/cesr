summary.ces.glmfit <-
function(cesobj, ads=FALSE, jvs=FALSE, prod=FALSE){

  if ( !class(cesobj)[1] == 'ces' | !class(cesobj)[2] == 'glmfit' )
    stop("No information to summarise\n")
  if ( ads == FALSE & jvs==FALSE & prod == FALSE ){
    ads <- jvs <- prod <- TRUE
  }
  
  res <- list()
  if ( ads==TRUE & length(cesobj$ad.results) > 0 ){ 
    cat(sprintf('Adult Abundance (%s)\n',cesobj$spp.name))
    res$adults <- prsumglm(cesobj$ad.results, cesobj$model.type)
  }
  if ( jvs==TRUE & length(cesobj$jv.results) > 0 ){    
    cat(sprintf('Juvenile Abundance (%s)\n',cesobj$spp.name))
    res$juveniles <- prsumglm(cesobj$jv.results, cesobj$model.type)
  }
  if ( prod==TRUE & length(cesobj$ad.results) & length(cesobj$jv.results) > 0 ){ 
    cat(sprintf('Productivity (%s)\n',cesobj$spp.name))
    res$productvity <- prsumglm(cesobj$pr.results, cesobj$model.type)
  }
  
  invisible(res)
  
}

