writeces.glmfit <-
function(cesobj, file="", ads=FALSE, jvs=FALSE, prod=FALSE){

  if ( !class(cesobj)[1] == 'ces' | !class(cesobj)[2] == 'glmfit' )
    stop("No information to summarise\n")
  if ( ads == FALSE & jvs==FALSE & prod == FALSE ){
    ads <- jvs <- prod <- TRUE
  }
  
  if( file == "" ) 
    file <- file.choose()
  
  if ( ads == TRUE & length(cesobj$ad.results) > 0 ){    
    cat(sprintf('Adult Abundance (%s)\n', cesobj$spp.name), file=file, append=TRUE)
    disp <- summary(cesobj$ad.results$model)$dispersion
    dev <- cesobj$ad.results$model$deviance
    df <- cesobj$ad.results$model$df.residual;
    cat(sprintf("Model Fit: Dispersion parameter is %4.2f (resid. deviance %5.1f on %5.0f d.f.)\n\n", disp, dev, df), file=file, append=TRUE)
    suppressWarnings(capture.output(print(cesobj$ad.results$parms), file=file, append=TRUE))
    cat('\n', file=file, append=TRUE)
  }
  if ( jvs==TRUE & length(cesobj$jv.results) > 0 ){    
    cat(sprintf('Juvenile Abundance (%s)\n',cesobj$spp.name), file=file, append=TRUE)
    disp<-summary(cesobj$jv.results$model)$dispersion
    dev<-cesobj$jv.results$model$deviance 
    df<-cesobj$jv.results$model$df.residual
    cat(sprintf("Model Fit: Dispersion parameter is %4.2f (resid. deviance %5.1f on %5.0f d.f.)\n\n", disp, dev, df), file=file, append=T)
    suppressWarnings(capture.output(print(cesobj$jv.results$parms), file=file, append=TRUE))
    cat('\n', file=file, append=TRUE)
  }
  if ( prod==TRUE & length(cesobj$pr.results) > 0 ){    
    cat(sprintf('Productivity (ppn juvs) (%s)\n',cesobj$spp.name), file=file, append=TRUE)
    disp<-summary(cesobj$pr.results$model)$dispersion
    dev<-cesobj$pr.results$model$deviance 
    df<-cesobj$pr.results$model$df.residual
    cat(sprintf("Model Fit: Dispersion parameter is %4.2f (resid. deviance %5.1f on %5.0f d.f.)\n\n", disp, dev, df), file=file, append=T)
    suppressWarnings(capture.output(print(cesobj$pr.results$parms), file=file, append=TRUE))
    cat('\n', file=file, append=TRUE)
  }
}

