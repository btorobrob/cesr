writeces.glmfit <-
function(x, file="", ads=FALSE, jvs=FALSE, prod=FALSE){

  if ( !class(x)[1] == 'ces' | !class(x)[2] == 'glmfit' )
    stop("No information to summarise\n")
  if ( ads == FALSE & jvs==FALSE & prod == FALSE ){
    ads <- jvs <- prod <- TRUE
  }

  if( file == "" ) 
    file <- file.choose()

  if ( ads == TRUE & length(x$ad.results) > 0 ){    
    cat(sprintf('Adult Abundance (%s)\n', x$spp.name), file=file, append=TRUE)
    disp <- summary(x$ad.results$model)$dispersion
    dev <- x$ad.results$model$deviance
    df <- x$ad.results$model$df.residual;
    cat(sprintf("Model Fit: Dispersion parameter is %4.2f (resid. deviance %5.1f on %5.0f d.f.)\n\n", disp, dev, df), file=file, append=TRUE)
    suppressWarnings(capture.output(print(x$ad.results$parms), file=file, append=TRUE))
    cat('\n', file=file, append=TRUE)
  }
  if ( jvs==TRUE & length(x$jv.results) > 0 ){    
    cat(sprintf('Juvenile Abundance (%s)\n',x$spp.name), file=file, append=T)
    disp<-summary(x$jv.results$model)$dispersion
    dev<-x$jv.results$model$deviance 
    df<-x$jv.results$model$df.residual
    cat(sprintf("Model Fit: Dispersion parameter is %4.2f (resid. deviance %5.1f on %5.0f d.f.)\n\n", disp, dev, df), file=file, append=T)
    suppressWarnings(capture.output(print(x$jv.results$parms), file=file, append=TRUE))
    cat('\n', file=file, append=T)
  }
  if ( prod==T & length(x$pr.results) > 0 ){    
    cat(sprintf('Productivity (ppn juvs) (%s)\n',x$spp.name), file=file, append=TRUE)
    disp<-summary(x$pr.results$model)$dispersion
    dev<-x$pr.results$model$deviance 
    df<-x$pr.results$model$df.residual
    cat(sprintf("Model Fit: Dispersion parameter is %4.2f (resid. deviance %5.1f on %5.0f d.f.)\n\n", disp, dev, df), file=file, append=T)
    suppressWarnings(capture.output(print(x$pr.results$parms), file=file, append=TRUE))
    cat('\n', file=file, append=TRUE)
  }
}

