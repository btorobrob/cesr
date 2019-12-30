summary.ces.counts <-
function(x) {

  if ( !class(x)[1]=='ces' | !class(x)[2]=='counts' )
    stop("No ces capture information\n")     

  if( !is.null(x$ad.data) ){
    ad.num <- tapply(x$ad.data$totcaps, x$ad.data$year, sum)
    ad.numc <- tapply(x$ad.data$corrcaps, x$ad.data$year, sum)
  } else
    ad.num <- ad.numc <- NA
  
  if( !is.null(x$jv.data) ){
    jv.num <- tapply(x$jv.data$totcaps, x$jv.data$year, sum)
    jv.numc <- tapply(x$jv.data$corrcaps, x$jv.data$year, sum)
  } else
    jv.num <- jv.numc <- NA
  

  cat("Total number of", x$spp.name, "caught:", sum(x$ad.data$totcaps), "adults and", sum(x$jv.data$totcaps), "juveniles\n\n")
  cat("Number of adults caught each year:\n")
  print(ad.num)
  cat("\nNumber of juveniles caught each year:\n")
  print(jv.num)

  res <- list(adcaps=ad.num, adcaps.corr=ad.numc,
             jvcaps=jv.num, jvcaps.corr=jv.numc, 
             spp=x$spp, spp.name=x$spp.name)
               
  class(res)<-c('ces','counts.summary')
  invisible(res)

}

