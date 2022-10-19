summary.ces.counts <-
function(cesobj) {

  if ( !class(cesobj)[1]=='ces' | !class(cesobj)[2]=='counts' )
    stop("No ces capture information\n")     

  if( !is.null(cesobj$ad.data) ){
    ad.num <- tapply(cesobj$ad.data$totcaps, cesobj$ad.data$year, sum)
    ad.numc <- tapply(cesobj$ad.data$corrcaps, cesobj$ad.data$year, sum)
  } else
    ad.num <- ad.numc <- NA
  
  if( !is.null(cesobj$jv.data) ){
    jv.num <- tapply(cesobj$jv.data$totcaps, cesobj$jv.data$year, sum)
    jv.numc <- tapply(cesobj$jv.data$corrcaps, cesobj$jv.data$year, sum)
  } else
    jv.num <- jv.numc <- NA
  

  cat("Total number of", cesobj$spp.name, "caught:", sum(cesobj$ad.data$totcaps), "adults and", sum(cesobj$jv.data$totcaps), "juveniles\n\n")
  cat("Number of adults caught each year:\n")
  print(ad.num)
  cat("\nNumber of juveniles caught each year:\n")
  print(jv.num)

  res <- list(adcaps=ad.num, adcaps.corr=ad.numc,
             jvcaps=jv.num, jvcaps.corr=jv.numc, 
             spp=cesobj$spp, spp.name=cesobj$spp.name)
               
  class(res)<-c('ces', 'counts.summary')
  invisible(res)

}

