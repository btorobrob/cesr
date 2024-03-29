plot.ces.counts <-
function(x, sites=FALSE){

  if ( !class(x)[1] == 'ces' | !class(x)[2] == 'counts' )
    stop("No ces capture information\n")     

  luniq <- function(x){length(unique(x))}
                                                                                          
  if ( sites ) {
    ad.num <- tapply(x$ad.data$site[x$ad.data$corrcaps>0], x$ad.data$year[x$ad.data$corrcaps>0], luniq)
    jv.num <- tapply(x$jv.data$site[x$jv.data$corrcaps>0], x$jv.data$year[x$jv.data$corrcaps>0], luniq)
    ylim1 <- c(0, max(pretty(max(ad.num, jv.num))))
  }
  else {
    ad.num <- tapply(x$ad.data$totcaps, x$ad.data$year, sum, na.rm=TRUE)
    jv.num <- tapply(x$jv.data$totcaps, x$jv.data$year, sum, na.rm=TRUE)
    ad.numc <- tapply(x$ad.data$corrcaps, x$ad.data$year, sum, na.rm=TRUE)
    jv.numc <- tapply(x$jv.data$corrcaps, x$jv.data$year, sum, na.rm=TRUE)
    ylim1 <- c(0, max(pretty(max(ad.numc, jv.numc))))
  }

  ad.site <- tapply(x$ad.data$corrcaps, x$ad.data$site, sum, na.rm=TRUE)
  jv.site <- tapply(x$jv.data$corrcaps, x$jv.data$site, sum, na.rm=TRUE)
  ad.srt <- cumsum(sort(ad.site, decreasing=TRUE))
  jv.srt <- cumsum(sort(jv.site, decreasing=TRUE))
  ylim2 <- c(0, max(pretty(max(ad.srt, jv.srt))))

  op <- par(mfrow=c(2,2), mar=c(2,4,1,1), oma=c(1,1,2,1))
  if ( sites ) {
    barplot(ad.num, ylab="Sites recording Adults", xlab="", ylim=ylim1, las=1)
    barplot(jv.num, ylab="Sites recording Juvs", xlab="", ylim=ylim1, las=1)
  } else {
    barplot(ad.numc, ylab="No. Adults", xlab="", ylim=ylim1, col='black', las=1)
    barplot(ad.num, yaxt='n', yaxt='n', add=TRUE)
    barplot(jv.numc, ylab="No. Juvs", xlab="", ylim=ylim1, col='black', las=1)
    barplot(jv.num, yaxt='n', xaxt='n', add=TRUE)
  }
  pp <- par(mar=c(2,4,2,1))
  plot(ad.srt, ylab="Cumulative Adults", xlab="", ylim=ylim2, type='l', las=1)
  title(main=sprintf("Total: %d Adults on %d sites",round(max(ad.srt)),length(ad.srt)),font.main=1,cex.main=0.9)
  plot(jv.srt, ylab="Cumulative Juvs", xlab="", ylim=ylim2, type='l', las=1)
  title(main=sprintf("Total: %d Juvs on %d sites",round(max(jv.srt)),length(jv.srt)),font.main=1,cex.main=0.9)
  par(pp)
  title(main=sprintf("Number of birds caught: %s",x$spp.name), outer=TRUE)

  par(op)    
  
  if ( sites ) {
    res <- list(adsites=ad.num, jvsites=jv.num, 
               ad.plots=ad.site, jv.plots=jv.site,
               spp=x$spp, spp.name=x$spp.name)
               
  } else {
    res <- list(adcaps=ad.num, adcaps.corr=ad.numc,
               jvcaps=jv.num, jvcaps.corr=jv.numc, 
               ad.plots=ad.site, jv.plots=jv.site,
               spp=x$spp, spp.name=x$spp.name)
  }               

  class(res)<-c('ces','counts.summary')
  invisible(res)
}

