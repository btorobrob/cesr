plot.ces.data <- 
function(x, N=20){

  x <- data.table::data.table(x)

  if( is.null(N) | is.na(N) ) # just in case?
    N <- 20

  op <- par(mfrow=c(2,2), mar=c(2,3,2,1), oma=c(1,1,2,1))

  # a plot of captures in each year
  years <- x[ , .N, by=year]
  years <- years[order(years$year), ] # not sure why this should be necessary, but it seems to be
  plot(x=years$year, y=years$N, type='b', bty='n', las=1, ylab='', xlab='', main='Captures by year') 
  
  # annual mean captures (and quartile range) by visit
  visits <- yrsum(x, 'visit')
  plot(x=visits$visit, y=visits$mean, ylim=c(0, max(visits$hq, na.rm=TRUE)), bty='n', las=1,
       ylab='', xlab='', main='Mean captures by visit') 
  arrows(visits$visit, visits$lq, visits$visit, visits$hq, length=0)
  
  # annual mean captures (and quartile range) by species
  spp <- yrsum(x, 'species')
  spp <- spp[rev(order(spp$mean))]
  # check to see if there are only a few species present
  Nspp <- min(length(unique(x$species)), N)
  plot(x=rev(spp$mean[1:Nspp]), y=seq(Nspp:1), bty='n', yaxt='n', xlim=c(0, max(spp$hq, na.rm=TRUE)),
       ylab='', xlab='', main='Mean captures by species')
  arrows(rev(spp$lq[1:Nspp]), seq(Nspp:1), rev(spp$hq[1:Nspp]), seq(Nspp:1), length=0)
  axis(2, at=seq(Nspp:1), labels=rev(spp$species[1:Nspp]), las=1)
  
  # annual mean captures (and quartile range) by site
  site <- yrsum(x, 'sitename')
  site <- site[rev(order(site$mean))]
  # check to see if there are only a few species present
  Nsite <- min(length(unique(x$sitename)), N)
  plot(x=rev(site$mean[1:Nsite]), y=seq(Nsite:1), bty='n', yaxt='n', xlim=c(0, max(site$hq, na.rm=TRUE)),
       ylab='', xlab='', main='Mean captures by site')
  arrows(rev(site$lq[1:Nsite]), seq(Nsite:1), rev(site$hq[1:Nsite]), seq(Nsite:1), length=0)
  axis(2, at=seq(Nsite:1), labels=rev(site$sitename[1:Nsite]), las=1)
  
  par(op)
  
}
