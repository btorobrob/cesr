plot.transients <-
function(cesobj, file=stdout(), col=c('blue', 'red'), border='gray10', leg.size=0.8, ...){
  
  chdata <- cesobj$chdata
  
  chmat <- matrix(unlist(strsplit(chdata$ch, split='')), 
                  nrow=length(chdata$ch), ncol=nchar(chdata$ch[1]), byrow=TRUE)
  chmat[chmat == "."] <- NA # doing this first avoids warnings about NAs by coercion
  chmat <- t(apply(chmat, 1, as.numeric)) # apply rotates the matrix for some reason
  first <- apply(chmat, 1, function(x) min(which(x==1))) # first capture
  ring.yr <- seq(cesobj$begin.time, (cesobj$begin.time+cesobj$years))[first]

  # Work out the status of each individual: 1 = resident caught; 2 = resident not caught; 3 = 'transient'
  get.status <- function(x){
    f <- min(which(x==1), na.rm=TRUE) 
    ifelse(x[f+1]==1, 1, ifelse(sum(x[(f+2):length(x)], na.rm=TRUE)>0, 2, 3))
  }
  
  status <- apply(chmat, 1, get.status) 
  yr.dat <- xtabs(~status+ring.yr)
  sitename <- chdata$sitename
  site.dat <- xtabs(~status+sitename)
  
  cols <- c(paste("#", paste(as.hexmode(col2rgb(col[1])),collapse=''),"88", sep=""), # resident - captured
           paste("#", paste(as.hexmode(col2rgb(col[1])),collapse=''),"44", sep=""), # resident - not captured
           paste("#", paste(as.hexmode(col2rgb(col[2])),collapse=''),"44", sep="")) # resident - not captured
  
  op <- par(mfrow=c(1,2), fig=c(0,0.5,0.1,1), mar=c(2.1, 3.1, 3.1, 0.5))
  barplot(yr.dat, col=cols, border="#111111", main="Captures by Year", las=1)
  par(fig=c(0.5,1,0.1,1), new=TRUE)
  barplot(site.dat, col=cols, border="#111111", main="Captures by Site", las=1)
  par(mar=c(0.1,0,0,0.1), fig=c(0,1,0,0.1), new=TRUE)
  legend('center', legend=c('captured same year', 'captured later', 'single capture'), horiz=TRUE,
         col=cols, border=border, bty='n', pch=15, cex=leg.size, pt.cex=leg.size*2, ...)
  par(op)

  invisible(list(years = yr.dat,
              sites = site.dat,
              spp.name = cesobj$spp.name))
}