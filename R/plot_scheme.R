plot_scheme <-
function(cesobj, filename="scheme.pdf", main=NULL, height=11.29, width=8.69,
         pch.cex=1, lwd=1, col.site=NULL, legend.pos=0, map.border=1,
         n.species=20, col.age=NULL, sp.cex=0.7, min.visit=0, ...){

  filetype <- substr(filename, nchar(filename)-2, nchar(filename))
  if( filetype == "pdf" )
    pdf(file=filename, width=width, height=height, ...)
  else if( filetype == "png" )
    png(filename=filename, width=width, height=height, ...)
  else if( filetype == "tif" | filetype == "tif" )
    tiff(filename=filename, width=width, height=height, ...)
  else
    stop(paste("unrecognised file type:", filetype))
  
  on.exit(dev.off()) # for safety!
  
  sites <- extract.coverage(cesobj)
  years <- as.data.frame(table(sites$years$year))
  cesobj.dt <- data.table::data.table(cesobj) # needed at end for speed
  cesobj.dt <- cesobj.dt[order(ring,year,visit), ]
  cesobj.dt[ , ry:=paste0(year,"_",ring)]
  cesobj.dt[ , retrap := duplicated(ry)]

  par(mfrow=c(3,2), oma=c(1,1,3,1), mgp=c(2.5,1,0))

  ### plot of sites per year ----
  par(mar=c(3, 7, 3, 1))

  gdata <- unique(cesobj.dt,by=c('year','site'))[ , .N, by=.(year,habitat)]
  gdata <- reshape::cast(gdata, year~habitat, value='N', fun.aggregate=sum)
  gdata[is.na(gdata)] <- 0
  n.cols <- ncol(gdata) # to avoid adding in the totals columns below
  gdata$rbtot <- (if( length(which(names(gdata)=="RB")) > 0 ) gdata$RB else 0)
  gdata$wstot <- (if( length(which(names(gdata)=="WS")) > 0 ) gdata$rbtot + gdata$WS else gdata$rbtot)
  gdata$dstot <- (if( length(which(names(gdata)=="DS")) > 0 ) gdata$wstot + gdata$DS else gdata$wstot)
  gdata$wdtot <- (if( length(which(names(gdata)=="WD")) > 0 ) gdata$dstot + gdata$WD else gdata$dstot)
  gdata$gntot <- (if( length(which(names(gdata)=="GN")) > 0 ) gdata$wdtot + gdata$GN else gdata$wdtot)
  gdata$tot <- (if( n.cols > 2 ) rowSums(gdata[ , (2:n.cols)]) else gdata$tot <- gdata[ , 2])

  # do.call(eval(parse(text="viridis::viridis")),list(6)) # for later sometime
  if( is.null(col.site) )
    col.site <- c("#7D9D33", "#CED38C", "#DCC949", "#BCA888", "#CD8862", "#775B24")
    # devtools::install_github("G-Thomson/Manu")::Kakapo
  
  plot(-1, -1, type='h', lwd=lwd, las=1, bty="n", 
       xlim=range(gdata$year), ylim=c(0,max(gdata$tot)), 
       main="Number of sites", ylab="", xlab="")
  if( sum(gdata$rbtot) > 0 ) # mostly for France...
    arrows(gdata$year[gdata$rbtot!=0], 0,
           gdata$year[gdata$rbtot!=0], gdata$rbtot[gdata$rbtot!=0],
           length=0, lwd=5*lwd, lend="square", col=col.site[1])
  if( sum(gdata$wstot) > 0 ) 
    arrows(gdata$year[gdata$wstot!=gdata$rbtot], gdata$rbtot[gdata$wstot!=gdata$rbtot],
           gdata$year[gdata$wstot!=gdata$rbtot], gdata$wstot[gdata$wstot!=gdata$rbtot],
           length=0, lwd=5*lwd, lend="square", col=col.site[2])
  if( sum(gdata$dstot) > 0 ) 
    arrows(gdata$year[gdata$dstot!=gdata$wstot], gdata$wstot[gdata$dstot!=gdata$wstot],
           gdata$year[gdata$dstot!=gdata$wstot], gdata$dstot[gdata$dstot!=gdata$wstot],
           length=0, lwd=5*lwd, lend="square", col=col.site[3])
  if( sum(gdata$wdtot) > 0 ) 
    arrows(gdata$year[gdata$dstot!=gdata$wdtot], gdata$dstot[gdata$dstot!=gdata$wdtot],
           gdata$year[gdata$dstot!=gdata$wdtot], gdata$wdtot[gdata$dstot!=gdata$wdtot],
           length=0, lwd=5*lwd, lend="square", col=col.site[4])
  if( sum(gdata$gntot) > 0 ) 
    arrows(gdata$year[gdata$wdtot!=gdata$gntot], gdata$wdtot[gdata$wdtot!=gdata$gntot],
           gdata$year[gdata$wdtot!=gdata$gntot], gdata$gntot[gdata$wdtot!=gdata$gntot],
           length=0, lwd=5*lwd, lend="square", col=col.site[5])
  arrows(gdata$year[gdata$tot!=gdata$gntot], gdata$gntot[gdata$tot!=gdata$gntot],
         gdata$year[gdata$tot!=gdata$gntot], gdata$tot[gdata$tot!=gdata$gntot],
         length=0, lwd=5*lwd, lend="square", col=col.site[6])
  current <- as.data.frame(table(sites$sites$first.yr[sites$sites$current==1]))
  points(as.numeric(as.character(current$Var1)), current$Freq, pch=19, cex=pch.cex, col="black")
  
  if( legend.pos == 0 ) # have a guess
    legend.pos <- min(gdata$year) - ((max(gdata$year)-min(gdata$year))*0.9)
  else
    legend.pos <- min(gdata$year) - abs(legend.pos)
  legend(y=max(gdata$tot), x=legend.pos,
         legend=c("Reedbed","Wet Scrub","Dry Scrub","Woodland","Garden","Other","Current"),
         col=c(col.site,'black'), pch=c(rep(15,6),19), cex=sp.cex, bty='n', xpd=TRUE)
  
  ### the page title ----
  if( !is.null(main) ){
    title(paste("Summary of CES ringing in", main), outer=TRUE)
    subtitle <- paste("For", nrow(sites$sites), "sites in", 
                      paste(range(sites$years$year), collapse=" - "))
    mtext(subtitle, side=3, line=-0.25, cex=0.75, outer=TRUE)
  }

  ### map of sites ----
  par(mar=c(1, 1, 1, 1))
  if( length(map.border) == 1 )
    map.border <- rep(map.border, 2)
  pt.col <- rep(col.site[6], length(sites$sites$habitat))
  pt.col[sites$sites$habitat=="RB"] <- col.site[1]
  pt.col[sites$sites$habitat=="WS"] <- col.site[2]
  pt.col[sites$sites$habitat=="DS"] <- col.site[3]
  pt.col[sites$sites$habitat=="WD"] <- col.site[4]
  pt.col[sites$sites$habitat=="GN"] <- col.site[5]
  pt.border <- ifelse(sites$sites$current==1, 2, 1)
  
  map.ces(sites, type='s', col=pt.col, pch=21, cex=1.5*pch.cex, lwd=pt.border, 
          ylim=range(sites$sites$lat) + c(-map.border[1],map.border[1]), 
          xlim=range(sites$sites$lon) + c(-map.border[2],map.border[2]), mar=c(1, 1, 1, 1))
  rect(par('usr')[1], par('usr')[3], par('usr')[2], par('usr')[4])
  
  ### count of species ----
  par(fig=c(0, 0.4, 0, 0.67), mar=c(4, 7, 2, 1), new=TRUE)
  
  if( is.null(col.age) )
    col.age <- col.site[c(1,3)]  
  else
    col.age <- rep(col.age, 2)
  ad <- summary(cesobj, age=4, sp.order='count', silent=TRUE)$species
  jv <- summary(cesobj, age=3, sp.order='count', silent=TRUE)$species
  all <- merge(jv, ad, all=TRUE, by=c('Euring Code', 'Species'))
  names(all) <- c('code', 'species', 'juv', 'ad')
  all$juv[is.na(all$juv)] <- 0
  all$ad[is.na(all$ad)] <- 0
  all$tot <- all$juv + all$ad
  all <- all[order(all$tot, decreasing = TRUE), ]
  all <- all[1:n.species, ]
  
  if( unlist(options('ceslang')) == "Latin" ) # shorten scientific names, doesn't make sense else
    lab.name <- paste0(substr(all$species,1,1), ".", sub('^[[:alpha:]]*','',all$species))
  else 
    lab.name <- all$species
  ypos <- seq(n.species, 1)
  
  ptitle <- paste(n.species, "most caught species")
  plot(x=0, y=0, col="white", yaxt='n', xlab="Total Number of Captures", ylab='', main=ptitle, 
       xlim=c(1,max(all$tot)), ylim=c(1, n.species+0.2), bty='n', xpd=NA)
  points(x=all$tot, y=ypos, pch=19, cex=pch.cex, col=col.age[2])
  arrows(0, ypos, all$tot, ypos, length=0, col=col.age[2])
  points(x=all$juv, y=ypos, pch=19, cex=pch.cex, col=col.age[1])
  arrows(0, ypos, all$juv, ypos, length=0, col=col.age[1])
  axis(2, labels=lab.name, at=ypos, cex.axis=0.7, las=1)
  legend("bottomright", legend=c("Juvenile", "Adult"), bty="n", 
         col=col.age[1:2], pch=19)
  
  ### proportion retraps ----
  # remove within year retraps
  inds <- cesobj.dt[!cesobj.dt$retrap, ]
  # work out age at ringing
  inds <- cesobj.dt[ , roage:=min(age), by=ring]
  inds[ , retrap:=NULL] # saves a warning on the next line 
  # remove birds caught in multiple years, only want first retrap
  inds <- inds[ , retrap:=seq_len(.N), by=ring][retrap==2, ]
  # now count them
  retrap <- as.data.frame(xtabs(~species+roage, data=inds))
  retrap <- reshape::cast(retrap, species~roage, value='Freq', fun.aggregate = sum)
  names(retrap) <- c('code', 'juv.r', 'ad.r')
  retrap$code <- as.integer(as.character(retrap$code))
  retrap[is.na(retrap)] <- 0
  all <- merge(all, retrap, by='code', all.x=TRUE) # only need the top ones
  all[ , juv.p:=100*juv.r/juv]
  all[ , ad.p:=100*ad.r/ad]

  par(fig=c(0.4, 0.5, 0, 0.67), mar=c(4, 0, 2, 1), new=TRUE)
  plot(x=0, y=0, col="white", yaxt='n', xlab="% Retrap", ylab='', 
       xlim=c(1,max(all$juv.p,all$ad.p, na.rm=TRUE)), ylim=c(1, n.species+0.2), bty='n')
  axis(2, at=c(1,n.species),labels=FALSE, lwd.ticks=0)
  arrows(0, ypos-0.1, all$juv.p, ypos-0.1, length=0, col=col.age[1])
  arrows(0, ypos+0.1, all$ad.p, ypos+0.1, length=0, col=col.age[2])
  points(all$juv.p, ypos-0.1, pch=19, cex=pch.cex, col=col.age[1])
  points(all$ad.p, ypos+0.1, pch=19, cex=pch.cex, col=col.age[2])
  
  ### visit dates ----
  par(fig=c(0.5,0.85,0.33,0.67), mar=c(4,4,3,0), new=TRUE)
  
  vis.tab <- as.matrix(xtabs(~visit+julian, data=cesobj))
  nvisits <- nrow(vis.tab)
  dates <- as.integer(dimnames(vis.tab)[[2]])
  vis.start <- apply(vis.tab, 1, function(x) dates[min(which(x>0))])
  vis.q1 <- apply(vis.tab, 1, function(x) dates[max(which(cumsum(x)<(sum(x)*0.25)))])
  vis.q3 <- apply(vis.tab, 1, function(x) dates[min(which(cumsum(x)>(sum(x)*0.75)))])
  vis.end <- apply(vis.tab, 1, function(x) dates[max(which(x>0))])

  plot(-1,-1, xlim=range(dates), ylim=c(1,nvisits), las=1, bty='n', xaxt='n', yaxt='n',
       xlab='Day of Year', ylab='Visit Number', main="Catches by Visit")
  axis(1, at=c(121,135.5,152,167,182,197.5,213,228.5,244),
             c('1 May','','1 June','','1 July','','1 Aug','','1 Sep'))
  axis(2, at=seq(1,nvisits), labels=seq(1,nvisits), las=1)
  arrows(vis.start, seq(1,nvisits), vis.end,seq(1,nvisits), lwd=lwd, length=0, col=col.site[1])
  arrows(vis.q1, seq(1,nvisits), vis.q3,seq(1,nvisits), lwd=3*lwd, length=0, col=col.site[1])

  ### number of each visit ----
  par(fig=c(0.85,1,0.33,0.67), mar=c(4,1,3,1), new=TRUE)
  
  cc <- unique(cesobj.dt[ , c("year","sitename","visit")])
  visit.count <- table(cc$visit)
  
  xmin <- ifelse(min.visit < 1, min.visit*min(visit.count), min.visit)

  plot(-1,-1, xlim=c(xmin,max(visit.count)), ylim=c(1,nvisits), las=1, bty='n', yaxt='n',
       xlab='Number', ylab='')
  arrows(rep(1,nvisits), seq(1,nvisits), visit.count, seq(1,nvisits), 
         lwd=5*lwd, length=0, lend="square", col=paste0(col.site[1],"99"))
  abline(v=max(visit.count), lwd=lwd, lty=3)
  
  ### catch per visit ----
  par(fig=c(0.5,1,0,0.25), mar=c(4,4,1,1), new=TRUE)

  ad <- cesobj.dt[age==4, .N, by=c('visit','year')]
  jv <- cesobj.dt[age==3, .N, by=c('visit','year')]
  all  <- merge(ad, jv, by=c('visit','year'), all=TRUE)

  nvis <- cc[ , .N , by=c("year","visit")]

  all <- merge(all, nvis, by=c("year","visit"), all.x=TRUE)
  names(all) <- c('year', 'visit', 'ads', 'juvs', 'nvis')
  all$ads[is.na(all$ads)] <- 0
  all$juvs[is.na(all$juvs)] <- 0
  all$ads <- all$ads / all$nvis
  all$juvs <- all$juvs / all$nvis
  visits <- seq(min(all$visit), max(all$visit))
  ad.mean <- aggregate(all$ads, list(all$visit), mean)$x
  jv.mean <- aggregate(all$juvs, list(all$visit), mean)$x
  ad.min <- aggregate(all$ads, list(all$visit), min)$x
  jv.min <- aggregate(all$juvs, list(all$visit), min)$x
  ad.max <- aggregate(all$ads, list(all$visit), max)$x
  jv.max <- aggregate(all$juvs, list(all$visit), max)$x

  plot(-1, -1, xlim=range(visits), ylim=range(ad.min,ad.max,jv.min,jv.max), las=1,
       xlab='Visit Number', ylab='Mean Captures', bty='n', xaxt='n')
  axis(1, at=seq(1,nvisits), labels=seq(1,nvisits))
  points(visits-0.1, jv.mean, pch=19, cex=pch.cex, col=col.age[1])
  arrows(visits-0.1, jv.min, visits-0.1, jv.max, length=0, col=col.age[1])
  points(visits+0.1, ad.mean, pch=19, cex=pch.cex, col=col.age[2])
  arrows(visits+0.1, ad.min, visits+0.1, ad.max, length=0, col=col.age[2])
  
  ## Recaps by visit ----
  recap.vis <- as.data.frame(xtabs(~visit+age+retrap, data=cesobj.dt))
  recap.vis <- reshape::cast(recap.vis, visit+age~retrap, value='Freq', fun.aggregate=sum)
  recap.vis$visit <- as.numeric(as.character(recap.vis$visit))
  names(recap.vis) <- c('visit', 'age', 'N', 'R')
  recap.vis$ppn <- 100 * recap.vis$R / (recap.vis$N + recap.vis$R)

  par(fig=c(0.5,1,0.25,0.33), mar=c(0.5,4,1.5,1), new=TRUE)
  plot(-1, -1, xlim=range(visits), ylim=c(0,max(recap.vis$ppn,na.rm=TRUE)), las=1,
       xlab='', ylab='% Retrap', bty='n', xaxt='n')
  axis(1, labels=NA, at=seq(1,nvisits))
  arrows(recap.vis$visit[recap.vis$age==3]-0.1, 0,
         recap.vis$visit[recap.vis$age==3]-0.1, recap.vis$ppn[recap.vis$age==3],
         length=0, col=col.age[1])
  points(recap.vis$visit[recap.vis$age==3]-0.1, recap.vis$ppn[recap.vis$age==3], pch=19, col=col.age[1])
  arrows(recap.vis$visit[recap.vis$age==4]+0.1, 0,
         recap.vis$visit[recap.vis$age==4]+0.1, recap.vis$ppn[recap.vis$age==4],
         length=0, col=col.age[2])
  points(recap.vis$visit[recap.vis$age==4]+0.1, recap.vis$ppn[recap.vis$age==4], pch=19, col=col.age[2])
  
  
}
