plot.ces.markfit<-
function(x){
  
  if ( !class(x)[1] == 'ces' | !class(x)[2] == 'markfit' )
    stop("No information to plot\n")
  
  op <- par(mfrow=c(2,1), mar=c(2,4,1,1), oma=c(1,1,2,1)) 
  col.list=c('black','red','blue','green','purple','orange')
  
  x$survival$col <- col.list[1]
  subtitle=''
  if( !is.na(x$group$name) ){
    tmp <- x$survival[0,]
    for(i in 1:length(x$group$level)){
      rows <- grep(x$group$level[i],x$survival$group)
      tmp <- rbind(tmp,x$survival[rows,],rep(NA,length(x$survival[,1])))
      tmp$col[rows+i-1] <- col.list[i] # need to account for inserted NA's
      subtitle=paste(subtitle, col.list[i],':',x$group$level[i],sep=' ')
    }
    x$survival <- tmp
  } 
  
  s <- seq(length(x$survival$years)-1)
  xlim <- c(min(x$survival$years,na.rm=T), max(x$survival$years,na.rm=T))
  plot(1, type="n", xlim=xlim, ylim=c(0,1), ylab='Adult survival', las=1)
  segments(x0=x$survival$years[s], y0=x$survival$estimate[s],
           x1=x$survival$years[s+1], y1=x$survival$estimate[s+1],col=x$survival$col)
  segments(x0=x$survival$years[s], y0=x$survival$lcl[s],
           x1=x$survival$years[s+1], y1=x$survival$lcl[s+1],col=x$survival$col,lty=2)
  segments(x0=x$survival$years[s], y0=x$survival$ucl[s],
           x1=x$survival$years[s+1], y1=x$survival$ucl[s+1],col=x$survival$col,lty=2)
  title(main=sprintf("Estimates for %s",x$spp.name), outer=T)
  title(main=subtitle, line=-0.5, cex.main=0.75, outer=T)
  
  x0 <- (1:length(x$recapture$sitename))
  y0 <- x$recapture$estimate
  yus <- x$recapture$estimate + x$recapture$se + 0.001
  yls <- x$recapture$estimate - x$recapture$se + 0.001
  plot(x=x0, y=y0, type='p', xlab="",ylab="P(recapture)",ylim=c(0,1), las=1)
  arrows(x0,y0, x0,yus, angle=90, length=0.05)  # issues warning when trying to add
  arrows(x0,y0, x0,yls, angle=90, length=0.05)  # arrows to refyear with se=0
  
  par(op)
}