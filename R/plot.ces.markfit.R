plot.ces.markfit<-
function(cesobj){
  
  if ( !class(cesobj)[1] == 'ces' | !class(cesobj)[2] == 'markfit' )
    stop("No information to plot\n")
  
  surv.est <- cesobj$survival
  cap.est <- cesobj$recapture
  group <- cesobj$group
  
  subtitle=''
  col.list=c('black', 'red', 'blue', 'green', 'purple', 'orange')
  surv.est$col <- col.list[1]
  op <- par(mfrow=c(2,1), mar=c(2,4,1,1), oma=c(1,1,2,1)) 

  if( !is.na(group$name) ){
    tmp <- surv.est[0, ]
    for(i in 1:length(group$level)){
      rows <- grep(group$level[i], surv.est$group)
      tmp <- rbind(tmp, survest[rows, ], rep(NA,length(survest[ , 1])))
      tmp$col[rows+i-1] <- col.list[i] # need to account for inserted NA's
      subtitle=paste(subtitle, col.list[i], ':', group$level[i], sep=' ')
    }
    surv.est <- tmp
  } 
  
  s <- seq(length(surv.est$years)-1)
  xlim <- c(min(surv.est$years, na.rm=TRUE), max(surv.est$years, na.rm=TRUE))
  plot(1, type="n", xlim=xlim, ylim=c(0, 1), ylab='Adult survival', las=1)
  segments(x0=surv.est$years[s], y0=surv.est$estimate[s],
           x1=surv.est$years[s+1], y1=surv.est$estimate[s+1], col=surv.est$col)
  segments(x0=surv.est$years[s], y0=surv.est$lcl[s],
           x1=surv.est$years[s+1], y1=surv.est$lcl[s+1], col=surv.est$col,lty=2)
  segments(x0=surv.est$years[s], y0=surv.est$ucl[s],
           x1=surv.est$years[s+1], y1=surv.est$ucl[s+1], col=surv.est$col,lty=2)
  title(main=sprintf("Estimates for %s", cesobj$spp.name), outer=TRUE)
  title(main=subtitle, line=-0.5, cex.main=0.75, outer=TRUE)
  
  x0 <- (1:length(cap.est$sitename))
  y0 <- cap.est$estimate
  yus <- cap.est$estimate + cap.est$se + 0.005
  yls <- cap.est$estimate - cap.est$se + 0.005

  plot(x=x0, y=y0, type='p', xlab="",ylab="P(recapture)",ylim=c(0,1), las=1)
  arrows(x0,y0, x0,yus, angle=90, length=0.05)  # issues warning when trying to add
  arrows(x0,y0, x0,yls, angle=90, length=0.05)  # arrows to refyear with se=0
  
  par(op)
}