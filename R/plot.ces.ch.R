plot.ces.ch <- 
function(cesobj, col=c('red', 'blue'), sitelist=NULL, cex=0.75, xlab='Occasion', ylab='Site', main=''){
  
  chdata <- cesobj$chdata

  # check the sitenames are valid
  if( !is.null(sitelist) ){
    if( length(which(sitelist %in% chdata$sitename)) < length(sitelist) ){
      ok <- which(sitelist %in% site.names)
      if( length(ok) == 0 )
        stop('no sites found', call.=FALSE)
      else
        warning(paste('some sites not found:', paste(sitelist[-ok], collapse=" ")), call.=FALSE)
      sitelist <- sitelist[ok]
    }
    chdata <- chdata[chdata$sitename %in% sitelist, ]
  }
  
  # create a function to clear the start/end of the capture histories
  fillch <- function(x){
    f <- min(which(x==1)) # first capture
    l <- max(which(x==1)) # last capture
    c(rep(NA,(f-1)), x[f:l], rep(NA,(length(x)-l)))
  }
  
  # create table of unique ch by site
  tbl.ch <- table(paste(chdata$ch,'_', chdata$site, sep=''))
  
  # get the ch and their site numbers
  tmp <- strsplit(dimnames(tbl.ch)[[1]], '_') 
  
  # combine the data...
  df <- data.frame(ch = as.character(sapply(tmp, function(x) x[1])), # the first element of each entry in tmp
                   site = as.integer(sapply(tmp, function(x) x[2])), # and the second
                   n = as.integer(tbl.ch), stringsAsFactors=FALSE)
  #... sort it ...
  data.srt <- df[order(df$site, df$ch, decreasing=c(FALSE,TRUE)), ]
  # ... convert it into a matrix ...
  data.mat <- matrix(unlist(strsplit(data.srt$ch,split='')), 
                     nrow=length(data.srt$ch), ncol=nchar(data.srt$ch[1]), byrow=TRUE)
  data.mat[data.mat == "."] <- NA # doing this first avoids warnings about NAs by coercion
  data.mat <- t(apply(data.mat, 1, as.numeric)) # apply rotates the matrix for some reason
  # ... clear the initial/end capture occasions ...
  data.mat <- t(apply(data.mat, 1, fillch)) # and use t() again
  # ... and finally convert captures into n.captures for colouring by image()
  data.mat <- data.mat * df$n
  
  # now the colours
  col1 <- paste("#", paste(as.hexmode(col2rgb(col[1])),collapse=''),"44", sep="") # not captured
  col2 <- paste(as.hexmode(col2rgb(col[2])), collapse='') # captured base color
  cols <- c(col1, paste("#", col2, c("22","66","AA","CC"), sep='')) # apply transparency
  
  # now breaks and labels
  if( max(df$n) > 10 )
    breaks <- c(-0.1, 0.1, 1.1, 4.1, 10.1, 1000)
  else
    breaks <- c(-0.1, 0.1, 1.1, 3.1, 5.1, 1000)
  tick.pos <- c(0, cumsum(table(data.srt$site))+0.5)
  lab.pos <- tick.pos[-length(tick.pos)] + (diff(tick.pos)/2) # plot the labels at the midpoints
  labs <- unique(chdata$sitename)
  
  # and finally the plotting
  op <- par(mar=c(2.1, 4.1, 3.1, 2.1))
  image(x=seq(1,ncol(data.mat)), y=seq(1,nrow(data.mat)), z=t(data.mat), useRaster=TRUE,
        col=cols, breaks=breaks, bty='n', xaxt='n', yaxt='n', xlab="", ylab=ylab, main=main)
  axis(1, at=seq(1,ncol(data.mat)), labels=rep("", ncol(data.mat)))
  mtext(side=1, text=xlab, line=1)
  axis(2, at=tick.pos, labels=rep("",length(tick.pos)), lty=NULL) # don't draw the line for tidiness sake
  axis(2, at=lab.pos, labels=labs, las=1, tick=FALSE, cex.axis=cex)
  par(op)
  
}