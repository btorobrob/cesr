writeces.plots  <- 
function(x, file, verbose=TRUE){

  if ( !class(x)[1]=='ces' )
    stop("Please supply a CES object\n")     
  if ( !x$header[[1]]=='plots' )
    stop("Please supply a CES plots object\n")
  if ( file == "" ) 
    file <- file.choose()

  # summarise plot coverage and number of visits
  earlyv <- x$coverage[x$coverage$per=='E', c(1,2,4)]
  latev <- x$coverage[x$coverage$per=='L', c(1,2,4)]
  names(earlyv)[3] <- 'N_Early'; names(latev)[3] <- 'N_Late'
  allv <- merge(earlyv, latev)
  allv$N_Visits <- allv$N_Early + allv$N_Late
  allm <- expand.grid(unique(allv$site), min(allv$year):max(allv$year))
  colnames(allm) <- c('site', 'year')
  allm <- merge(allm, allv, all.x=TRUE)
   
  # now work out which visits are missing 
  mv <- x$missing.visits
  mv$sy <- paste(as.character(mv$site), as.character(mv$year), sep="_")
  if ( verbose ) { # column for each visit
    tab.mv <- as.data.frame(xtabs(~sy+visit, data=mv))
    tab.mv$Freq <- -1 * (tab.mv$Freq-1)  # swaps 1 and 0, so missing is 0
    colnames(tab.mv) <-c ('sy', 'visitno', 'visit') # means cols are labelled usefully
    mis.vis <- reshape(tab.mv, idvar='sy', v.names='visit', timevar='visitno', direction='wide')
    sites <- unique(mv[,-3])
    sites <- merge(sites, mis.vis, all.x=TRUE)
  } else { # collapse columns into string
    tab.mv <- as.matrix(xtabs(~sy+visit, data=mv))
    # replace 1's (missing) with visit number...
    val.mv <- matrix(rep(c(min(mv$visit):max(mv$visit)), nrow(tab.mv)), nrow=nrow(tab.mv), ncol=ncol(tab.mv), byrow=TRUE)
    sum.mv <- val.mv * tab.mv
    # ...and this bit pastes those into one string...
    mv.str <- rep( '_', length(sum.mv[,1]) )
    for ( i in 1:length(sum.mv[,1]) )
      mv.str[i] <- paste(sum.mv[i,], collapse=',')
    # ...now merge it back into the main dataset
    sites <- unique(mv[,-3])                    
    mis.vis <- as.data.frame(cbind(rownames(sum.mv),mv.str))
	names(mis.vis) <- c('sy', 'Missing')  
    sites <- merge(sites, mis.vis, all.x=TRUE)
    sites$Missing <- gsub(',', ' ', sub('0','',gsub(',0','',sites$Missing)))
    # if first visit is missing no need to remove leading space
    sites$Missing[substr(sites$Missing,1,1)==' '] <- sub(' ','',sites$Missing[substr(sites$Missing,1,1)==' '])
  }

  sites <- sites[ ,-1]
  allm <- merge(allm, sites, all.x=TRUE)
  if ( verbose ) {
    max.vis <- max(allm$N_Visits)                       
    for ( i in 1:max.vis ) {
      allm[allm$N_Visits==max.vis,i+5] <- 1
      allm[allm$N_Visits==0,i+5] <- 0
    }
  }
  
  write.table(allm, file=file, sep=',', row.names=FALSE)

}

