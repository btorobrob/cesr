writeces.counts <-
function(x, file="", na.string='.'){

  if ( !class(x)[1]=='ces' )
    stop("Please supply a CES object")     
  if( file == "" ) 
    file <- file.choose()

  ads <- x$ad.data[ , !names(x$ad.data) %in% c('N', 'Nprime', 'sy', 'Early', 'Late')]
  names(ads) <- c('site', 'year', 'adcaps', 'adexcaps', 'nvisits')
  jvs <- x$jv.data[ ,c(-4,-5)]
  names(jvs) <- c('site', 'year', 'jvcaps', 'jvexcaps', 'nvisits')
  all <- merge(ads, jvs)
  all$species <- x$header$species

  write.table(all, file=file, sep=',', row.names=FALSE, na=na.string)
}

