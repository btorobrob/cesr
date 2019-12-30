# INSERTING NA's - SEE SFH DATA
# extract.ch(SFH,species=13080,plots=SFHcov)

extract.ch <-
function(x, species=0, late=FALSE, group=NA, exclude=NULL, min.n=0, plots=NULL){
  
  ## creates the extra time period by using the year-1 column
  ## this works because we are only estimating adult survival, so one age group
  
  if( !(class(x)[1] == 'ces' & class(x)[2] == 'data')  )
    stop("No ces data\n")

  if( is.null(plots) )
    stop("no plot data, use extract.coverage() first and give plots=")
  
  if ( species == 0 ) {
    if ( length(unique(x$species)) == 1 ) 
      species <- as.character(unique(x$species))
    else
      stop("please supply a Euring species code, using species=")
  } 
  
  selspp <- species # to avoid name conflicts in x as a data.table

  # first birds captured as adults
  x <- data.table::data.table(x, key=c('species','age'))
  x <- x[ .(selspp, 4) ]
  
  ind <- x[ , list(ring, site, sex, year) ] # pulls out adults
  ind <- ind[ !(is.na(ring)), ] # remove species for which no adults caught

  if( nrow(ind) == 0 )
    stop(paste('No records for', selspp))
  
  first.year <- min(ind$year)
  last.year <- max(ind$year)
  
  ind[ , yearno := (year-first.year+1) ]
  ind[ , ind := paste(ring, site, sep='_') ]
  
  if ( min.n > 0 ){
    xx <- ind[ , list(nencs=.N), by=site]
    xx <- xx[(nencs > min.n)]
    data.table::setkey(xx, 'site')
    data.table::setkey(ind, 'site')
    ind <- ind[xx][ , nencs := NULL]
  }
  
  # extract years of operation
  py <- data.table::data.table(plots$years)
  if ( late == TRUE ){
    years <- py[(early == 1 & late == 1), list(site, year, nvisits)]
  } else { # most adults caught in first 6 visits (at least in UK) so this is default
    years <- py[(early == 1), list(site, year, nvisits)]
  }
  allyrs <- seq(min(years$year), max(years$year)) # check whether any years not represented
  missyrs <- allyrs[!(allyrs %in% unique(years$year))]
  if( length(missyrs) > 0 ){ # if there are missing years, add some rows with a warning
    warning(paste("some years appear to be missing, check your data:", paste(missyrs, collapse=" ")), call.=FALSE)
    years <- rbindlist(list(years,as.data.table(cbind(year=missyrs))), use.names=TRUE, fill=TRUE)
  }
  years[ , site := as.numeric(site) ]

  data.table::setkey(ind, year, site) # set the keys to merge data
  data.table::setkey(years, year, site)
  data <- merge(ind, years, all.x=TRUE)
  # and remove birds from non-included years or sites when no birds caught
  data <- data[!is.na(data$nvisits) & !is.na(data$site)] 
  numyrs <- max(data$yearno) - min(data$yearno) + 1
  # now determine missing years - get years (col 2) as columns
  cov <- data.table::data.table(reshape::cast(reshape::melt(years[ ,list(site, year)], id='site'), site~value)) 
  tmp <- cov[ , 'site', with=FALSE] # this is kludgy
  cov[!is.na(cov)] <- '0'   # need these characters for the capture history
  cov[is.na(cov)] <- '.'
  cov[ , site := tmp]
  data.table::setnames(cov,c('site', paste0('cov', min(years$year):max(years$year)))) # maybe more than in ind
     
  # set-up capture matrix
  years <- matrix(0, ncol=nrow(data), nrow=numyrs)
  #  no idea how this works, but it seems to
  years[data$yearno + (1:ncol(years)-1)*nrow(years)] <- 1
  # transpose so individuals are rows rather than columns
  years <- t(years)
  
  data1 <- data.table::data.table(data, years)
  
  xx <- data1[ , c('ind', paste0('V',1:numyrs)), with=FALSE]
  
  yrcols <- paste0('V',1:numyrs)
  sum2 <- function(x) min(sum(x), 2) # effectively loses multiple captures
  yearsum <- xx[ , lapply(.SD, sum2), by=ind, .SDcols=yrcols]  

  setnames(yearsum, c('ind', paste0('sum', (min(data$year)):max(data$year))))
  sc <- paste0('sum', min(data$year):max(data$year)) # select the year columns
  yearsum$n_encs <- apply(as.matrix(yearsum[ ,.SD, .SDcols=(sc)]), 1, sum) # this is much faster than using :=

  data.table::setkey(yearsum, 'n_encs')
  nzero <- nrow(yearsum[0])
  if( nzero > 0 ){
    wmessage <- ifelse(nzero == 1,
                       c('histories with no captures encountered, 1 record deleted'),
                       paste('histories with no captures encountered,', nzero, 'records deleted'))
    warning(wmessage, call.=FALSE)
    yearsum <- yearsum[!J(0)]
  }
  yearsum[ , n_encs := NULL]
  # get siteno from ind
  yearsum[ , site := as.integer(matrix(unlist(strsplit(ind, '_')), ncol=2, byrow=TRUE)[ , 2])]
  
  # now work out year of first capture, gives no entries before the right entry, which is helpful
  yearsum$first <- apply(as.matrix(yearsum[ ,.SD, .SDcols=(sc)]), 1, function(x) min(which(x>0))) #again much faster than :=
  firstcol <- which(names(yearsum) == 'first') 
  data.table::setkey(yearsum, site)
  data.table::setkey(cov, site)
  
  yearsum <- merge(yearsum, cov) # WARNING: code below relies on site being first col then sum20xx columns
  sumcol <- grep('sum', names(yearsum))
  covcol <- grep('cov', names(yearsum))
  
  create_ch <- function(x, firstcol, covcol, sumcol){
    first <- as.integer(x[firstcol])
    if( first == 1 ) # caught on first occasion
      pre <- ''
    else
      pre <- paste0(x[covcol[1:(first-1)]], collapse='')
    catch <- paste0(1L, as.integer(ifelse(x[sumcol[first]]==1,0,1)), collapse='')
    if( first == length(sumcol) ) # caught on last occasion
      post <- ''
    else{
      post1 <- x[sumcol[(first+1):length(sumcol)]]
      post2 <- x[covcol[(first+1):length(covcol)]]
      post1[post2 == '.'] <- '.'
      post <- paste0(post1, collapse='')
    }
    paste0(pre, catch, post)
  }
  
  yearsum[ , ch := apply(yearsum, 1, create_ch, firstcol, covcol, sumcol)]  
  yearsum[ , ch := gsub('2', '1', ch)]
  yearsum[ , ring := matrix(unlist(strsplit(as.character(ind),'_')), ncol=2, byrow=T)[ , 1]]
  yearsum <- yearsum[ ,list(ring, ch, site=as.factor(site))]

  gplvls <- NA
  if ( !is.na(group) ){ # now get the grouping variable
    cols <- c(which(names(x) == 'ring'), which(names(x) == group))
    if( length(cols) != 2 ){
      gpmessage <- paste('grouping variable', group, 'not identified')
      warning(gpmessage, call.=FALSE)
    }
    g <- x[ ,.SD, .SDcols=(cols)]
    data.table::setnames(g, c('ring', 'group'))
    # this bit checks each ring has a unique group
    gtab <- xtabs(~ring+group, data=g)
    gind <- apply(gtab[ , 1:length(gtab[1,])], 1, which.max) # will identify the first group id if ties
    # assign birds to groups
    groups <- data.table(names(gind), dimnames(gtab)$group[gind])
    groups[ , V2 := as.factor(V2)]
    data.table::setnames(groups, old=c('V1','V2'), new=c('ring', 'group'))
    
    data.table::setkey(yearsum, ring)
    data.table::setkey(groups, ring)
    res <- merge(yearsum, groups, all.x=TRUE)
    res <- res[!is.na(group), ] # just in case?
    if ( !is.null(exclude) ){
      nmv <- nrow(res[ group %in% exclude ])
      if ( nmv > 0 ){
        excmessage <- paste('there were', nmv, 'records excluded with value(s)', paste(exclude, collapse=', '), sep=' ')
        warning(excmessage, call.=FALSE)
        res <- res[ !group %in% exclude, ]
      }
    } 
    
    gplvls <- unique(res$group)
    data.table::setnames(res, 'group', c(noquote(group)))

  } else 
    res <- yearsum
  
  lang <- options()$ceslang
  spp.name <- as.character(cesnames[cesnames$spp==species, which(colnames(cesnames)==lang) ])
  
  results <- list(mr_data = as.data.frame(res),
                  begin.time = min(data$year),
                  years = numyrs-1, 
                  sitenames = unique(x$sitename),
                  group = list(name=group, levels=gplvls),
                  spp.name = spp.name)
  class(results) <- c('ces', 'ch', 'list')
  results
}