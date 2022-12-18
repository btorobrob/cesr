extract.counts <-
function(cesobj, species=0, age=0, plots=NULL){
  
  if ( !(class(cesobj)[1]=='ces' & class(cesobj)[2]=='data')  )
    stop("Please supply a CES data object\n")
  if ( age != 3 & age != 4 )
    stop("Invalid age - use either 3 (juvenile) or 4 (adult)")
  if ( is.null(plots) )
    stop('Need to use extract.coverage() to determine when plots operated')
  
  selspp <- species
  selage <- age
  
  data.in <- data.table::data.table(cesobj, key=c('site', 'age'))
  
  # sort out the input dataset
  if ( species == 0 )
    data.in <- data.in[age %in% selage, .(site, visit, year, ring)]
  else
    data.in <- data.in[species %in% selspp & age %in% selage, .(site, visit, year, ring)]
  
  data.in <- data.in[!is.na(data.in$ring), ]
  data.in[ , sy := paste(site, year, sep='_')]
  covered <- plots$years$sy
  data.in <- data.in[sy %in% covered] # in case sites/years have been excluded
  
  ## Use warnings in case juvenile faulty, but adults OK?
  if( nrow(data.in) == 0 ){
    if( selage == 3 )
      warning('no data for juveniles of this species', call.=FALSE)
    else if( selage == 4 )
      warning('no data for adults of this species', call.=FALSE)
    return(NULL)
  }
  
  # select plots to include
  include <- data.table::data.table(plots$years)
  
  if( selage == 4 ){
    include <- include[((nvisits >= plots$min.visits) & early==1)]
  } else 
    include <- include[((nvisits >= plots$min.visits) & late==1)]
  
  # calculate catch totals - this is for individuals (i.e. 'new for year')
  data.in1 <- unique(data.in, by=c('site','year','ring'))
  if( nrow(data.in1) == 0 ){
    if( selage == 3 )
      warning('no juvenile data selected, check coverage periods specified correctly', call.=FALSE)
    else if( selage == 4 )
      warning('no adult data selected, check coverage periods specified correctly', call.=FALSE)
    return(NULL)
  }
  catch.totals <- data.in1[ , .N, by=c('site', 'year')]
  data.table::setnames(catch.totals, 'N', 'totcaps')
  catch.totals[ , sy := paste0(site, '_', year)]
  
  # need to get totals for all sites including the zero catches 
  catch.totals <- merge(catch.totals, include, by='sy', all.y=TRUE)
  # neat way of adding multiple columns, need to make sure all records have a site and year... 
  catch.totals[ , `:=`(site=ifelse(is.na(site.x), site.y, site.x), 
                       year=ifelse(is.na(year.x), year.y, year.x))]
  catch.totals[is.na(totcaps), totcaps := 0]
  # remove extraneous variables
  catch.totals[ , c('site.x', 'year.x', 'site.y', 'year.y', 'early', 'late') := NULL]
  
  data.in <- merge(data.in[ , .(visit, ring, sy)], include, by='sy', all.y=TRUE)
  data.in[ , complete := (nvisits==plots$all.visits)] 
  
  # first calculate number caught in complete years by visit and totals by year
  data.complete <- data.in[complete==TRUE, .(site, year, visit, ring)]
  data.complete <- data.complete[!is.na(visit), ] # remove years when no birds of this spp caught
  if( nrow(data.complete) == 0 ){ # continuing throws an error in calcNprime
    if( selage == 3 )
      warning("no juvenile birds caught on complete visits", call.=FALSE)
    else if( selage == 4 )
      warning("no adult birds caught on complete visits", call.=FALSE)
    return(NULL)
  }

  missing.visits <- data.table::data.table(plots$missing.visits, key=c('site','year'))
  sites <- data.table(site=unique(data.in$site))
  complete_sites <- unique(data.complete$site)
  # sites with more than 25 individuals get a specific corr factor
  sites <- merge(sites, data.complete[ , .N, by=site][N >= 25], by='site', all.x=TRUE) 
  sites[is.na(N), N := 0 ] # sites with no complete visits
  
  data.complete[ , ring:=paste0(site,year,ring)] # ugly, but o/wise duplicated doesnt work when birds move sites!
  ninds <- data.complete[!duplicated(ring) , .N, by=site]
  sites <- merge(sites, ninds, by='site', all.x=TRUE)
  names(sites) <- c('site', 'ncaps', 'N')
  
  res <- merge(catch.totals, sites, by='site', all.x=TRUE)
  n.small <- data.complete[!duplicated(ring) , .N, ]
  data.complete[ , ring := as.numeric(as.factor(ring))]
  data.complete <- data.complete[!is.na(visit), ] # not sure this is needed?
  n.prime <- calcNprime(as.data.frame(data.complete), as.data.frame(missing.visits), as.data.frame(sites))
  years <- c(min(data.complete$year), max(data.complete$year))
  n.prime.df <- data.frame(site = rep(sites$site, length(seq(years[1],years[2]))),
                           year = rep(seq(years[1],years[2]), each=uniqueN(sites$site)),
                           nprime = c(n.prime))
  res <- merge(res, n.prime.df, by=c('site', 'year'), all.x=TRUE)
  res[is.na(res$totcaps), totcaps := 0] # no birds caught, shouldn't happen, but just in case
  res[(N<25 & ncaps==0), N := n.small] # use global correction for those no complete data
  res[ , corrcaps := totcaps * (N/nprime)]
  
  # add some useful info
  res <- merge(res, plots$sites[ , c('site', 'sitename', 'nyears')], by='site', all.x=TRUE)

  if( species == 0 ){
    return(data.frame(res))
  } else {
    if( age == 3 ){
      result <- list(ad.data = NULL, 
                     jv.data = data.frame(res),
                     species = species)
    } else {
      result <- list(ad.data = data.frame(res), 
                     jv.data = NULL,
                     species = species)
    }
  }
  class(result) <- c('ces', 'counts', 'list')
  result
}

