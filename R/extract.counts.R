extract.counts <-
function(data, species=0, age=0, plots=NULL){

  if ( !(class(data)[1]=='ces' & class(data)[2]=='data')  )
    stop("Please supply a CES data object\n")
  if ( age != 3 & age != 4 )
    stop("Invalid age - use either 3 (juvenile) or 4 (adult)")
  if ( is.null(plots) )
    stop('need to use extract.coverage() to determine when plots operated')
  
  selspp <- species
  selage <- age
  
  data.in <- data.table::data.table(data, key=c('site', 'age'))
  
  # sort out the input dataset
  if ( species == 0 )
    data.in <- data.in[age %in% selage, .(site, visit, year, ring)]
  else
    data.in <- data.in[species %in% selspp & age %in% selage, .(site, visit, year, ring)]
  
  data.in <- data.in[!is.na(data.in$ring), ]
  data.in[ , sy := paste(site, year, sep='_')]
  covered <- plots$years$sy
  data.in <- data.in[sy %in% covered] # in case sites/years have been excluded
  
  if( nrow(data.in) == 0 )
    stop('No data for species ', species, '\n')
  
  # select plots to include
  include <- data.table::data.table(plots$years)
  
  if( selage == 4 ){
    include <- include[((nvisits >= plots$min.visits) & early==1)]
  } else 
    include <- include[((nvisits >= plots$min.visits) & late==1)]
  
  # calculate catch totals - this is for individuals (i.e. 'new for year')
  data.in1 <- unique(data.in, by=c('site','year','ring'))
  if( nrow(data.in1) == 0 )
    stop('no data selected, check coverage periods specified correctly')
  catch.totals <- data.in1[ , .N, by=c('site', 'year')]
  data.table::setnames(catch.totals, 'N', 'totcaps')
  catch.totals[ , sy := paste0(site, '_', year)]
  
  # need to get totals for all sites including the zero catches 
  catch.totals <- merge(catch.totals, include, by='sy', all=TRUE)
  # neat way of adding multiple columns, need to make sure all records have a site and year... 
  catch.totals[ , `:=`(site=ifelse(is.na(site.x),site.y, site.x), 
                       year=ifelse(is.na(year.x),year.y, year.x))]
  catch.totals[is.na(totcaps), totcaps := 0]
  catch.totals[is.na(nvisits), nvisits := 0]
  # remove extraneous variables
  catch.totals[ , c('site.x', 'year.x', 'site.y', 'year.y', 'early', 'late') := NULL]
  
  data.in <- merge(data.in[ , .(visit, ring, sy)], include, by='sy', all=TRUE)
  data.in[ , complete := (nvisits==plots$all.visits)]

  # first calculate number caught in complete years by visit and totals by year
  data.complete <- data.in[complete==TRUE, .(site, year, visit, ring)]
  # set up the results dataframe
  years <- c(min(data.in1$year), max(data.in1$year))
  res <- data.table::data.table(expand.grid(unique(include$site), years[1]:years[2]))
  data.table::setnames(res, c('site', 'year'))
  res <- merge(res, catch.totals, by=c('site','year'), all.y=TRUE) # all.y should drop non-existent combinations
  res[ , c('N', 'Nprime') := 0 ] 
  
  missing.visits <- data.table::data.table(plots$missing.visits[ , seq(1,4)], key=c('site','year'))
  sites <- data.table(site=unique(data.in1$site))
  complete_sites <- unique(data.complete$site)
  # sites with more than 10 individuals get a specific corr factor
  sites <- merge(sites, data.complete[ , .N, by=site][N > 10], by='site', all.x=TRUE) 
  
  for( i in 1:nrow(sites) ){
    s <- as.integer(sites[i, .(site)])
    # if no complete years or fewer than 10 individuals caught in complete years use all sites data, o/wise use data for that site
    if( is.na(sites[i, .(N)]) )
      use.data <- data.complete
    else
      use.data <- data.complete[site==s]
    for( j in years[1]:years[2]){
      if( data.in[(site==s & year==j), .N] > 0 ){ 
        selrow <- which(res$site==s & res$year==j)
        set(res, selrow, 'N', uniqueN(use.data[ , .(ring)]))
        mis_vis <- missing.visits[(site==s & year==j), visit]
        if( length(mis_vis) > 0 ){
          use.data.miss <- use.data[!(visit %in% mis_vis), ]
          set(res, selrow, 'Nprime', uniqueN(use.data.miss[ , .(ring)]))
        }
      }
    }
  }

  res$totcaps[is.na(res$totcaps)] <- 0 # no birds caught
  res$Nprime[res$Nprime == 0] <- res$N[res$Nprime == 0] # complete visits
  res$corrcaps <- 0
  res$corrcaps[res$totcaps > 0] <- res$totcaps[res$totcaps > 0] * 
                                    res$N[res$totcaps > 0] / res$Nprime[res$totcaps > 0]
#  res[ , c('N', 'Nprime'):= NULL] # dont think we need these?

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

