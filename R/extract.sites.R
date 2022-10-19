extract.sites <-
function(cesdata, exclude=NULL){

  if( !(class(cesdata)[1]=='ces' & class(cesdata)[2]=='data')  )
    stop("Cannot extract from non-CES data\n")
  
  x <- data.table::data.table(cesdata, key = "site") 
  # get one row per site and just the needed variables
  sites <- unique(x[ , .(site, habitat, lat, long, netlength, sitename)], by='site')
  sites <- sites[!(as.character(sitename) %in% as.character(exclude))]
  
  cyr <- max(x$year)  # just use the last year present in the dataset
  
  # Number of years a site has been running
  nyears <- x[ , .(uniqueN(year), min(year), max(year)), by='site']
  data.table::setnames(nyears, c('site', 'nyears', 'first.yr', 'last.yr'))
  sites <- merge(sites, nyears, by='site')
  sites[ , current := ifelse(last.yr==cyr, 1, 0)]
  sites[ , miss.yrs := (last.yr-first.yr+1)-nyears]
  country <- table(cesdata$countryID)
  sites$country <- names(country[which.max(country)]) # in case more than one entry
  
  sites <- data.frame(sites)
  class(sites)<-c('ces', 'sites', 'data.frame')
  sites
}

