readces <-
function(file=NULL, visits='std', fill.sex=FALSE, group.race=FALSE){
  
  if( is.null(file) )
    file <- file.choose()
  
  if( !file.exists(file) )
    stop(paste("cannot find the file:", file, "please check typing and that are you in the right directory"))

    # get column names and work out how many
  coln <- strsplit(readLines(file, n=1), '[,;]')[[1]]
  # short form names
  var.names <- c('countryID', 'siteID', 'coords', 'habitat', 'visit',
                 'day', 'month', 'year', 'netlength', 'StartTime', 'EndTime',
                 'scheme', 'ring', 'species', 'sex', 'age', 'brood',
                 'moult', 'wing', 'weight', 'weighTime', 'p3', 'fat')
  # longer form ones
  alt.names <- c('Country_Identifier', 'Site_Identifier', 'sitename', 'coordinates', 
                 'site_coordinates', 'visit_period', 'visit_start_time', 'visit_end_time',
                 'total_net_length', 'ring_scheme', 'ring_number', 'brood_patch_score', 
                 'wing_length', 'mass', 'body_mass', 'time_of_weighing', 'length_p3', 
                 'fat_score', 'moult_state', 'habitat_type', 'start', 'end')
  # map back to the main list
  column_nos <- c(1:23, 1, 2, 2, 3, 3, 5, 10, 11, 9, 12, 13, 17, 19, 20, 20, 21,
                  22, 23, 18, 4, 10, 11)
  
  match.names <- adist(tolower(coln), tolower(c(var.names, alt.names)))
  which.names <- unlist(apply(match.names, 1, which.min))
  col.numbers <- column_nos[which.names]
  col.names <- var.names[col.numbers]
  
  if( any(duplicated(col.names) == TRUE) ){ # duplicates indicate something is amiss
    duplicated.names <- which(duplicated(col.names))
    col.names[duplicated.names] <- paste0('var', duplicated.names)
    wmsg <- paste('unrecognised column names:', paste(coln[duplicated.names], collapse=', '))
    warning(wmsg, call. = FALSE)
  }
  
  # this forces some structure but is maybe too restrictive?  
  #  ncol <- length(coln)
  #  if( ncol == 14 )
  #    col.names <- c('countryID','siteID','coords','habitat','visit','day','month','year','NetLength',
  #                  'scheme','ring','species','sex','age')
  #  else if( ncol == 23 )
  #    col.names <- c('countryID','siteID','coords','habitat','visit','day','month','year','NetLength',
  #                  'StartTime','EndTime','scheme','ring','species','sex','age','brood','moult','wing','weight','weighTime','p3','fat')
  #  else {
  #    col.names <- coln
  #    warning('unexpected number of columns read, names not checked which may cause errors', call.=FALSE)
  #  }
  
  result <- suppressWarnings(data.table::fread(file))
  # use suppressWarnings to avoid messages about bumping col classes late in the data
  
  setnames(result, col.names)
  
  # check species are valid Euring codes
  result[ , species := as.factor(species)]
  int_spp <- suppressWarnings(as.integer(as.character(result$species)))  # force them to be integer
  char_spp <- unique(as.character(result$species[is.na(int_spp)])) # select the rejected records
  if( length(char_spp) > 0 ){
    wmessage <- paste('non-numeric species codes:', paste(char_spp,collapse=', '), 'will be ignored')
    warning(wmessage, call.=FALSE)
    result <- result[!is.na(int_spp), ]
  }
  # now check if they are likely CES species
  dodgy <- unique(int_spp[!(int_spp %in% cesnames$spp) & !is.na(int_spp)])
  if( length(dodgy) > 0 ){
    wmessage <- paste('surprising species codes encountered:', paste(dodgy,collapse=', '), ', consider checking these')
    warning(wmessage, call.=FALSE)
  }

  # check that ages are all 3/4 for simplicity later on...
  if( length(result$age[!result$age %in% c(2, 3, 4)]) > 0 ){
    result[ , age1 := as.character(age)]
    result[ , age := NULL] # sometimes it is character, so start from scratch
    setkey(result, 'age1')
    ages <- sort(unique(result$age1))
    adult <- c('4','5','6','7','8','9','A','B','C','D','E','F','G','H','J','K','L','M','P','N','Q','R','S','T')
    result[age1 %in% adult, age := 4] 
    result[age1 %in% c('3','3J'), age := 3] 
    result[age1 == 2, age := 2] 
    result <- result[ age %in% c(2, 3, 4) ] # anything else we just delete, note should keep 2s 
    result[ , age1 := NULL]
    ages <- ages[!ages %in% c('2','3','3J','4','5','6')] # check for non 3/4 age codes
    if( length(ages) > 0 ){
      age.warning <- paste('unexpected age-codes:', paste(ages, collapse=','), 'encountered; recoded as 4 or deleted')
      warning(age.warning, call. = FALSE)
    }
  }
  result[ , age := as.integer(age)]
  
  # check sex
  result$sex[result$sex %in% c('1','3','5','8','m')] <-'M'
  result$sex[result$sex %in% c('2','4','6','9','f')] <-'F'
  result$sex[result$sex %in% c('0','7','-','U')] <- NA
  if( fill.sex ){
    tmpM <- result[ , (count=sum(sex=='M')), by=ring]
    tmpF <- result[ , (count=sum(sex=='F')), by=ring]
    tmp <- merge(tmpM, tmpF, by='ring', all=TRUE)
    tmp[ , sex := NA ]
    tmp$sex[tmp$V1.x > tmp$V1.y ] <-'M'
    tmp$sex[tmp$V1.y > tmp$V1.x ] <-'F'
    result <- merge(result, tmp, by='ring', all.x=TRUE)
    sex.change <- nrow(result[ ((sex.x != sex.y) & sex.x != '-'), ])
    if( sex.change > 0 )
      warning(paste(sex.change, 'encounters changed sex'), call.=FALSE)
    result[ , sex.x := NULL]
    setnames(result, 'sex.y', 'sex')
  }
  result[ , sex := as.factor(sex)]
  
  # set site names
  if( length(grep("[#]", as.character(result$siteID))) > 0 ) # yes, really - thanks to Arizaga
    warning("Using the '#' sign in sitenames confuses Mark, rename your sites!", call.=FALSE)
  result[ , sitename := as.factor(siteID)] 
  result[ , site := as.numeric(as.factor(siteID))]
  result[ , siteID := NULL] # no longer needed
  
  # remove races if required
  result[ , race := species]  # just so we know it is there
  if( group.race )
    result[ , species := (10 * floor(as.numeric(as.character(species))/10))]  # concatenate races, original code now in race

  # check for duplicated rings on different species (yes, really!)
  dup_spp <- unique(result[ , c('species','ring')], by=c('species', 'ring')) 
  dup_ndx <- duplicated(dup_spp, by=c('ring'))
  if( sum(dup_ndx) > 0 ){
    wmessage <- paste('rings associated with more than one species code:', paste(dup_spp$ring[dup_ndx],collapse=', '))
    warning(wmessage, call.=FALSE)
  }
  
  # check visits
  if( is.character(result$visit) ){
    setkey(result, visit)
    if( length(visits) == 1 & visits == 'std' ){
      stdv <- c ('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12')
      result <- result[visit %in% stdv]
      result[ , visit := as.integer(visit)]
    } else if ( length(visits) > 1 )
      result$visit <- result$visit[visits]
      ## Note this means that visit might be either numeric or character!
  } 
  
  # and that dates are numeric
  nmissd <- nmissm <- nmissy <- 0 # just so the if lower down doesn't fail
  if( !is.integer(result$day) ){
    suppressWarnings(result[ , day := as.integer(day) ])
    nmissd <- is.na(result$day)
  }
  if( !is.integer(result$month) ){
    suppressWarnings(result[ , month := as.integer(month) ])
    nmissm <- is.na(result$month)
  }
  if( !is.integer(result$year) ){
    suppressWarnings(result[ , year := as.integer(year) ])
    nmissy <- is.na(result$year)
  }
  count <- sum((nmissd + nmissm + nmissy) > 0)
  if( count > 0 ){
    wmessage <- paste('non-numeric dates detected in', count, 'records')
    warning(wmessage, call.=FALSE)
  }
  inv.days <- nrow(result[day < 1 | day > 31])
  if( inv.days > 0 ){
    wmessage <- paste(inv.days, 'day values outside the range 1-31 detected')
    warning(wmessage, call.=FALSE)
  }
  non.summer <- nrow(result[month<4 | month>9])
  if( non.summer > 0 ){
    wmessage <- paste(non.summer, 'records outside the period April to September, is this expected?')
    warning(wmessage, call.=FALSE)
  }
  # now create a Julian day column for doing phenology things
  # create jday first to avoid an error about using $ with atomic vectors
  jday <- function(d, m, y){ strptime(paste0(m, '/', d, '/', y), format="%m/%d/%Y")$yday }
  result[ , julian := jday(day, month, year)]

  # convert co-ordinates
  if( !is.character(result$coords) )
    warning('coordinates not in Euring format', call. = FALSE)
  coords <- result$coords
  # do this to avoid a cryptic warning about NAs being coerced
  c1 <- suppressWarnings(as.integer(substr(coords,1,3)))
  c2 <- suppressWarnings(as.integer(substr(coords,4,5))/60)
  c3 <- suppressWarnings(as.integer(substr(coords,6,7))/3600)
  if( anyNA(c(c1, c2, c3)) )
    warning("missing values generated for latitude, are all the coordinates 15 characters long?", call.=FALSE)
  result[ , lat := c1 + c2 + c3]
  ew <- ifelse(substr(coords,8,8) == '-', -1 , 1)
  llfmt <- max(nchar(coords), na.rm  = TRUE) # does coords have 14 or 15 characters

  if ( llfmt == 15 ){   # Euring code specifies 15 chars, but just in case long deg is 2 digits rather than 3
    if( anyNA(as.integer(substr(coords,9,11))) | anyNA(as.integer(substr(coords,12,13))) | anyNA(as.integer(substr(coords,14,15))) )
      warning("missing values generated in longitude, check for stray non-numeric characters", call.=FALSE)
    c1 <- suppressWarnings(as.integer(substr(coords,9,11)))
    c2 <- suppressWarnings(as.integer(substr(coords,12,13)))/60
    c3 <- suppressWarnings(as.integer(substr(coords,14,15)))/3600
    if( max(c2, c3, na.rm=TRUE) > 60 )
      warning("values greater than 60 detected in latitude minutes/seconds, check coordinate format", call.=FALSE)
    result [ , long := ew * (c1 + c2 + c3)]
  } else if ( llfmt == 14 ){
    if( anyNA(as.integer(substr(coords,9,10))) | anyNA(as.integer(substr(coords,11,12))) | anyNA(as.integer(substr(coords,13,14))) )
      warning("missing values generated in longitude, check for possible stray non-numeric characters", call.=FALSE)
    c1 <- suppressWarnings(as.integer(substr(coords,9,10)))
    c2 <- suppressWarnings(as.integer(substr(coords,11,12))/60)
    c3 <- suppressWarnings(as.integer(substr(coords,13,14))/3600)
    if( max(c2, c3, na.rm=TRUE) > 60 )
      warning("values greater than 60 detected in longitude minutes/seconds, check coordinate format", call.=FALSE)
    result [ , long := ew * (c1 + c2 + c3)]
  } else {
    err <- which(nchar(coords) %in% c(14, 15))
    wmessage <- paste("Unrecognised lat-long format in row", paste(head(which(nchar(coords) < 14)), collapse=' '), '...')
    warning(wmessage, call.=FALSE)
  }   
  result[ , coords := NULL ]

  # check NetLengths
  if( !is.integer(result$netlength) ){
    suppressWarnings(result[ , netlength := as.integer(netlength) ])
    nmiss <- sum(is.na(result$netlength))
    if( nmiss > 0 ){
      wmessage <- paste('non-numeric net lengths detected in', nmiss, 'records')
      warning(wmessage, call.=FALSE)
    }
  }
  nzero <- result[netlength == 0, .N]
  if( nzero > 0 ){
    result[netlength == 0, netlength := NA]
    wmessage <- paste('net length of zero detected in', nzero, 'records; these set to NA')
    warning(wmessage, call.=FALSE)
  }

  # habitats
  result[ , habitat := toupper(habitat)]
  result[habitat %in% c('RD','RE'), habitat := 'RB']

  dodgy <- unique(result$habitat[!(result$habitat %in% c('DS','FA','GN','RB','WD','WS'))])
  if( length(dodgy) > 0 ){
    wmessage <- paste("unrecognised habitat codes:", paste(dodgy, collapse=', '))
    warning(wmessage, call.=FALSE)
  }
  result[ , habitat := as.factor(habitat)]
  
  result[ , scheme := as.factor(scheme)]

  # return dataframe
  result <- as.data.frame(result)
  class(result) <- c('ces', 'data', 'data.frame')
  country <- unique(result$countryID)
  if ( length(country) == 1 )
    attr(result,'country') <- country
  else
    warning("more than one country code detected, country attribute not set", call.=FALSE)
  
  return(result)

}
