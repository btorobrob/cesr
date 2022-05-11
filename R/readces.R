readces <-
function(file=NULL, visits='std', fill.sex=FALSE, group.race=TRUE){
  
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
                 'moult', 'wing', 'weight', 'weighTime', 'p3', 'fat', 'lat', 'long')
  # longer form ones
  alt.names <- c('Country_Identifier', 'Site_Identifier', 'sitename', 'coordinates', 
                 'site_coordinates', 'visit_period', 'visit_start_time', 'visit_end_time',
                 'total_net_length', 'ring_scheme', 'ring_number', 'brood_patch_score', 
                 'wing_length', 'mass', 'body_mass', 'time_of_weighing', 'length_p3', 
                 'fat_score', 'moult_state', 'habitat_type', 'start', 'end',
                 'latitude', 'longitude', 'lon')
  # map back to the main list
  column_nos <- c(1:25, 1, 2, 2, 3, 3, 5, 10, 11, 9, 12, 13, 17, 19, 20, 20, 21,
                  22, 23, 18, 4, 10, 11, 24, 25, 25)
  
  match.names <- adist(tolower(coln), tolower(c(var.names, alt.names)))
  which.names <- unlist(apply(match.names, 1, which.min))
  col.numbers <- column_nos[which.names]
  col.names <- var.names[col.numbers]
  
  result <- suppressWarnings(data.table::fread(file))
  # use suppressWarnings to avoid messages about bumping col classes late in the data
  setnames(result, col.names)

  # duplicates indicate something is amiss
  duplicated.cols <- which(duplicated(col.names))
  if( any(duplicated.cols) == TRUE ){ 
    wmsg <- paste('Unrecognised columns', paste(duplicated.cols, collapse=', '), 'removed')
    warning(wmsg, call. = FALSE)
    result <- subset(result, select=col.names[-duplicated.cols])
  }
  
  # Check for unknown species
  unknown_spp <- sum(result$species==0, na.rm=TRUE)
  if( unknown_spp > 0 ){
    result <- result[species > 0, ]
    wmessage <- paste(unknown_spp, 'unknown species (0) records removed')
    warning(wmessage, call.=FALSE)
  }
  
  # remove races if required
  result[ , race := species]  # just so we know it is there
  if( group.race ){
    result[ , species := (10 * floor(as.numeric(as.character(species))/10))]  # concatenate races, original code now in race
    wmessage <- paste("Subspecies have been grouped in the 'species' column, use the 'race' column',",
                      "or group.race=FALSE if this is not desired")
    warning(wmessage, call.=FALSE)
  }
  
  # check species are valid Euring codes
  result[ , species := as.factor(species)]
  int_spp <- suppressWarnings(as.integer(as.character(result$species)))  # force them to be integer
  char_spp <- unique(as.character(result$species[is.na(int_spp)])) # select the rejected records
  if( length(char_spp) > 0 ){
    wmessage <- paste('Non-numeric species codes:', paste(char_spp,collapse=', '), 'will be ignored')
    warning(wmessage, call.=FALSE)
    result <- result[!is.na(int_spp), ]
  }
  # now check if they are likely CES species
  dodgy <- unique(int_spp[!(int_spp %in% cesnames$spp) & !is.na(int_spp)])
  if( length(dodgy) > 0 ){
    wmessage <- paste('Surprising species codes encountered:', paste(dodgy,collapse=', '), ', consider checking these')
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
      age.warning <- paste('Unexpected age-codes:', paste(ages, collapse=','), 'encountered; recoded as 4 or deleted')
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
  
  # check for duplicated rings on different species (yes, really!)
  dup_spp <- unique(result[ , c('species','ring')], by=c('species', 'ring')) 
  dup_ndx <- duplicated(dup_spp, by=c('ring'))
  if( sum(dup_ndx) > 0 ){
    wmessage <- paste('Rings associated with more than one species code:', paste(dup_spp$ring[dup_ndx],collapse=', '))
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
    wmessage <- paste('Non-numeric dates detected in', count, 'records')
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

  # convert co-ordinates if necessary
  if( any(colnames(result)=="coords") ){

    if( !is.character(result$coords) )
      warning('Coordinates not in Euring format "+ddmmss±dddmmss"', call. = FALSE)

    coords <- result$coords
    # first check to see whether they are the right length (14, 15 characters)
    llfmt <- mean(nchar(coords), na.rm  = TRUE) 
    if( !llfmt %in% c(14, 15) ){
      err <- which(!nchar(coords) %in% c(14, 15))
      if( length(err) == 0 )
        wmessage <- "Check coordinates field - a mix of lengths detected?"
      else
        wmessage <- paste("Unrecognised coordinate format in sites:", paste(unique(result$sitename[err]), collapse=', '))
      warning(wmessage, call.=FALSE)
      llfmt <- round(llfmt, 0)
    }
    
    # first check for decimal degrees
    if( any(as.numeric(substr(coords,4,4)) > 5) ){
      warning('Reading in coordinates as decimal degrees', call. = FALSE)

      result [ , lat := as.integer(substr(coords,1,7))/10000]
      result [ , long := as.integer(substr(coords,8,20))/10000]

    # no? then in Euring ddmmss format
    } else {
      
      # get the latitudes
      c1 <- suppressWarnings(as.integer(substr(coords,1,3))) # avoid cryptic warning about coerced NAs
      c2 <- suppressWarnings(as.integer(substr(coords,4,5))/60)
      c3 <- suppressWarnings(as.integer(substr(coords,6,7))/3600)
      if( anyNA(c(c1, c2, c3)) )
        warning("missing values generated for latitude, are all the coordinates 15 characters long?", call.=FALSE)
      result[ , lat := c1 + c2 + c3]
      
      # now the longitudes
      ew <- ifelse(substr(coords,8,8) == '-', -1 , 1) # hemisphere
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
        result [ , long := NA]
      }   
      result[ , coords := NULL ]
      
    } # end of the Euring format block

  } # end of reading in the coordinates 

  # now do a final check
  if( sum(colnames(result) %in% c("lat","long")) != 2 ) {
    wmessage <- paste('Coordinates not recognised, check you have either a coordinates or lat & long columns')
    warning(wmessage, call.=FALSE)
  }    

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
  
  # check biometrics for commas rather than decimal points
  if( is.character(result$wing) )
    result$wing <- suppressWarnings(as.numeric(gsub(",", ".", result$wing, fixed=TRUE)))
  if( is.character(result$weight) )
    result$weight <- suppressWarnings(as.numeric(gsub(",", ".", result$weight, fixed=TRUE)))
  if( is.character(result$p3) )
    result$p3 <- suppressWarnings(as.numeric(gsub(",", ".", result$p3, fixed=TRUE)))
 
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
