# check for duplicate rows on a day - is this a merge problem
readces <-
function(file=NULL, visits='std', group.race=TRUE, fix=FALSE, winter=FALSE, verbose=FALSE){
  
  # create a blank vector for collecting dodgy records, set here so we know it exists
  rows2corr <- numeric()
  report.cols <- c("RowNo", "ring", "species", "sitename", "age_in", "sex_in", "day", "month", "year")
  warning.flag <- 0 # if any warnings arise (probable) suggest setting verbose to TRUE

  if( is.null(file) )
    file <- file.choose()
  
  if( !file.exists(file) )
    stop(paste("cannot find the file:", file, "please check typing and that are you in the right directory"))

  # get column names and work out how many
  coln <- strsplit(readLines(file, n=1), '[,;]')[[1]]
  
  # first check whether this is already an output of readces()
  final.names <- c("countryID", "sitename", "site", "lat", "long", "habitat",
                   "netlength", "visit", "julian", "day", "month", "year",
                   "StartTime", "EndTime", "scheme", "ring", "species", "age",
                   "sex", "race", "wing", "weight", "p3", "brood", "moult", 
                   "fat", "weighTime")
  if( sum(coln%in%final.names) == length(coln) ){
    # names match, so hopefully already in the right format
    message("Reading in a formatted CES data file")
    result <- suppressWarnings(data.table::fread(file))
    result <- as.data.frame(result)
    class(result) <- c('ces', 'data', 'data.frame')
    return(result)
  }

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
                 'latitude', 'longitude', 'lon', 'visitno', 'cessite')
  # map back to the main list
  column_nos <- c(1:25, 1, 2, 2, 3, 3, 5, 10, 11, 9, 12, 13, 17, 19, 20, 20, 21,
                  22, 23, 18, 4, 10, 11, 24, 25, 25, 5, 2)
  
  match.names <- adist(tolower(coln), tolower(c(var.names, alt.names)))
  which.names <- unlist(apply(match.names, 1, which.min))
  col.numbers <- column_nos[which.names]
  col.names <- var.names[col.numbers]
  
  result <- suppressWarnings(data.table::fread(file))
  n.read <- nrow(result)
  # use suppressWarnings to avoid messages about bumping col classes late in the data
  setnames(result, col.names)

  # duplicates indicate something is amiss
  duplicated.cols <- which(duplicated(col.names))
  if( any(duplicated.cols) == TRUE ){ 
    wmessage <- paste('Unrecognised columns', paste(coln[duplicated.cols], collapse=', '), 'removed')
    warning(wmessage, call.=FALSE, immediate.=TRUE)
    result <- subset(result, select=col.names[-duplicated.cols])
  }

  result <- result[ , RowNo:=row.names(.SD)]
  setcolorder(result, "RowNo")
  
  # combine scheme and ring number to ensure unique identifiers
  result[ , ring := paste0(result$scheme, "_", gsub('[.]','',result$ring))]

  # Check for unknown species
  unknown_spp <- nrow(result[species==0 | species==99999, ])
  if( unknown_spp > 0 ){
    result <- result[!(species==0 | species==99999), ]
    wmessage <- paste(unknown_spp, 'records with no species ("0", "99999") have been removed')
    warning(wmessage, call.=FALSE, immediate.=TRUE)
  }
  
  # remove races if required
  result[ , race := species]  # just so we know it is there
  if( group.race ){
    result[ , species := (10 * floor(as.numeric(as.character(species))/10))]  # concatenate races, original code now in race
    wmessage <- paste("Subspecies have been grouped in the 'species' column, use the 'race' column,",
                      "or group.race=FALSE if this is not desired")
    message(wmessage)
  }
  
  # check species are valid Euring codes
  int_spp <- suppressWarnings(as.integer(as.character(result$species)))  # force them to be integer
  char_spp <- unique(as.character(result$species[is.na(int_spp)])) # select the rejected records
  if( length(char_spp) > 0 ){
    if( verbose )
      rows2corr <- c(rows2corr, result$RowNo[result$species %in% char_spp])
    wmessage <- paste('Non-numeric species codes:', paste(char_spp,collapse=', '), 'will be deleted')
    warning(wmessage, call.=FALSE, immediate.=TRUE)
    warning.flag <- 1
    result <- result[!is.na(int_spp), ]
  }
  # now check if they are likely CES species
  dodgy <- unique(int_spp[!(int_spp %in% cesnames$spp) & !is.na(int_spp)])
  if( length(dodgy) > 0 ){
    wmessage <- paste('Unrecognised species codes encountered:', paste(dodgy,collapse=', '), ', consider checking them')
    message(wmessage)
  }
  result[ , species := as.factor(species)]
  
  # check that ages are all 3/4 for simplicity later on...
  result[ , age_in := age] # keep a copy of the original for error reports
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
      wmessage <- paste('Age-codes:', paste(ages, collapse=','), 'have been recoded as 4 or deleted')
      message(wmessage)
    }
  }
  result[ , age := as.integer(age)]
  # check for juveniles not in first year of ringing
  dodgy <- merge(result[ , min(year), by=ring], result[age==3, max(year), by=ring], by='ring')
  names(dodgy) <- c('ring', 'min', 'minj')
  result <- merge(result, dodgy, by='ring', all.x=TRUE)
  dodgy <- dodgy[minj > min, ]
  if( nrow(dodgy) > 0 ){
    rings <- unique(dodgy$ring)
    if (verbose )
      rows2corr <- c(rows2corr, result$RowNo[result$ring %in% rings])
    if ( fix ){
      result[(age==3 & year>min), age := 4]
      wmessage <- paste(nrow(dodgy), "age records of impossible 3's corrected")
      warning(wmessage, call.=FALSE, immediate.=TRUE)
    } else {
      wmessage <- paste(nrow(dodgy), 'individuals aged 3 after first year of ringing')
      warning(wmessage, call.=FALSE, immediate.=TRUE)
    }
    warning.flag <- 1
  }
  result[ , ':='(min=NULL, minj=NULL)] # no longer needed
  
  # tidy up sex
  result[ , sex_in := sex] # keep a copy of the original for error reports
  result[sex %in% c('1','3','5','8','m','M'), sex := 'M'] 
  result[sex %in% c('2','4','6','9','f','F'), sex := 'F'] 
  result[sex %in% c('0','7','-','U'), sex := "-"] # just for the record 
  result[!(sex %in% c('F', 'M')), sex:= "-"] # pick up any other entries
  
  sexes <- setDT(result)[sex%in%c('F','M'), .N, by=.(sex,ring)][order(-N), .(sex_t=sex[1L]), keyby=ring]
  result <- merge(result, sexes, by='ring', all.x=TRUE)
  nch1 <- sum(result$sex != result$sex_t, na.rm = TRUE)
  nch2 <- sum(result$sex[result$sex%in%c("F","M")] != result$sex_t[result$sex%in%c("F","M")], na.rm = TRUE)
  if( nch2 > 0 )
    warning.flag <- 1
  if( nch2 > 0 & verbose ){
    dodgy <- unique(result[sex!=sex_t & sex_t!="-" & sex!="-", ][ , ring])
    # this construction saves having to use result$ on every variable - neat!
    wmessage <- paste(nch2, 'records changed sex')
    warning(wmessage, call.=FALSE, immediate.=TRUE)
    rows2corr <- c(rows2corr, result[ring %in% dodgy][ , RowNo])
  }
  if( nch1 > 0 & fix ){
    # fill in M/F (according to which is most commonly recorded)
    wmessage <- paste(nch1, 'sexes fixed, of which', nch2, 'records changed sex')
    warning(wmessage, call.=FALSE, immediate.=TRUE)
    result$sex[!is.na(result$sex_t)] <- result$sex_t[!is.na(result$sex_t)]
  }
  result[ , sex := as.factor(sex)]
  result[ , sex_t := NULL]    
  
  # set site names
  if( length(grep("[#]", as.character(result$siteID))) > 0 ){ # yes, really - thanks to Arizaga
    wmessage <- "Using the '#' sign in sitenames confuses Mark, rename your sites!"
    warning(wmessage, call.=FALSE, immediate.=TRUE)
  }
  result[ , sitename := as.factor(siteID)] 
  result[ , site := as.numeric(as.factor(siteID))]
  result[ , siteID := NULL] # no longer needed
  
  # check for duplicated rings on different species (yes, really!)
  dup_spp <- unique(result[ , c('species','ring')], by=c('species', 'ring')) 
  dup_ndx <- duplicated(dup_spp, by=c('ring'))
  n.duplicated <- sum(dup_ndx)
  if( n.duplicated > 0 ){
    if( verbose )
      rows2corr <- c(rows2corr, result[ring %in% unique(dup_spp$ring[dup_ndx]), ][ , RowNo])
    wmessage <- paste(n.duplicated, 'ring numbers are associated with more than one species code')
    warning(wmessage, call.=FALSE, immediate.=TRUE)
  }
  
  # check visits
  result[ , visit := as.character(visit)]
  na.visit <- sum(is.na(result$visit))
  if( na.visit > 0 ){
    wmessage <- paste(na.visit, "records without a visit number deleted")
    warning(wmessage, call.=FALSE, immediate.=TRUE)
    result <- result[!is.na(visit)]
  }
  if( length(visits) == 1 & visits == 'std' ){
    stdv <- c ('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13')
    n.extra <- length(result$visit[!result$visit%in%stdv])
    if( n.extra > 0 ){
      wmessage <- paste(n.extra, ifelse(n.extra==1, "encounter", "encounters"), "on non-standard visits removed")
      message(wmessage)
    }   
    result <- result[visit %in% stdv]
    result[ , visit := as.integer(visit)]
  } else {
    if ( length(visits) > 1 )
      result <- result[visit %in% visits, ]
    if( any(is.na(suppressWarnings(as.integer(result$visit)))) ){
      wmessage <- "visit will be treated as character"
      message(wmessage)
    } else 
      result[ , visit := as.integer(visit)]
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
    warning(wmessage, call.=FALSE, immediate.=TRUE)
  }
  inv.days <- nrow(result[day < 1 | day > 31])
  if( inv.days > 0 ){
    if( verbose )
      rows2corr <- c(rows2corr, result[day < 1 | day > 31, ][ , RowNo])
    wmessage <- paste(inv.days, 'day values outside the range 1-31 detected')
    warning(wmessage, call.=FALSE, immediate.=TRUE)
    warning.flag <- 1
  }
  # are dates in the (typical) CES period?
  if( !winter ){
    non.summer <- nrow(result[month < 4 | month > 9])
    if( non.summer > 0 ){
      if( verbose )
        rows2corr <- c(rows2corr, result[month < 4 | month > 9, ][ , RowNo])
      wmessage <- paste(non.summer, ifelse(non.summer==1,'record','records'), 'outside the period April to September, is this expected?')
      warning(wmessage, call.=FALSE, immediate.=TRUE)
      warning.flag <- 1
    }
  }
  # now create a Julian day column for doing phenology things
  # create jday first to avoid an error about using $ with atomic vectors
  jday <- function(d, m, y){ strptime(paste0(m, '/', d, '/', y), format="%m/%d/%Y")$yday }
  result[ , julian := jday(day, month, year)]
  # and reset days if a winter CES
  if( winter ){ # set start dates to be July and change year Julian date accordingly
    if( month < 7 ){
      result[ , year := year-1]
      result[ , julian := julian + 184]
    }  
    else
      result[ , julian := julian - 181]
    # are dates in the (typical) winter CES period?
    non.winter <- nrow(result[month > 3 & month < 10])
    if( non.summer > 0 ){
      if( verbose )
        rows2corr <- c(rows2corr, result[month > 3 & month < 10, ][ , RowNo])
      wmessage <- paste(non.winter, ifelse(non.winter==1,'record','records'), 'outside the period October to March, is this expected?')
      warning(wmessage, call.=FALSE, immediate.=TRUE)
      warning.flag <- 1
    }
  }

  # convert co-ordinates if necessary
  if( any(colnames(result)=="coords") ){

    if( !is.character(result$coords) ){
      wmessage <- paste0('Coordinates not in Euring format "+ddmmss', quote("\uB1"), 'dddmmss"')
      warning(wmessage, call.=FALSE, immediate.=TRUE)
    }
    coords <- result$coords
    # first check to see whether they are the right length (14, 15 characters)
    llfmt <- mean(nchar(coords), na.rm=TRUE) 
    if( !llfmt %in% c(14, 15) ){
      err <- which(!nchar(coords) %in% c(14, 15))
      if( length(err) == 0 )
        wmessage <- "Check coordinates field - a mix of lengths detected?"
      else
        wmessage <- paste("Unrecognised coordinate format in sites:", paste(unique(result$sitename[err]), collapse=', '))
      warning(wmessage, call.=FALSE, immediate.=TRUE)
      llfmt <- round(llfmt, 0)
    }
    
    # first check for decimal degrees
    if( any(as.numeric(substr(coords,4,4)) > 5) ){ # i.e. there are minutes > 50
      wmessage <- 'Reading in coordinates as decimal degrees'
      message(wmessage)

      result [ , lat := as.integer(substr(coords,1,7))/10000]
      result [ , long := as.integer(substr(coords,8,20))/10000]

    # no? then in Euring ddmmss format
    } else {
      
      # get the latitudes
      c1 <- suppressWarnings(as.integer(substr(coords,1,3))) # avoid cryptic warning about coerced NAs
      c2 <- suppressWarnings(as.integer(substr(coords,4,5))/60)
      c3 <- suppressWarnings(as.integer(substr(coords,6,7))/3600)
      if( anyNA(c(c1, c2, c3)) ){
        wmessage <- "missing values generated for latitude, are all the coordinates 15 characters long?"
        warning(wmessage, call.=FALSE, immediate.=TRUE)
      }
      if( max(c2, c3, na.rm=TRUE) > 60 ){
        wmessage <- "values greater than 60 detected in latitude minutes/seconds, check coordinate format"
        warning(wmessage, call.=FALSE, immediate.=TRUE)
      }
      result[ , lat := c1 + c2 + c3]
      
      # now the longitudes
      ew <- ifelse(substr(coords,8,8) == '-', -1 , 1) # hemisphere
      if ( llfmt == 15 ){   # Euring code specifies 15 chars, but just in case long deg is 2 digits rather than 3
        if( anyNA(as.integer(substr(coords,9,11))) | 
            anyNA(as.integer(substr(coords,12,13))) | 
            anyNA(as.integer(substr(coords,14,15))) ){
          wmessage <- "missing values generated in longitude, check for stray non-numeric characters"        
          warning(wmessage, call.=FALSE, immediate.=TRUE)
        }
        c1 <- suppressWarnings(as.integer(substr(coords,9,11)))
        c2 <- suppressWarnings(as.integer(substr(coords,12,13)))/60
        c3 <- suppressWarnings(as.integer(substr(coords,14,15)))/3600
        if( max(c2, c3, na.rm=TRUE) > 59 ){
          wmessage <- "values greater than 59 detected in longtitude minutes/seconds, check coordinate format"
          warning(wmessage, call.=FALSE, immediate.=TRUE)
        }
        result [ , long := ew * (c1 + c2 + c3)]
      } else if ( llfmt == 14 ){
        if( anyNA(as.integer(substr(coords,9,10))) | 
            anyNA(as.integer(substr(coords,11,12))) | 
            anyNA(as.integer(substr(coords,13,14))) ){
          wmessage <- "missing values generated in longitude, check for possible stray non-numeric characters" 
          warning(wmessage, call.=FALSE, immediate.=TRUE)
        }
        c1 <- suppressWarnings(as.integer(substr(coords,9,10)))
        c2 <- suppressWarnings(as.integer(substr(coords,11,12))/60)
        c3 <- suppressWarnings(as.integer(substr(coords,13,14))/3600)
        if( max(c2, c3, na.rm=TRUE) > 59 ){
          wmessage <- "values greater than 59 detected in longitude minutes/seconds, check coordinate format"
          warning(wmessage, call.=FALSE, immediate.=TRUE)
        }
        result [ , long := ew * (c1 + c2 + c3)]
      } else {
        result [ , long := NA]
      }   
    } # end of the Euring format block

    result[ , coords := NULL ] # tidy-up
  } # end of reading in the coordinates 
  
  # now do a final check and tidy
  if( sum(colnames(result) %in% c("lat","long")) != 2 ) {
    wmessage <- paste('Coordinates not recognised, check you have either a coords or lat & long columns')
    warning(wmessage, call.=FALSE, immediate.=TRUE)
  } else {
    # do the rounding first to minimise effects of very small differences
    roundc <- function(x) floor(1000*x)/1000 # so consistently bottom-left
    result[ , c('lat','long') := lapply(.SD,roundc), .SDcols=c('lat','long')]

    # check that sites have only one set of coordinates
    check.sites <- table(unique(result[ , c('sitename', 'lat', 'long')])$sitename)
    if( length(table(check.sites)) > 1 ){
      dodgy <- dimnames(check.sites)[[1]][check.sites > 1]
      wmessage <- paste(length(dodgy), "site(s) have multiple coordinates")
      message(wmessage)
      coordacc <- function(x) max(abs(max(x)-min(x)))
      result[ , c('lata','lona') := lapply(.SD,coordacc), .SDcols=c('lat','long'), by=list(sitename)]
      dodgy <- result[!duplicated(result$sitename), c('sitename','lata','lona')]
      dodgy <- dodgy$sitename[dodgy$lata > 0.1 | dodgy$lona > 0.1]
      wmessage <- paste("sites:", paste(dodgy, collapse=", "), "have coordinates >10km apart")
      warning(wmessage, call.=FALSE, immediate.=TRUE)
      result[ , ':=' (lata=NULL, lona=NULL) ]
      
      if( fix ){ # averages to give one coordinate per site 
        result[ , c('lat','long') := lapply(.SD,mean), .SDcols=c('lat','long'), by=list(sitename)]
        result[ , c('lat','long') := lapply(.SD,roundc), .SDcols=c('lat','long')]
      }
    }
  }
  # check NetLengths
  if( !is.integer(result$netlength) ){
    netl <- unique(result$netlength)
    suppressWarnings(result[ , netlength := as.integer(netlength) ])
    nmiss <- sum(is.na(result$netlength))
    if( nmiss > 0 ){
      netl <- netl[is.na(suppressWarnings(as.integer(netl)))]
      wmessage <- paste0('non-numeric net lengths (', paste(sprintf("'%s'", netl), collapse=","), ') detected in ', nmiss, ' records')
      warning(wmessage, call.=FALSE, immediate.=TRUE)
    }
  }
  nzero <- result[netlength == 0, .N]
  if( nzero > 0 ){
    result[netlength == 0, netlength := NA]
    wmessage <- paste('net length of zero detected in', nzero, ifelse(nzero==1,'record','records'), '; these set to NA')
    message(wmessage)
  }
  count.lengths <- result[ , .(count=uniqueN(netlength)), by=sitename]
  if( nrow(count.lengths) > 0 ){
    wmessage <- paste('multiple net lengths detected sites:', 
                      paste(count.lengths$sitename[count.lengths$count>0], collapse=','))
    warning(wmessage, call.=FALSE, immediate.=TRUE)
  }
    
  

  # habitats
  result[ , habitat := toupper(habitat)]
  result[habitat %in% c('RD','RE'), habitat := 'RB']

  dodgy <- unique(result$habitat[!(result$habitat %in% c('DS','FA','GN','RB','WD','WS'))])
  if( length(dodgy) > 0 ){
    wmessage <- paste("unrecognised habitat codes:", paste(dodgy, collapse=', '))
    warning(wmessage, call.=FALSE, immediate.=TRUE)
  }
  result[ , habitat := as.factor(habitat)]

  # rudimentary biometric checks for now
  # check for commas rather than decimal points
  if( is.character(result$wing) )
    result$wing <- suppressWarnings(as.numeric(gsub(",", ".", result$wing, fixed=TRUE)))
  if( is.numeric(result$wing) ){
    result[wing == 0, wing := NA]
    too.long <- unique(result$species[result$wing > 150 & !is.na(result$wing)])
    if( length(too.long) > 0 ){
      wmessage <- paste("long (>150mm) wing lengths recorded for species codes:",
                        paste0(too.long[order(too.long)], collapse=", "), "are you sure?")
      warning(wmessage, call.=FALSE, immediate.=TRUE)
    }
    too.short <- unique(result$species[result$wing < 45 & !is.na(result$wing)])
    if( length(too.short) > 0 ){
      wmessage <- paste("short (<45mm) wing lengths recorded for species codes:",
                        paste0(too.short[order(too.short)], collapse=", "), "are you sure?")
      warning(wmessage, call.=FALSE, immediate.=TRUE)
    }  
  } 
  if( is.character(result$weight) )
    result[ , weight := suppressWarnings(as.numeric(gsub(",", ".", weight, fixed=TRUE)))]
  if( is.numeric(result$weight) ){
    result[weight == 0, weight := NA]
    too.long <- unique(result$species[result$weight > 150 & !is.na(result$weight)])
    if( length(too.long) > 0 ){
      wmessage <- paste("heavy (>150g) weights recorded for species codes:",
                        paste0(too.long[order(too.long)], collapse=", "), "are you sure?")
      warning(wmessage, call.=FALSE, immediate.=TRUE)
    }
    too.short <- unique(result$species[result$wing < 5 & !is.na(result$wing)])
    if( length(too.short) > 0 ){
      wmessage <- paste("light (<5g) weights recorded for species codes:",
                        paste0(too.short[order(too.short)], collapse=", "), "are you sure?")
      warning(wmessage, call.=FALSE, immediate.=TRUE)
    }  
  }
  if( is.character(result$p3) )
    result$p3 <- suppressWarnings(as.numeric(gsub(",", ".", result$p3, fixed=TRUE)))

  # set country attribute
  country <- unique(result$countryID)
  if ( length(country) > 1 )
    country <- substr(country[1], 1, 2)
  attr(result,'country') <- country
    
  if( warning.flag & !verbose ){
    wmessage <- "Warnings were raised, consider using verbose=TRUE to review these records"
    message(wmessage)
  }
    
  if( verbose & length(rows2corr) > 0 ){
    errfile <- paste0(getwd(), '/', country, '_err.csv')
    wmessage <- paste("Please review records in", sprintf("'%s'", errfile), "for errors")
    warning(wmessage, call.=FALSE, immediate.=TRUE)
    report.data <- result[RowNo %in% unique(rows2corr), ..report.cols]
    setorder(report.data, ring, year, month, day)
    write.csv(report.data, file=errfile, row.names=FALSE, quote=FALSE)
  }
  
  n.out <- nrow(result)
  wmessage <- paste(n.read, "rows were read in,", n.out, "rows were retained")
  message(wmessage)
  
  # tidy up and order nicely
  result[ , ':='(scheme=NULL, age_in=NULL, sex_in=NULL, RowNo=NULL) ] 
  
  col.order=c("countryID", "sitename", "site", "lat", "long", "habitat", "netlength",
              "visit", "julian", "day", "month", "year", "StartTime", "EndTime", 
              "scheme", "ring", "species", "age", "sex", "race",
              "wing", "weight", "p3", "brood", "moult", "fat", "weighTime")
  setcolorder(result, col.order[which(col.order %in% names(result))])
  if( fix )
    setorder(result, sitename, year, visit, species, ring)
  
  result <- as.data.frame(result)
  class(result) <- c('ces', 'data', 'data.frame')
  return(result)

}
