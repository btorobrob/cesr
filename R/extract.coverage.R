extract.coverage <-
function(cesdata, early=NA,late=NA, min.visits=1, all.visits=0, exclude=list(years=NULL, sites=NULL)){

  if( !(class(cesdata)[1]=='ces' & class(cesdata)[2]=='data')  )
    stop("Cannot extract from non-CES data\n")

  cesdata <- data.table::data.table(cesdata, key='year,site,visit')
  if( is.character(cesdata$visit) )
    stop('extracting coverage only works with standard (numbered) visits\n')
  
  if( !is.null(exclude$years) )
    cesdata <- cesdata[!year %in% exclude$years]
  if( !is.null(exclude$sites) )
    cesdata <- cesdata[!sitename %in% exclude$sites]
  if( nrow(cesdata) == 0 )
    stop("no data left after exclusions\n")
  
  # check the visit parameters
  last.visit <- max(cesdata$visit, na.rm=TRUE)
  if( all.visits == 0 ){ # if this is not specified bad things can happen if there are some rogue extra visits 
    all.visits <- last.visit
    warning(paste('all.visits not specified,', last.visit, 'visits found'), call.=FALSE)
  } else if( all.visits > last.visit ){ # are there fewer visits than there are supposed to be?
    all.visits <- last.visit 
    warning(paste('all.visits is larger than highest visit number, it has been changed to', last.visit), call.=FALSE)
  } else if( last.visit > all.visits ){ # are there more visits than there are supposed to be?
    nr <- nrow(cesdata[cesdata$visit > all.visits])
    ns <- length(table(cesdata$site[cesdata$visit > all.visits]))
    ny <- length(table(cesdata$year[cesdata$visit > all.visits]))
    cesdata <- cesdata[!cesdata$visit > all.visits, ] # get rid of them
    wmsg <- paste('extra visits detected:', nr, 'records will be ignored from', ns, 'site(s) and', ny, 'year(s)')
    warning(wmsg, call.=FALSE)
  }

  if( is.na(early[1]) ) # this should effectively mean no selection
    # need to subset to avoid warning when early/late are specified
    early <- c(all.visits, 1)
  if( is.na(late[1]) )
    late <- c(all.visits, 1)
  if( (early[1] < early[2]) | (late[1] < late[2]) ){
    if( early[1] < early[2] )
      early <- rev(early)
    if( late[1] < late[2] )
      late <- rev(late)
    warning('first elements of early/late should be greater than second, they have been reversed', call.=FALSE)
  }
  
  # just keep the necessary variables
  data1 <- cesdata[ , c('visit', 'sitename', 'site', 'year')]

  # identify visits covered, i.e. those where some birds were caught; 
    # Note this will miss a few visits where no birds are caught
  visit.cov <- data1[ , .N, by=.(year, site, visit)]
  data.table::setnames(visit.cov, 'N', 'nbirds')
  data2 <- visit.cov[visit.cov$nbirds>0, ]
  data2[ , sy := paste(site, year, sep='_')]
  data2[ , early := ifelse(visit <= (early[1]), 1, 0)]  
  data2[ , late := ifelse(visit >= (all.visits-late[1]+1), 1, 0)]  
  
  # identify which periods have complete or sufficient visits
  per.ce <- data2[early==1, sum(early), by=.(site, year)]
  per.ce[ , per := 'E']
  per.cl <- data2[late==1, sum(late), by=.(site, year)]
  per.cl[ , per := 'L']  
  per.cov <- rbind(per.ce, per.cl)
  data.table::setnames(per.cov, 'V1', 'nvisits')
  
  # now work out which sessions are included and which are complete
  per.cov$include[per.cov$per=='E'] <- (per.cov$nvisits[per.cov$per=='E'] >= early[2])
  per.cov$include[per.cov$per=='L'] <- (per.cov$nvisits[per.cov$per=='L'] >= late[2])
  per.cov$complete[per.cov$per=='E'] <- (per.cov$nvisits[per.cov$per=='E'] == early[1])
  per.cov$complete[per.cov$per=='L'] <- (per.cov$nvisits[per.cov$per=='L'] == late[1])
  per.cov$sy <- paste(per.cov$site, per.cov$year, sep='_')

  # identify years in which each site covered, i.e. include[E] or include[L] == TRUE
  site.cov <- per.cov[include==TRUE, .N, by=.(site, year, per)]
  data.table::setnames(site.cov, 'N', 'cover')
  site.cov1 <- site.cov[per == 'E', .SD, .SDcols=c('cover'), key=.(site,year)]
  data.table::setnames(site.cov1, 'cover', 'early')
  site.cov2 <- site.cov[per == 'L', .SD, .SDcols=c('cover'), key=.(site,year)]
  data.table::setnames(site.cov2, 'cover', 'late')
  site.cov <- merge(site.cov1, site.cov2, all=TRUE)
  site.cov$early[is.na(site.cov$early)] <- 0
  site.cov$late[is.na(site.cov$late)] <- 0
  
  # now get the total number of visits per season
  numvis <- data2[ , uniqueN(visit), by=.(site, year)] # early and late periods may overlap so can't sum(early, late)
  data.table::setnames(numvis, 'V1', 'nvisits')
  site.cov <- merge(site.cov, numvis, by=c('site','year'), all.x=TRUE) # only want sites that are considered covered
  site.cov[ , sy := paste(site, year, sep='_')]

  # now identify missing visits in years that are covered, i.e. at least one visit made
  # this will miss a few visits where no birds are caught
  # first identify years when covered
  all <- data.table(expand.grid(unique(data1$site), unique(data1$year), unique(data1$visit)))
  data.table::setnames(all, c('site','year','visit'))
  all[ , sy := paste(site, year, sep='_')]
  all <- subset(all, sy %in% unique(per.cov$sy))
  miss.vis <- merge(all, visit.cov, by=c('site','year','visit'), all.x=TRUE)
  miss.vis <- miss.vis[(is.na(nbirds)), -'nbirds'] # nbirds is NA by definition, so drop
  sumtxt <- paste(length(unique(visit.cov$site)), 'sites contributed', 
                  nrow(visit.cov[nbirds>0]), 'visits, with',
                  nrow(miss.vis), 'visits missing\n')
  cat(sumtxt)
  if( nrow(miss.vis) > nrow(visit.cov[nbirds>0])/10 )
    warning('more than 10% of visits are missing, has all.visits been specified correctly?', call.=FALSE)

  cesdata <- as.data.frame(cesdata) # convert back to df to use extract.sites
  class(cesdata) <- c('ces', 'data', 'data.frame')
  sites <- extract.sites(cesdata, exclude$sites) 
  
  result <- list(sites = sites,
                years = data.frame(site.cov),
                coverage = data.frame(per.cov),
                missing.visits = data.frame(miss.vis),
                min.visits = min.visits, 
                all.visits = all.visits
              )
  class(result)<-c('ces', 'plots', 'list')
  result
}

