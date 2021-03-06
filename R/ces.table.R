# write a plot function!
# return vals for annual and compare? first is change, second is %
#   --- I think actually this useful, document somewhere?
# check for NAs in important columns in readces
# check the se column of the constant model - is this the annual or di,fference se?
# Rethink the column labels?!
## Can we collect objects, at least if they have a common name?

ces.table <-
function(cesobj, species=NA, columns=c("A-0", "P-0", "S-0"), plots=NULL, min.n=100, conf.lim=TRUE, ndigits=2, year=-1, visit.corr=TRUE){
  
  if( class(cesobj)[1]!='ces' | class(cesobj)[2]!='data' )
    stop('not a CES data object')
  
  if( is.na(species[1]) ){
      species <- table(cesobj$species)
      species <- as.numeric(names(species[species > min.n]))
  }
  species <- suppressWarnings(as.numeric(species)) # pick up the NAs next for a more informative message
  if( any(is.na(species)) ){
    warning('non-numeric species codes detected and ignored', call.=FALSE, immediate.=TRUE)
    species <- species[!is.na(species)]
  }
  check.spp <- species %in% cesnames$spp
  if( sum(check.spp) < length(check.spp) ){ # i.e. some entries are FALSE
    dodgy <- paste(species[!check.spp], collapse=',')
    warning(paste('invalid species codes detected:', dodgy), call.=FALSE, immediate.=TRUE)
    species <- species[check.spp]
  }
    
  if( is.null(plots) ){
    warning("no plot data provided, extracting all visits", call.=FALSE, immediate.=TRUE)
    plots <- extract.coverage(cesobj)
  }

  n.spp <- length(species)
  n.col <- length(columns)
  year <- ifelse(year==-1, max(cesobj$year, na.rm=TRUE), year)
    
  # easy to create a lookup table, but is there a more elegant way of doing this?
  idx <- expand.grid(1:n.spp, 1:n.col)
  names(idx) <- c('spp', 'cols')
  idx$counter <- seq(1:nrow(idx))
  
  res <- list() # a list to hold the results
  table.est <- matrix("", ncol=n.col, nrow=n.spp, dimnames=list(species,columns)) # a list to hold the table entries
  
  for( i in 1:length(species) ){
    
    if( sum(grepl('[AJP]', columns)) > 0 ) # need the count data
      spp.data <- extract.data(cesobj, species=species[i], plots=plots)
    if( sum(grepl('S', columns)) > 0 ){ # need the survival data
      spp.mark <- extract.ch(cesobj, species=species[i], plots=plots)
      ilogit <- function(x){ exp(x) / (1+exp(x)) } # a useful function to get the results
    }
    
    for( j in 1:n.col ){
      
      ctr <- idx$counter[idx$spp==i & idx$cols==j] 
      dtype <- substr(columns[j], 1, 1)
      mtype <- substr(columns[j], 2, 2)
      nyear <- as.numeric(substr(columns[j], 3, nchar(columns[j]))) # just in case > 10 so more than 3 chars
      
      if( dtype == 'A' ){ ## Adult Models ----
        if( nyear == 0 ){ # annual model irrespective
          res[[ctr]] <- list(ad.results = ann.model.counts(spp.data$ad.data, offset=visit.corr),
                             model.type = list(type='annual', refyear=year, nyrs=0), limits=0.95,
                             spp = spp.data$spp, spp.name = spp.data$spp.name)
          class(res[[ctr]]) <- c('ces', 'glmfit')
          table.est[i, j] <- get.estimate(res[[ctr]], mtype='annual', cl=conf.lim, ndigits=ndigits) 
        }
        else if( mtype == '-' ){ # a compare model  
          res[[ctr]] <- list(ad.results = annc.model.counts(spp.data$ad.data, compare=nyear, offset=visit.corr),
                             model.type = list(type='constant', refyear=year, nyrs=nyear), limits=0.95,
                             spp = spp.data$spp, spp.name = spp.data$spp.name)
          class(res[[ctr]]) <- c('ces', 'glmfit')
          table.est[i, j] <- get.estimate(res[[ctr]], mtype='constant', cl=conf.lim, ndigits=ndigits) 
        }
        else if( mtype == '/' ){ # a trend model  
          res[[ctr]] <- list(ad.results = annt.model.counts(spp.data$ad.data, trend=nyear, offset=visit.corr),
                             model.type = list(type='trend', refyear=year, nyrs=nyear), limits=0.95,
                             spp = spp.data$spp, spp.name = spp.data$spp.name)
          class(res[[ctr]]) <- c('ces', 'glmfit')
          table.est[i, j] <- get.estimate(res[[ctr]], mtype='trend', cl=conf.lim, ndigits=ndigits) 
        }
        else{
          warning("unrecognised model type: please use '-' or '/' only", call. = FALSE)
          next
        } # End Adult Models
      } else if( dtype == 'J' ){ ## Juvenile Models ----
        if( nyear == 0 ){ # annual model irrespective
          res[[ctr]] <- list(jv.results = ann.model.counts(spp.data$jv.data, offset=visit.corr),
                             model.type = list(type='annual', refyear=year, nyrs=0), limits=0.95,
                             spp = spp.data$spp, spp.name = spp.data$spp.name)
          class(res[[ctr]]) <- c('ces', 'glmfit')
          table.est[i, j] <- get.estimate(res[[ctr]], mtype='annual', cl=conf.lim, ndigits=ndigits) 
        }
        else if( mtype == '-' ){ # a compare model  
          res[[ctr]] <- list(jv.results = annc.model.counts(spp.data$jv.data, compare=nyear, offset=visit.corr),
                             model.type = list(type='constant', refyear=year, nyrs=nyear), limits=0.95,
                             spp = spp.data$spp, spp.name = spp.data$spp.name)
          class(res[[ctr]]) <- c('ces', 'glmfit')
          table.est[i, j] <- get.estimate(res[[ctr]], mtype='constant', cl=conf.lim, ndigits=ndigits) 
        }
        else if( mtype == '/' ){ # a trend model  
          res[[ctr]] <- list(jv.results = annt.model.counts(spp.data$jv.data, trend=nyear, offset=visit.corr),
                             model.type = list(type='trend', refyear=year, nyrs=nyear), limits=0.95,
                             spp = spp.data$spp, spp.name = spp.data$spp.name)
          class(res[[ctr]]) <- c('ces', 'glmfit')
          table.est[i, j] <- get.estimate(res[[ctr]], mtype='trend', cl=conf.lim, ndigits=ndigits) 
        }
        else {
          warning("unrecognised model type: please use '-' or '/' only", call. = FALSE)
          next
        } # End Juvenile Models
      } else if( dtype == 'P' ){ ## Productivity Models ----
        
        pr.data <- list(ad.data=spp.data$ad.data, jv.data=spp.data$jv.data)
        
        if( nyear == 0 ){ # annual model irrespective
          res[[ctr]] <- list(pr.results = ann.model.prod(pr.data, offset=visit.corr),
                             model.type = list(type='annual', refyear=year, nyrs=0), limits=0.95,
                             spp = spp.data$spp, spp.name = spp.data$spp.name)
          class(res[[ctr]]) <- c('ces', 'glmfit')
          table.est[i, j] <- get.estimate(res[[ctr]], mtype='annual', cl=conf.lim, ndigits=ndigits) 
        } else if( mtype == '-' ){ # a compare model  
          res[[ctr]] <- list(pr.results = annc.model.prod(pr.data, compare=nyear, offset=visit.corr),
                             model.type = list(type='constant', refyear=year, nyrs=nyear), limits=0.95,
                             spp = spp.data$spp, spp.name = spp.data$spp.name)
          class(res[[ctr]]) <- c('ces', 'glmfit')
          table.est[i, j] <- get.estimate(res[[ctr]], mtype='constant', cl=conf.lim, ndigits=ndigits) 
        } else if( mtype == '/' ){ # a trend model  
          res[[ctr]] <- list(pr.results = annt.model.prod1(pr.data, trend=nyear, offset=visit.corr),
                             model.type = list(type='trend', refyear=year, nyrs=nyear), limits=0.95,
                             spp = spp.data$spp, spp.name = spp.data$spp.name)
          class(res[[ctr]]) <- c('ces', 'glmfit')
          table.est[i, j] <- get.estimate(res[[ctr]], mtype='trend', cl=conf.lim, ndigits=ndigits) 
        } else {
          warning("unrecognised model type: please use '-' or '/' only", call. = FALSE)
          next
        } # End Productivity Models
      } else if( dtype == 'S' ){ ## Survival Models ----
        if( mtype == '-' ){ # a compare model
          res[[ctr]] <- list(s.results = mark.ces(spp.mark, exclude=NULL, type='+', compare=nyear, cleanup=TRUE),
                             model.type = list(type='compare', refyear=year, nyrs=nyear), limits=0.95,
                             spp = spp.data$spp, spp.name = spp.data$spp.name)
          class(res[[ctr]]) <- c('ces', 'markfit')
          row.est <- res[[ctr]]$s.results$survival[nrow(res[[ctr]]$s.results$survival), ]
          est <- round(ilogit(row.est[2]), ndigits)
          if( conf.lim ){
            lcl <- round(ilogit(row.est[4]), ndigits)
            ucl <- round(ilogit(row.est[5]), ndigits)
            table.est[[i, j]] <- paste0(est, ' (', lcl, ', ', ucl, ')')
          } else
            table.est[[i, j]] <- as.character(est)
        } else if( mtype == '/' ){ # a trend model
          res[[ctr]] <- list(s.results = mark.ces(spp.mark, exclude=NULL, type='+', trend=nyear, cleanup=TRUE),
                             model.type = list(type='trend', refyear=year, nyrs=nyear), limits=0.95,
                             spp = spp.data$spp, spp.name = spp.data$spp.name)
          class(res[[ctr]]) <- c('ces', 'markfit')
          row.est <- res[[ctr]]$s.results$model$results$beta[grep('Phi:Tind:Time', rownames(res[[ctr]]$s.results$model$results$beta)), ]
          est <- round(row.est[1], ndigits)
          if( conf.lim ){
            lcl <- round(row.est[3], ndigits)
            ucl <- round(row.est[4], ndigits)
            table.est[[i, j]] <- paste0(est, ' (', lcl, ', ', ucl, ')')
          } else
            table.est[[i, j]] <- as.character(est)
        }
        else {
          warning("unrecognised model type: please use '-' or '/' only", call. = FALSE)
          next
        }
      } else {
        warning("unrecognised data type: please use 'A', 'J', 'P' or 'S'", call. = FALSE)
        next
      } # End Survival Models
    
      names(res)[ctr] <- paste('T', species[i], columns[j], sep='_')
    } # end columns loop
    
    cat(sprintf("Analysed species: %6.0f \n", species[i]))
  } # end species loop
  
  # get the first character ...
  col.headings <- toupper(as.character(sapply(columns, FUN=function(x) substr(x, 1, 1))))
  # ... and expand
  col.headings <- as.character(sapply(col.headings, FUN=function(x) switch(x, 'A'='Adults', 'J'='Juveniles', 'P'='Productivity', 'S'='Survival')))
  
  return.list <- list(table = table.est,
                      results = res)
  class(return.list) <- c('ces', 'res.table')
  
  print(knitr::kable(table.est, col.names=col.headings, align='r', row.names=TRUE))
  return(return.list)
}
