siteplot <- 
function(data, site=0, year=0, visit=FALSE, splist=NULL, nspecies=20, nyears=NA, age=0, subset=NULL, effort=FALSE, ylab=NULL, margin=8, pch=21, pt.cex=2, pt.col='red', plot=TRUE, ...){
  
  if( is.data.frame(data) ){ ## Create bxp data ----
    
    x <- data.table::data.table(data)
    
    if ( !is.null(subset) ) 
      x <- subset(x, eval(parse(text = subset)))
    
    if( age %in% c(3, 4) ){ # filter by age if needed
      selage <- age
      x <- x[age == selage, ]
    }
    
    if( year > 0 ){ # filter by year if needed
      selyr <- year
      x <- x[year == selyr, ]
    }
    
    if( year == 0 ) # summarise for all species across years
      ds <- x[ , .N, by=list(year, sitename)]
    else {
      if( visit == TRUE) # summarise by visit within a year across species
        ds <- x[ , .N, by=list(visit, sitename)]
      else # summarise by species (across visits) within a year
        ds <- x[ , .N, by=list(species, sitename)]
    }
    
    if( effort ) {
      netlen <- x[, mean(NetLength), by = sitename] 
      names(netlen) <- c('sitename', 'netlen')
      ds <- ds
      ds <- merge(ds, netlen, by='sitename', all.x=TRUE)
      ds[ , N := (N / netlen)]
    }
    
    if( year == 0 ) 
      ds.bxp <- boxplot(formula=N~year, data=ds, range=0, plot=FALSE)
    else {
      if( visit == TRUE )
        ds.bxp <- boxplot(formula=N~visit, data=ds, range=0, plot=FALSE)
      else {
        ds.bxp <- boxplot(formula=N~species, data=ds, range=0, plot=FALSE)
        # and get real names      
        ds.bxp$codes <- ds.bxp$names  # need to retain spp code, with labels in names
        lang <- ifelse(is.null(getOption("ceslang")), 'Latin', getOption("ceslang")) 
        spnames <- cesnames[ , c(1, which(colnames(cesnames) == lang))]
        ds.bxp$names <- droplevels(spnames[match(ds.bxp$names, as.character(spnames$spp)), 2])
      }
    }
    
    ds.attr <- list(effort = effort,
                    type = ifelse(year==0, 'year', ifelse(visit==TRUE, 'visit', 'species')))
    
  } # end generate new bxp data
  
  else if( is.list(data) & all.equal(names(data), c("ds","ds.bxp","ds.attr")) ){  ## Get bxp data ----
    
    if( (age > 0) | (!is.null(subset)) )
      warning('summarised data were provided, no further subsetting possible\n', call. = FALSE)
    
    # extract the summarised data
    ds <- data$ds
    ds.attr <- data$ds.attr
    ds.bxp <- data$ds.bxp
    
  }
  
  else
    stop('data is not a recognised object', call. = FALSE)
  
  if( plot ){   ## Plot bxp data ----
    
    if( is.null(ylab) )
      if( ds.attr$effort )
        ylab <- 'No. of birds caught per metre'
      else
        ylab <- 'Number of birds caught'
      
      if( ds.attr$type == 'year' ){ ## plot by year
        
        if( length(ds$sitename[ds$sitename==site]) == 0 )
          stop(paste0('no data for site ', site), call. = FALSE)
        
        years <- ds$year[ds$sitename==site] # the site data to plot
        N <- ds$N[ds$sitename==site]        # do this here so can use years 2 lines down
        
        if( !is.na(nyears) ) { # truncate the boxplot 
          
          num.yrs <- ifelse(length(years) > nyears, length(years), nyears)
          
          col.last <- length(ds.bxp$names)
          col.first <- col.last - num.yrs + 1
          ds.bxp$stats <- ds.bxp$stats[ , (col.first:col.last)]
          ds.bxp$n <- ds.bxp$n[(col.first:col.last)]
          ds.bxp$conf <- ds.bxp$conf[ , (col.first:col.last)]
          ds.bxp$names <- ds.bxp$names[(col.first:col.last)]
          
        }
        
        years <- years - as.integer(ds.bxp$names[1]) + 1
        bxp(ds.bxp, las=1, ylab=ylab, ...)
        points(years, N, pch=pch, cex=pt.cex, bg=pt.col)    
        
      } else if( ds.attr$type == 'species' ) { ## plot by species
        
        if( is.null(splist) ){ # get a splist of length nspecies
          selsite <- site
          spp_count <- ds[sitename==selsite, .(N), by=species]
          spp_count <- spp_count[rev(order(spp_count$N)), ]
          nspecies <- min(nspecies, nrow(spp_count))   # so don't generate a load of NA's
          splist <- as.character(spp_count$species[1:nspecies])
          splist <- splist[!is.na(splist)]
        }
        
        if( is.na(splist[1]) )
          stop(paste0('no data for site ', site, ' in ', year), call. = FALSE)
        
        if( splist[1] != 'all' ){ # subset the elements of ds.bxp
          site.bxp <- ds.bxp
          selrows <- which(ds.bxp$codes %in% splist)
          selrows <- selrows[rev(order(selrows))] # because bxp plots from bottom up
          site.bxp$stats <- ds.bxp$stats[ , selrows]
          site.bxp$n <- ds.bxp$n[selrows]
          site.bxp$conf <- ds.bxp$conf[ , selrows]
          site.bxp$names <- droplevels(ds.bxp$names[selrows])
          site.bxp$codes <- ds.bxp$codes[selrows]
          if( length(selrows) == 1 ){
            site.bxp$stats <- rbind(site.bxp$stats, 0)
            site.bxp$n <- c(site.bxp$n, 0)
            site.bxp$conf <- rbind(site.bxp$conf, 0)
            site.bxp$names <- c(site.bxp$names, NA)
            site.bxp$codes <- c(site.bxp$codes, NA)
            warning(paste('only one species for site', site, '\n'), call. = FALSE)
          }
        }
        
        # and plot
        oldmar <- par('mar')
        if( length(margin) == 4 )
          par(mar = margin )
        else 
          par(mar = c(par('mar')[1], margin[1], par('mar')[3:4]))
        
        bxp(site.bxp, las=1, xlab=ylab, horizontal=TRUE, ...)
        counts <- ds$N[ds$sitename==site]
        spp <- match(as.character(ds$species[ds$sitename==site]), site.bxp$codes)
        points(counts[!is.na(spp)], as.integer(na.omit(spp)), pch=pch, cex=pt.cex, bg=pt.col)  
        #      if( legend ) # what did I mean here?
        #        text()
        par(mar = oldmar)
        
      } else if( ds.attr$type == 'visit' ) { ## plot by visit
        
        oldmar <- par('mar')
        if( length(margin) == 4 )
          par(mar = margin )
        else 
          par(mar = c(par('mar')[1], margin[1], par('mar')[3:4]))

        if( length(ds$sitename[ds$sitename==site]) == 0 )
          stop(paste0('no data for site ', site), call. = FALSE)
        
        visits <- ds$visit[ds$sitename==site] # the site data to plot
        N <- ds$N[ds$sitename==site]
        
        bxp(ds.bxp, las=1, ylab=ylab, ...)
        points(visits, N, pch=pch, cex=pt.cex, bg=pt.col)    
        
      }
  }
  
  invisible(list(ds = ds,
                 ds.bxp = ds.bxp,
                 ds.attr = ds.attr))
}