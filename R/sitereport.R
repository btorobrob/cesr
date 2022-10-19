sitereport <- 
function(cesdata, sitelist=NULL, year=0, template_file=NULL, outdir=getwd(), no.species=12, nyears=NA, verbose=TRUE){

  if( is.null(template_file) )
    template_file <- paste0(path.package('cesr'), '/exec/CES-template.Rmd')
  if( !file.exists(template_file) ){
    warning('template file not found!', call. = FALSE)
    template_file <- file.choose()
  }

  thisyr <- ifelse(year < 1, (as.integer(substr(date(), 21, 24)) - 1), year) 
  # assume doing report for last year in the dataset
  if( sum(unique(cesdata$year) %in% thisyr) == 0 ){
    maxyr <- max(cesdata$year)
    warning(paste('No data for', thisyr, 'available, report will cover to', maxyr), call.=FALSE)
    thisyr <- maxyr
  }
    
  message('extracting data for ', thisyr)

  cesdata <- data.table::data.table(cesdata) # for quicker summarising

  if( is.null(sitelist) | sitelist[1] == 'all' )
    sitelist <- unique(subset(cesdata, year == thisyr, select=sitename))[ , 1] # subset returns a df
  
  DT <- data.table(cesdata, key = 'sitename') # get netlength per site
  DT <- DT[ , head(.SD, 1), by = key(DT)]
  netL <- DT[sitename %in% sitelist, .(sitename, netlength)] 
  
  # create summary datasets for all sites 
  yrdata <- siteplot(data = cesdata, effort=TRUE, plot=FALSE)
  sppjdatay <- siteplot(data = cesdata, age=3, year = thisyr, effort=TRUE, plot=FALSE)
  sppadatay <- siteplot(data = cesdata, age=4, year = thisyr, effort=TRUE, plot=FALSE)
  sppjdatav <- siteplot(data = cesdata, age=3, year = thisyr, visit=TRUE, effort=TRUE, plot=FALSE)
  sppadatav <- siteplot(data = cesdata, age=4, year = thisyr, visit=TRUE, effort=TRUE, plot=FALSE)
  
  for( i in 1:length(sitelist) ){
    
    thissite <- netL$sitename[i]
    netlen <- netL$netlength[i]
    
    if( cesdata[(sitename==thissite & year==thisyr), .N] == 0 ){
      message('no data for site ', thissite, ' skipped')
      next
    }
    if( is.na(netlen) ){
      message('no netlength for site ', thissite, ' skipped')
      next
    }

    # get species list
    spp_count <- cesdata[(sitename==thissite & year==thisyr), .N, by=species]
    spp_count <- spp_count[rev(order(spp_count$N)), ]
    nspecies <- min(no.species, nrow(spp_count))   # try not to generate a load of NA's
    splist.site <- as.character(spp_count$species[1:nspecies])
    splist.site <- splist.site[!is.na(splist.site)]
 
    output_file <- paste0('ces_', thissite, '.doc')
    rmarkdown::render(input = template_file, output_file = output_file, output_dir = outdir, quiet = TRUE)
    if( verbose ) 
      message('report for site ', thissite, ' written')
    
  }   

}




