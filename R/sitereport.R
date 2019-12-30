sitereport <- 
function(data, sitelist=NULL, year=0, template_file=NULL, no.species=12, outdir=getwd(), nyears=NA, verbose=TRUE){

  if( is.null(template_file) )
    template_file <- paste0(path.package('cesr'), '/exec/CES-template.Rmd')
  if( !file.exists(template_file) ){
    warning('template file not found!', call. = FALSE)
    template_file <- file.choose()
  }

  thisyr <- ifelse(year < 1, (as.integer(substr(date(), 21, 24)) - 1), year) 
  # assume doing report for last year in the dataset
  if( sum(unique(data$year) %in% thisyr) == 0 )
    stop(paste('No data for', thisyr, 'available'))
    
  message('extracting data for ', thisyr)

  Data <- data.table::data.table(data) # for quicker summarising

  if( is.null(sitelist) | sitelist[1] == 'all' )
    sitelist <- unique(subset(data, year == thisyr, select=sitename))[ , 1] # subset returns a df
  
  DT <- data.table(data, key = 'sitename') # get Netlength per site
  DT <- DT[ , head(.SD, 1), by = key(DT)]
  netL <- DT[sitename %in% sitelist, .(sitename, NetLength)] 
  
  # create summary datasets for all sites 
  yrdata <- siteplot(data = data, effort=TRUE, plot=FALSE)
  sppjdatay <- siteplot(data = data, age=3, year = thisyr, effort=TRUE, plot=FALSE)
  sppadatay <- siteplot(data = data, age=4, year = thisyr, effort=TRUE, plot=FALSE)
  sppjdatav <- siteplot(data = data, age=3, year = thisyr, visit=TRUE, effort=TRUE, plot=FALSE)
  sppadatav <- siteplot(data = data, age=4, year = thisyr, visit=TRUE, effort=TRUE, plot=FALSE)
  
  for( i in 1:length(sitelist) ){
    
    thissite <- netL$sitename[i]
    netlen <- netL$NetLength[i]
    
    if( Data[(sitename==thissite & year==thisyr), .N] == 0 ){
      message('no data for site ', thissite, ' skipped')
      next
    }
    if( is.na(netlen) ){
      message('no netlength for site ', thissite, ' skipped')
      next
    }

    # get species list
    spp_count <- Data[(sitename==thissite & year==thisyr), .N, by=species]
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




