extract.data <-
function(data, species=0, plots=NULL){

  if( !(class(data)[1] == 'ces' & class(data)[2] == 'data')  )
    stop("no ces data\n")
  
  if( is.null(plots) )
    stop("no plot data, use extract.coverage() first")

  if( species == 0 ) {
    species <- unique(data$species)
    if( length(species) > 1 ) 
      stop("please supply a Euring species code")
  }
 
  selspp <- as.numeric(species[1])
  
  # temporarily convert for speed
  data <- data.table::data.table(data, key='species')
  data <- as.data.frame(data[species==selspp, .(site, visit, year, ring, age)])
  class(data) <- c("ces", "data", "data.frame") # expected by extract.counts()
  
  if( nrow(data) == 0 )
    stop('no data for species ', species, '\n')

  adcounts <- extract.counts(data, age=4, plots=plots)
  jvcounts <- extract.counts(data, age=3, plots=plots)

  lang <- getOption('ceslang')
  spp.name <- as.character(cesnames[cesnames$spp==species, which(colnames(cesnames)==lang) ])

  result <- list(ad.data = adcounts, 
                 jv.data = jvcounts, 
                 spp = species,
                 spp.name = spp.name) 
  class(result) <- c('ces', 'counts')
  result
  
}

