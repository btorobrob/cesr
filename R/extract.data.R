extract.data <-
function(cesdata, species=0, plots=NULL, min.yrs=1){

  if( !(class(cesdata)[1] == 'ces' & class(cesdata)[2] == 'data') )
    stop("no ces data\n")
  
  if( is.null(plots) )
    stop("no plot data, use extract.coverage() first")

  if( species == 0 | !is.numeric(species) ) {
    species <- unique(data$species)
    if( length(species) > 1 | !is.numeric(species) ) 
      stop("please supply a valid Euring species code")
  }
  selspp <- as.numeric(species) # data.table seems to get confused if this is the same as a colname
  
  # temporarily convert for speed
  data <- data.table::data.table(cesdata, key='species')
  data <- data[(species==selspp & visit <= plots$all.visits), .(site, visit, year, ring, age)]
  if( min.yrs > 1 ){
    data <- merge(data, plots$sites[ , c('site', 'nyears')], by='site', all.x=TRUE)
    data <- data[nyears >= min.yrs, ]
  }
  data <- as.data.frame(data)
  class(data) <- c("ces", "data", "data.frame") # expected by extract.counts()

  if( nrow(data) == 0 )
    stop('no data for species ', species, '\n')

  adcounts <- extract.counts(data, age=4, plots=plots)
  jvcounts <- extract.counts(data, age=3, plots=plots)

  lang <- getOption('ceslang')
  spp.name <- as.character(cesnames[cesnames$spp==species, which(colnames(cesnames)==lang) ])
  
  njuvs <- ifelse(is.null(jvcounts), 0, sum(jvcounts$totcaps))
  nads <- ifelse(is.null(adcounts), 0, sum(adcounts$totcaps))
  cat(paste("Extracted", njuvs, "juvenile and", nads, "adult captures\n"))
  
  result <- list(ad.data = adcounts, 
                 jv.data = jvcounts, 
                 spp = species,
                 spp.name = spp.name) 
  class(result) <- c('ces', 'counts')
  result
  
}

