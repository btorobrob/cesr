summary.ces.spp <-
function(cesdata, age=0, sp.order='alpha', df=FALSE, nrow=6, silent=TRUE){

  if( !class(cesdata)[1] == 'ces' )
    stop("Please supply a ces data object\n")  #
  
  if( class(cesdata)[2] == 'spp.summary' ){
    
    print(head(cesdata$species[rev(order(cesdata$species$Count)), ], n = nrow))
    return( invisible() )
    
  } else if( !class(cesdata)[2]=='data' )
    stop("Please supply a ces data object\n")
  
  if ( df ){ # produce traditional summary output
    print( summary.data.frame(cesdata) )
    return( invisible() )
  }
  
  x <- data.table::data.table(cesdata)
  lang <- getOption('ceslang')
  selage <- age # to avoid confusing data.table[]

  if ( selage==3 | selage==4 ){
    x <- x[age %in% selage] 
    if ( age == 3 ) agestr='juvenile' 
      else agestr='adult'
  } else
    agestr <- 'all'
 
  # some simple totals
  data.table::setkeyv(x, c('ring', 'species', 'age'))
  total <- x[ , .N]
  birds <- uniqueN(x, by='ring')
  spp <- uniqueN(x, by='species')
  sites <- uniqueN(x, by='site')

  # get data and name spp
  cesdata <- unique(x[ , .(species, ring)])
  sppnames <- cesnames[ , c(1, which(colnames(cesnames)==lang)) ]
  colnames(sppnames) <- c('species', 'name')
  # some housekeeping
  sppnames$species <- as.integer(as.character(sppnames$species))
  sppnames$name <- as.character(sppnames$name)
  cesdata$species <- as.integer(as.character(cesdata$species))
  # now do the merge
  cesdata <- merge(cesdata, sppnames, by='species', all.x=TRUE)
  # substitute EURING number if no spp name
  cesdata$name[is.na(cesdata$name)] <- cesdata$species[is.na(cesdata$name)]
  # summarise by spp and sort
  species <- unique(cesdata[ , count:=.N, by=name, mult='first'][ , .(species, name, count)])
  if ( sp.order == 'count' ){
    species <- species[order(-count),]
  } else if ( sp.order == 'alpha' ){
    species <- species[order(name)] # already sorted alphabetically
  } else 
    species <- species[order(species)]   # sort taxonomically by default
  # these just make the output slightly prettier
  if ( agestr=='all' ) 
    as <-'individual' 
  else 
    as <- agestr  
  data.table::setnames(species, c('Euring Code', 'Species','Count'))
  cat(sprintf("%i captures from %i %s birds of %i species at %i sites\n\n", total, birds, as, spp, sites))
  if( !silent ) # in case one just wants to get the table
    print(as.data.frame(species), row.names = FALSE)
  
  res <- list(captures = total, 
              birds = birds, 
              sites = sites, 
              species = species)
  class(res) <- c('ces', 'spp.summary')
  
  invisible(res)
}

