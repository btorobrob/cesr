spp_check <- function(cesobj){
  
  cesobj$ring_spp <- paste(x$ring, x$species, sep="_")
  # remove multiple captures w/in species
  cesobj <- cesobj[!duplicated(cesobj$ring_spp), ] 
  # get the number of ring occurrences  
  tt <- as.data.frame(xtabs(~ring, data=cesobj))
  # and select out the multiples
  tt <- tt[tt$Freq>1, ]
  # pull back which species these have been ringed as
  for( i in 1:nrow(tt) )
    tt$species[i] <- paste(cesobj$species[cesobj$ring==tt$ring[i]], collapse=",")
  
  tt <- tt[ , c('ring', 'species')]
  row.names(tt) <- NULL
  
  return(tt)
  
}