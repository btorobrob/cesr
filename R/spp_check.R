spp_check <- function(cesdata){
  
  cesdata$ring <- paste(cesdata$scheme, cesdata$ring, sep="_")
  cesdata$ring_spp <- paste(cesdata$ring, cesdata$species, sep="_")
  # remove multiple captures w/in species
  cesdata <- cesdata[!duplicated(cesdata$ring_spp), ] 
  # get the number of ring occurrences  
  tt <- as.data.frame(xtabs(~ring, data=cesdata))
  # and select out the multiples
  tt <- droplevels(tt[tt$Freq>1, ])
  if (nrow(tt) == 0 )
    return(NULL)
  else   # pull back which species these have been ringed as
    for( i in 1:nrow(tt) )
      tt$species[i] <- paste(cesdata$species[cesdata$ring==tt$ring[i]], collapse=",")
  
  tt <- tt[ , c('ring', 'species')]
  tt$ring <- substr(tt$ring, 5, 20)
  row.names(tt) <- NULL
  
  return(tt)
  
}