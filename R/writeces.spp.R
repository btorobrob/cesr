writeces.spp <-
function(x, file="", age=0){

  if( !class(x)[1]=='ces' )
    stop("Please supply a CES object\n")     
  if( file == "" ) 
    file <- file.choose()
  lang<-getOption('ceslang')
  if( age==3 | age==4 )
    x$data <- x$data[x$age==age, ] 

 # some simple totals
 total <- length(x$ring)
 birds <- length(unique(x$ring))
 sites <- length(unique(x$site))

 # get data and name spp
 cesdata <- unique(cbind(x$spp, x$ring, x$year))
 colnames(cesdata) <- c('spp','ring','year')
 cesnames <- cesnames[ , c(1,which(colnames(cesnames)==lang)) ]
 cesdata <- merge(cesdata, cesnames, all.x=TRUE)
 colnames(cesdata) <- c('spp', 'ring', 'year', 'species')
 # substitute EURING number if no spp name
 cesdata[ , 4] <- as.character(cesdata[ , 4])
 cesdata[is.na(cesdata[ , 4]), 4] <- cesdata$spp[is.na(cesdata[ , 4])]
  
 capture.output(cat(total, 'captures of', birds, 'birds at', sites, 'sites\n\n'), file=file)
 capture.output(xtabs(~species+year,data=cesdata), file=file, append=TRUE)

}

