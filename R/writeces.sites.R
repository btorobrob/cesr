writeces.sites <-
function(x, file, na.string='.'){
    
  if ( !class(x)[1]=='ces' )
    stop("Please supply a CES object\n")     
  
  if( file == "" ) 
    file <- file.choose()

  write.table(x$sites, file=file, sep=',', row.names=FALSE, na=na.string)

}

