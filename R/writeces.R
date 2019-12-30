writeces <-
function(x, file="", na.string='.', age=0, ads=FALSE, jvs=FALSE, prod=FALSE, verbose=TRUE){

  if( !class(x)[1]=='ces' )
    stop('No ces data to write\n')
  
  if ( class(x)[2] == 'counts' ){
      writeces.counts(x, file, na.string='.')
    } else if ( class(x)[2] == 'sites' ){
      writeces.sites(x, file, na.string='.')
    } else if ( class(x)[2] == 'plots' ){
      writeces.plots(x, file, verbose)
    } else if ( class(x)[2] == 'data' ){
      writeces.spp(x, file, age)
    } else if ( class(x)[2] == 'glmfit' ){
      writeces.glmfit(x, file, ads=FALSE, jvs=FALSE, prod=FALSE)
    } else 
      cat('No data to write!\n')

}

