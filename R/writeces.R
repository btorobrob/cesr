writeces <-
function(cesobj, file="", na.string='.', age=0, ads=FALSE, jvs=FALSE, prod=FALSE, verbose=TRUE){

  if( !class(cesobj)[1]=='ces' )
    stop('No ces data to write\n')
  
  switch(class(cesobj)[2], 
          counts = writeces.counts(cesobj, file = file, na.string = '.'),
          sites = writeces.sites(cesobj, file = file, na.string = '.'),
          plots = writeces.plots(cesobj, file = file, verbose=verbose), 
          data = writeces.spp(cesobj, file = file, age = age),
          glmfit = writeces.glmfit(cesobj, file = file, ads = ads, jvs = jvs, prod = prod),
 #         markfit = plot.ces.markfit(x),
          cat('No data to write!\n'))

}

