plot.ces <-
function(x, sites=FALSE, graph='X', N=20, ...){

  if( class(x)[1] != 'ces' )
    stop('Not a CES object!')
  
  switch( class(x)[2], 
          plots = plot.ces.plots(x),
          plot.summary = plot.ces.plots(x),
          counts = plot.ces.counts(x, sites=sites),
          data = plot.ces.data(x, N=as.integer(N)),
          glmfit = plot.ces.glmfit(x, graph=graph, ... ),
          markfit = plot.ces.markfit(x),
          cat('No plot method defined!\n'))

}

