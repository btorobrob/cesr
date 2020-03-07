plot.ces <-
function(cesobj, sites=FALSE, graph='X', N=20, sitelist=NULL, col=c('red', 'blue'), ...){

  if( class(cesobj)[1] != 'ces' )
    stop('Not a CES object!')
  
  switch(class(cesobj)[2], 
         plots = plot.ces.plots(cesobj),
         plot.summary = plot.ces.plots(cesobj),
         counts = plot.ces.counts(cesobj, sites=sites),
         data = plot.ces.data(cesobj, N=as.integer(N)),
         glmfit = plot.ces.glmfit(cesobj, graph=graph, ... ),
         markfit = plot.ces.markfit(cesobj),
         ch = plot.ces.ch(cesobj, sitelist=sitelist, col=col, ...),
         res.table = plot.ces.table(cesobj, graph=graph, ...),
         cat('No plot method defined!\n'))
  
}

