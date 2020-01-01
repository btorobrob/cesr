summary.ces <-
function(x, age=0, sp.order='taxon', ads=FALSE, jvs=FALSE, prod=FALSE, df=FALSE, silent=FALSE){

  if( !class(x)[1] == 'ces' )
    summary(x)
  else {
    switch( class(x)[2],
            counts = summary.ces.counts(x),
            plots = summary.ces.plots(x),
            sites = summary.ces.sites(x),
            data = summary.ces.spp(x, age=age, sp.order=sp.order, df=df, silent=silent),
            glmfit = summary.ces.glmfit(x, ads=ads, jvs=jvs, prod=prod),
            markfit = summary.ces.markfit(x),
            ch = summary.ces.ch(x),
            cat('No data to summarise!\n'))
  }
}

