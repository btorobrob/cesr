summary.ces <-
function(cesobj, age=0, sp.order='taxon', ads=FALSE, jvs=FALSE, prod=FALSE, df=FALSE, silent=FALSE){

  if( !class(cesobj)[1] == 'ces' )
    summary(cesobj)
  else {
    switch( class(cesobj)[2],
            counts = summary.ces.counts(cesobj),
            plots = summary.ces.plots(cesobj),
            sites = summary.ces.sites(cesobj),
            data = summary.ces.spp(cesobj, age=age, sp.order=sp.order, df=df, silent=silent),
            glmfit = summary.ces.glmfit(cesobj, ads=ads, jvs=jvs, prod=prod),
            markfit = summary.ces.markfit(cesobj),
            ch = summary.ces.ch(cesobj),
            res.table = summary.ces.table(cesobj),
            cat('No data to summarise!\n'))
  }
}

