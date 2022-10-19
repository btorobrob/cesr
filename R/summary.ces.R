summary.ces <-
function(object, age=0, sp.order='alpha', ads=FALSE, jvs=FALSE, prod=FALSE, df=FALSE, nrow=6,silent=FALSE, ...){

  if( !class(object)[1] == 'ces' )
    summary(object)
  else {
    switch( class(object)[2],
            counts = summary.ces.counts(object),
            plots = summary.ces.plots(object),
            sites = summary.ces.sites(object),
            data = summary.ces.spp(object, age=age, sp.order=sp.order, df=df, nrow=nrow, silent=silent),
            glmfit = summary.ces.glmfit(object, ads=ads, jvs=jvs, prod=prod),
            markfit = summary.ces.markfit(object),
            ch = summary.ces.ch(object),
            res.table = summary.ces.table(object),
            cat('No data to summarise!\n'))
  }
}

