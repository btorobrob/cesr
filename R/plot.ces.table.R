plot.ces.table <-
function(cesobj, graph=NULL, ...){
  
  if( is.null(graph) | !is.numeric(graph) ){
    cat(paste0('Graph should be a number between 1 and ', length(cesobj$results), 
              '; choose from:\n'))
    print(names(cesobj$results))
    invisible()
  } else 
    result <- plot(cesobj$results[[graph]], ...)
  
  invisible(result)

}