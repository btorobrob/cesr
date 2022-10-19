plot.ces.table <-
function(x, graph=NULL, ...){
  
  if( is.null(graph) | !is.numeric(graph) ){
    cat(paste0('Graph should be a number between 1 and ', length(x$results), 
              '; choose from:\n'))
    print(names(x$results))
    invisible()
  } else 
    result <- plot(x$results[[graph]], ...)
  
  invisible(result)

}