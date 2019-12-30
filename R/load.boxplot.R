load.boxplot <-
function(filename=NULL, name='bxpdata'){
  
    if( is.null(filename) | !file.exists(filename) )
      filename <- file.choose()
    load(filename)
    assign(name, result, pos=1)
    rm(result)

}