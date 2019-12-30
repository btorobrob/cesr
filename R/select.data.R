select.data <-
function(x, sites=0, habitat='') {

  if ( !class(x)[1]=='ces' | !class(x)[2]=='data' )
    stop("Please supply a ces data object\n")     
  
  if ( sites[1] != 0 )
    x <- x[x$site %in% sites, ]
  if ( habitat != '' )
    x <- x[x$habitat == habitat, ]
  
  x
}