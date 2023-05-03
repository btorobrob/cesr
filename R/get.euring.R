get.euring <- function(string){
  
  if( !is.character(string) | length(string) > 1 )
    stop("please provide a single search string")
  if( !exists("cesnames") )
    cesnames <- get(data("cesnames"))
  rows <- unlist(apply(cesnames, 2, function(x, string){ grep(tolower(string), tolower(x), fixed=TRUE) }, string))
  return(cesnames[unique(rows), (1:2)])
  
}