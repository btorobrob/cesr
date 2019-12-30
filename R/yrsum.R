yrsum <- 
function(data, var){
  
  ## An internal function to extract annual means and quartiles from a CES data object
  x <- data[ , .N, by=c(var, 'year')]
  x.1 <- x[ , mean(N, na.rm=TRUE), by=var]
  names(x.1) <- c(var, 'mean')
  x.2 <- x[ , quantile(N, 0.25, na.rm=TRUE), by=var]
  names(x.2) <- c(var, 'lq')
  x.1 <- merge(x.1, x.2)
  x.3 <- x[ , quantile(N, 0.75, na.rm=TRUE), by=var]
  names(x.3) <- c(var, 'hq')
  x <- merge(x.1, x.3)
  
  return(x)
  
}