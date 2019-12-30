summary.ces.sites <-
function(x){
  
  # Mostly a placeholder, probably want to do more?
  cat("Total number of sites operated:", length(x$site), "\n")
  cat("In years", min(x$first.yr), "to", max(x$last.yr), "\n")
  
  invisible(x)
  
}
