summary.ces.ch <-
function(x){
  
  nind <- length(x$mr_data[ , 1])
  nocc <- nchar(x$mr_data$ch[1]) - 1 # because added an initial time period for residency
  
  # species and years
  chmat <- RMark::splitCH(x$mr_data$ch)
  
  fy <- function(x){ min(which(x == "1")) }
  first.year <- apply(chmat, 1, fy) + x$begin.time-1
    
  smessage <- paste(nind, "adults of", x$spp.name,
                    "captured on", nocc, "occasions,",
                    "at", length(unique(x$mr_data$site)), "sites", 
                    "starting in", x$begin.time, "\n\n", sep=" ")
  cat(smessage)

  # number of birds by year
  if ( is.na(x$group$name) )
    nbirds <- table(first.year, dnn=NULL)
  else 
    nbirds <- t(table(first.year, by=x$mr_data[ ,c(x$group$name)], dnn=NULL))
  cat('Number of birds first ringed in each year\n')
  print(nbirds)
  cat('\n')
  
  # number of retraps
  ### need to to do this by group
  nretraps <- apply( chmat, 1, function(x) sum(x=='1') ) - 1
  cat('Number of times individuals have been recaptured\n')
  print(table(nretraps, dnn=NULL))
  cat('\n')
  
  # number of sites
  if ( is.na(x$group$name) )
    sites <- table(x$mr_data$site, dnn=NULL)
  else
    sites <- t(table(x$mr_data$site, by=x$mr_data[ ,c(x$group$name)], dnn=NULL))
  cat('Number of individuals captured at each site\n')
  print(sites)  
  cat('\n')
  
  invisible(list(nbirds = nbirds, 
                 nretraps = nretraps,
                 sites = sites))
    
}