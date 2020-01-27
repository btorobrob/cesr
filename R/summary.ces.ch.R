summary.ces.ch <-
function(cesobj){
  
  chdata <- cesobj$chdata
  
  nind <- length(chdata[ , 1])
  nocc <- nchar(chdata$ch[1]) - 1 # because added an initial time period for residency
  
  # species and years
  chmat <- RMark::splitCH(chdata$ch)
  
  fy <- function(x){ min(which(x == "1")) }
  first.year <- apply(chmat, 1, fy) + cesobj$begin.time-1
  
  smessage <- paste(nind, "adults of", cesobj$spp.name,
                    "captured on", nocc, "occasions,",
                    "at", length(unique(chdata$site)), "sites", 
                    "starting in", cesobj$begin.time, "\n\n", sep=" ")
  cat(smessage)
  
  # number of birds by year
  if ( is.na(cesobj$group$name) )
    nbirds <- table(first.year, dnn=NULL)
  else 
    nbirds <- t(table(first.year, by=chdata[ , c(cesobj$group$name)], dnn=NULL))
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
  if ( is.na(cesobj$group$name) )
    sites <- table(chdata$site, dnn=NULL)
  else
    sites <- t(table(chdata$site, by=chdata[ , c(cesobj$group$name)], dnn=NULL))
  cat('Number of individuals captured at each site\n')
  print(sites)  
  cat('\n')
  
  invisible(list(nbirds = nbirds, 
                 nretraps = nretraps,
                 sites = sites))
  
}