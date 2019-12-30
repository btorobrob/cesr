summary.ces.plots <-
function(x){

  sites <- x$coverage[x$coverage$include, ]
  total <- length(unique(sites$site))
  years <- unique(sites[ , 1:2])
  sum.plots <- table(xtabs(~site, years), dnn='')
  
  year <- seq(min(x$years$year), max(x$years$year))
  n.year <- table(tapply(sites$year,sites$site,min))
  cum.years <- merge(as.data.frame(year),as.data.frame(n.year), 
                     by.x='year', by.y='Var1', all.x=TRUE)
  cum.years[is.na(cum.years)] <- 0
  cum.years$Freq <- cumsum(cum.years$Freq)
  cum.len <- cumsum(sum.plots)                                                    
                                                    
  cat("Total number of sites operated:", total, "\n\n")
  cat("Number of sites in each year:\n")
  print(table(years$year))
  cat("\nNumber of years sites run for:\n")
  print(sum.plots)

  result <- list(total = total,
                 years = years,
                 plots = sum.plots,
                 cumtots = list(years=cum.years,
                                length=cum.len))
  class(result) <- c('ces', 'plot.summary')

  invisible(result)
}

