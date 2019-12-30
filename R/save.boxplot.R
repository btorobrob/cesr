save.boxplot <-
function(data=NULL, annual=TRUE, species=TRUE, year=0, filename='bxp', ...){
  
  if( class(data)[1] == 'ces' ){
    
    if( annual ){
      result <- siteplot(data = data, year = 0, plot = FALSE, ...)
      save(list=c('result'), file = paste0(filename, '_year.Rdata'))
    }
    
    if( species ){
      if( year == 0 )
        year <- max(data$year, na.rm = TRUE)
      result <- siteplot(data=data, year = year, plot = FALSE, ...)
      save(result, file = paste0(filename, '_spp', year, '.Rdata'))
    }
  }
  
}