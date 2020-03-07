summary.ces.table <-
function(cesobj){
  
  print(knitr::kable(cesobj$table, align='r', row.names=TRUE))
  
}