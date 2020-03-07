print.ces <-
function(cesobj){
  
  if( !class(cesobj)[1] == 'ces' )
    print(cesobj)
  else 
    switch( class(cesobj)[2],
            res.table = summary.ces.table(cesobj),
            print(cesobj))

}