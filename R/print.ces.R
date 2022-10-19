print.ces <-
function(x, ...){
  
  if( !class(x)[1] == 'ces' )
    print(x)
  else 
    switch( class(x)[2],
            res.table = summary.ces.table(x),
            print(x))

}