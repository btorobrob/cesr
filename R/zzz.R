.onLoad <-
function(lib, pkg){
  
  write("Welcome to cesr 1.0.5, use help(cesr) to get started", "")

  utils::data(cesnames, package="cesr")
  
  setceslang('Latin')

}

  