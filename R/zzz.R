.onLoad <-
function(lib, pkg){
  
  write("Welcome to cesr 0.50, use help(cesr) to get started", "")

  utils::data(cesnames, package="cesr")
  
  setceslang('Latin')
  
  suppressMessages(require(data.table))
  
}

  