diainhabil <-  function(fecha){
  fechabase0 <- as.Date("2017-08-06")
  fechabase1 <- as.Date("2017-08-07")
  if(as.integer(fecha - fechabase0 ) %% 7 == 6){
      fecha = fecha + 2 
    }
  if(as.integer(fecha - fechabase1 ) %% 7 == 6){
    fecha = fecha + 1 
  }
  return(fecha)
}