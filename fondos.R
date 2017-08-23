#Clasificación de los clientes en serie accionaria.
serie <- function(monto){
  #Series that investors can achieve
  series <- c("'C-0","'C-1","'C-2","'C-3","'C-4")
  #Minimum money required per series
  montos <- c(100000000,30000000,5000000,1000000,10000)
  
  if(monto >= montos[1]){return(series[1])}
  if(between(monto,montos[2],montos[1])){return(series[2])}
  if(between(monto,montos[3],montos[2])){return(series[3])}
  if(between(monto,montos[4],montos[3])){return(series[4])}
  if(between(monto,montos[5],montos[4])){return(series[5])}
  if(monto < montos[5]){series[5]}
}