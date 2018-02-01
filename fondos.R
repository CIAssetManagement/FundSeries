#Clasificación de los clientes en serie accionaria.
serie <- function(monto,tipo = 'C'){
  #CIGUB, CIGUMP, CIGULP, CIUSD, CIEQUS, CIBOLS
  if(tipo == 'C' | tipo == 'BE'){
    if(tipo == 'C'){
      series <- c("C-0","C-1","C-2","C-3","C-4")
    } else {
      series <- c("BE-0","BE-1","BE-2","BE-3","BE-4")
    }
    montos <- c(100000000,30000000,5000000,1000000,10000)
    
    if(monto >= montos[1]){return(series[1])}
    if(between(monto,montos[2],montos[1])){return(series[2])}
    if(between(monto,montos[3],montos[2])){return(series[3])}
    if(between(monto,montos[4],montos[3])){return(series[4])}
    if(between(monto,montos[5],montos[4])){return(series[5])}
    if(monto < montos[5]){return(series[5])}
  }
  #CIPLUS
  if(tipo == 'BF'){
    series <- c("BF-2","BF-5","BF-7")
    montos <- c(500000,100000)
    
    if(monto >= montos[1]){return(series[1])}
    if(between(monto,montos[2],montos[1])){return(series[2])}
    if(monto < montos[2]){return(series[3])}
  }
  if(tipo == 'F' | tipo == 'M'){
    if(tipo == 'F'){
      series <- c("F1","F3")
    } else {
      series <- c("M1","M3")
    }
    montos <- c(100000,9999999)
    
    if(monto <= montos[2]){return(series[1])}
    if(monto > montos[2]){return(series[2])}
  }
}