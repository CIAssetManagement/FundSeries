#Archivo para la realizacion de las órdenes globales

#Paquete
#Directorio de trabajo
setwd("C:/Github/FundSeries")
archivo <- read.csv("OrdenPrueba1.csv",stringsAsFactors = FALSE)
#### Creacion de los datos

#Tipo de operacion
operacion <- as.character(archivo$Operacion)
operacion <- ifelse(operacion == "Compra","CPA-SI",ifelse(operacion == "Venta","VTA-SI","Error"))

#Contratos de la operación
contratos <- as.character(archivo$Contrato)

#Fondo
fondo <- paste0("+",archivo$Fondo)

#Precio
pri <- cbind(c("+CIGUB","+CIPLUS","+CIGUMP","+CIGULP","+CIUSD","+CIEQUS","+CIBOLS"),c(1.682033,1.919831,1.099947,1.099757,1.450861,1.056628,2.392346))
prices <- function(valor){
  vector <- match(valor,pri)
  price <- pri[vector,2]
  return(as.numeric(price))
}
precio <- sapply(fondo,prices)

#Serie
serie <- as.character(archivo$Serie)

#Tipo de valor
tipo <- as.character(archivo$Tipo.de.Valor)

#Cantidad de títulos
titulos <- as.character(archivo$Monto%/%precio)

#Importe de la operacion
importe <- as.character(as.numeric(titulos)*precio)

#Fecha de Operacion
foperacion <- format(Sys.Date()-5, "%d/%m/%Y")

#Fecha de liquidacion (en días)
liq <- cbind(c("+CIGUB","+CIPLUS","+CIGUMP","+CIGULP","+CIUSD","+CIEQUS","+CIBOLS"),c(0,2,2,2,2,3,3))
liquidacion <- function(valor){
  vector <- match(valor,liq)
  fliq <- liq[vector,2]
  fechas <- Sys.Date()-5+as.numeric(fliq)
  fechas <- lapply(fechas,diainhabil)
  fechas <- do.call("c",fechas)
  fliquidacion <- format(fechas,"%d/%m/%Y")
  return(fliquidacion)
}

#Fecha de Captura
numero <- ifelse(fondo=="+CIGUB",0,ifelse(fondo=="+CIPLUS",0,-1))
fcaptura <- format(Sys.Date()+-5+numero, "%d/%m/%Y")

#### Creacion de los documentos
zero <- as.character(integer(length(fondo)))
documento <- c("",paste0(operacion,"|",contratos,"|",fondo,"|",serie,"|",tipo,"|",titulos,"|",precio,"|",zero,"|",zero,"|",zero,"|",zero,"|",zero,"|",importe,"|",fliquidacion,"|",zero,"|",fcaptura,"|",zero,"|",zero,"|",importe,"|",foperacion,"|",precio,"|",zero))
#write.table(documento,"file.txt",quote = FALSE,row.names=FALSE,col.names=FALSE)
x <- capture.output(write.table(documento, row.names = FALSE, col.names = FALSE, quote = FALSE))
cat(paste(x, collapse = "\n"), file = "file.txt")

