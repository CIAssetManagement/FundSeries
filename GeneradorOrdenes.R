#Archivo para la realizacion de las órdenes globales

#Paquete
#Directorio de trabajo
setwd("C:/Github/OrdenesGlobales")
archivo <- read.csv("OrdenPrueba.csv",stringsAsFactors = FALSE)
#### Creacion de los datos

#Tipo de operacion
operacion <- as.character(archivo$Operacion)
operacion <- ifelse(operacion == "Compra","CPA.SI",ifelse(operacion == "Venta","VTA.SI","Error"))

#Contratos de la operación
contratos <- as.character(archivo$Contrato)

#Fondo
fondo <- paste0("+",archivo$Fondo)

#Precio
pri <- cbind(c("+CIGUB","+CIPLUS","+CIGUMP","+CIGULP","+CIUSD","+CIEQUS","+CIBOLSA"),c(1,1.1,1.2,1.3,1.4,1.5,1.6))
prices <- function(valor){
  vector <- match(valor,pri)
  price <- pri[vector,2]
  vprice <- format(Sys.Date()+as.numeric(price),"%d/%m/%Y")
  return(vprice)
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

#Fecha de captura
fcaptura <- format(Sys.Date(), "%d/%m/%Y")

#Fecha de liquidacion (en días)
liq <- cbind(c("+CIGUB","+CIPLUS","+CIGUMP","+CIGULP","+CIUSD","+CIEQUS","+CIBOLSA"),c(0,2,2,2,2,3,3))
liquidacion <- function(valor){
  vector <- match(valor,liq)
  fliq <- liq[vector,2]
  fliquidacion <- format(Sys.Date()+as.numeric(fliq),"%d/%m/%Y")
  return(fliquidacion)
}
fliquidacion <- sapply(fondo,liquidacion)

#### Creacion de los documentos
zero <- as.character(integer(length(fondo)))
documento <- c("",paste0(operacion,"|",contratos,"|",fondo,"|",serie,"|",tipo,"|",titulos,"|",precio,"|",zero,"|",zero,"|",zero,"|",zero,"|",zero,"|",importe,"|",fliquidacion,"|",zero,"|",fcaptura,"|",zero,"|",importe,"|",fcaptura,"|",precio,"|",zero))
#write.table(documento,"file.txt",quote = FALSE,row.names=FALSE,col.names=FALSE)
x <- capture.output(write.table(documento, row.names = FALSE, col.names = FALSE, quote = FALSE))
cat(paste(x, collapse = "\n"), file = "file.txt")
