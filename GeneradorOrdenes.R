#Archivo para la realizacion de las órdenes globales

#Paquete
library(readxl)
library(dplyr)
#Directorio de trabajo
setwd("C:/Github/FundSeries")
archivo <- read_excel("OrdenPrueba.xls")

#######################################################
#### Creacion de los datos
#######################################################

#Contratos de la operación
contratos <- as.character(archivo$CContrato)

#Tipo de operacion
operacion <- rep("CPA-SI",length(contratos))

#Fondo
fondo <- as.character(paste0("'",archivo$Emisora))

#Precio
pri <- cbind(c("+CIGUB","+CIPLUS","+CIGUMP","+CIGULP","+CIUSD","+CIEQUS","+CIBOLS"),c(1.682033,1.919831,1.099947,1.099757,1.450861,1.056628,2.392346))
prices <- function(valor){
  vector <- match(valor,pri)
  price <- pri[vector,2]
  return(as.numeric(price))
}
precio <- sapply(fondo,prices)

#Serie
antserie <- as.character(archivo$Serie)
newserie <- as.character(sapply(archivo$Importe,serie))
vender <- ifelse(antserie==newserie,"No vender","Vender")

#Tipo de valor
tipo <- ifelse(fondo=="+CIEQUS",52,ifelse(fondo=="+CIBOLS",52,51))

#Cantidad de títulos
titulos <- as.character(archivo$Importe%/%precio)

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

#### Creación del documento csv
document <- data.frame(cbind(contratos,fondo,archivo$Importe,antserie,newserie,tipo))
colnames(document) <- c("Contrato","Fondo","Monto","Serie Anterior","Nueva Serie","Tipo")
write.csv(document,"NuevasPosiciones.csv",col.names=TRUE)

#### Creacion del documento txt
zero <- as.character(integer(length(fondo)))
documento <- c("",paste0(operacion,"|",contratos,"|",fondo,"|",serie,"|",tipo,"|",titulos,"|",precio,"|",zero,"|",zero,"|",zero,"|",zero,"|",zero,"|",importe,"|",fliquidacion,"|",zero,"|",fcaptura,"|",zero,"|",zero,"|",importe,"|",foperacion,"|",precio,"|",zero))
documento <- documento[vender=="Vender"]
#write.table(documento,"file.txt",quote = FALSE,row.names=FALSE,col.names=FALSE)
x <- capture.output(write.table(documento, row.names = FALSE, col.names = FALSE, quote = FALSE))
cat(paste(x, collapse = "\n"), file = "file.txt")

