###########################################################################################################
#
#   Paso 1, Verificación de la serie
#
###########################################################################################################

#Paquete
library(readxl)
library(dplyr)
#Directorio de trabajo
setwd("C:/Github/FundSeries")
archivo <- read_excel("OrdenPrueba.xls")


#Contratos de la operación
contratos <- as.character(archivo$CContrato)

#Fondo
fondo <- as.character(archivo$Emisora)
fondo1 <- paste0("'",fondo)

#Serie
antserie <- as.character(gsub("'", "", archivo$Serie))
#Operacion normal
newserie <- as.character(sapply(archivo$Importe,serie))

#Operacion CIPLUS
#newserie <- as.character(sapply(archivo$Importe,seriep))

vender <- ifelse(antserie==newserie,"No vender","Vender")

#### Creación del documento csv
document <- data.frame(cbind(contratos,fondo1,archivo$Importe,antserie,newserie,vender))
colnames(document) <- c("Contrato","Fondo","Monto","Serie Anterior","Nueva Serie","Acción a realizar")
write.csv(document,"NuevasPosiciones.csv",col.names=TRUE)



###########################################################################################################
#
#   Paso 2, Venta
#
###########################################################################################################

#Tipo de operacion
operacion <- rep("VTA-SI",length(contratos))

#Serie de la venta
serie1 <- gsub("'","",antserie)

#Precio
precios <- read.csv("Precios.csv",header = TRUE)
prices <- function(fund,ser){
  names <- paste0("+",colnames(precios))
  vectorc <- match(fund,names)
  vectorr <- match(ser,precios[,1])
  price <- precios[vectorr,vectorc]
  return(as.numeric(price))
}
precio <- mapply(prices,fondo,serie1)

#Tipo de valor
tipo <- ifelse(fondo=="+CIEQUS",52,ifelse(fondo=="+CIBOLS",52,51))

#Títulos
titulos <- as.character(archivo$Títulos)

#Importe de la operacion
importe1 <- as.numeric(titulos)*precio
importe <- as.character(importe1)

#Fecha de Operacion
foperacion <- format(Sys.Date(), "%d/%m/%Y")

#Fecha de liquidacion (en días)
liq <- cbind(c("+CIGUB","+CIPLUS","+CIGUMP","+CIGULP","+CIUSD","+CIEQUS","+CIBOLS"),c(0,0,2,2,2,3,3))
liquidacion <- function(valor){
  vector <- match(valor,liq)
  fliq <- liq[vector,2]
  fechas <- Sys.Date()+as.numeric(fliq)
  fechas <- lapply(fechas,diainhabil)
  fechas <- do.call("c",fechas)
  fliquidacion <- format(fechas,"%d/%m/%Y")
  return(fliquidacion)
}
fliquidacion <- liquidacion(fondo)

#Fecha de Captura
#numero <- ifelse(fondo=="+CIGUB",0,ifelse(fondo=="+CIPLUS",0,-1))
fcaptura <- format(Sys.Date(), "%d/%m/%Y")

#### Creacion del documento txt
zero <- as.character(integer(length(fondo)))
documento <- c("",paste0(operacion,"|",contratos,"|",fondo,"|",serie1,"|",tipo,"|",titulos,"|",precio,"|",zero,"|",zero,"|",zero,"|",zero,"|",zero,"|",importe,"|",fliquidacion,"|",zero,"|",fcaptura,"|",zero,"|",zero,"|",importe,"|",foperacion,"|",precio,"|",zero))
documento <- documento[vender=="Vender"]
#write.table(documento,"ventas.txt",quote = FALSE,row.names=FALSE,col.names=FALSE)
x <- capture.output(write.table(documento, row.names = FALSE, col.names = FALSE, quote = FALSE))
cat(paste(x, collapse = "\n"), file = "venta.txt")



###########################################################################################################
#
#   Paso 3, Compra
#
###########################################################################################################

#Tipo de operacion
operacion <- rep("Compra Sociedades Inversio",length(contratos))

#Serie
serie2 <- newserie

#Precio
precio <- mapply(prices,fondo,serie2)

#Títulos
titulos <- as.character(importe1%/%precio)

#Importe
importe <- as.character(precio*as.numeric(titulos))

#### Creacion del documento txt
zero <- as.character(integer(length(fondo)))
documento <- c("",paste0(operacion,"|",contratos,"|",fondo,"|",serie2,"|",tipo,"|",titulos,"|",precio,"|",zero,"|",zero,"|",zero,"|",zero,"|",zero,"|",importe,"|",fliquidacion,"|",zero,"|",fcaptura,"|",zero,"|",zero,"|",importe,"|",foperacion,"|",precio,"|",zero))
documento <- documento[vender=="Vender"]
#write.table(documento,"compra.txt",quote = FALSE,row.names=FALSE,col.names=FALSE)
x <- capture.output(write.table(documento, row.names = FALSE, col.names = FALSE, quote = FALSE))
cat(paste(x, collapse = "\n"), file = "compra.txt")

