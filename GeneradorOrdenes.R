###########################################################################################################
#
#   Paso 1, Verificación de la serie
#
###########################################################################################################

#Funciones
options(scipen=999)
source("fondos.R",local=FALSE)

#Paquetes
library(readxl)
library(dplyr)
library(FundTools)

#Directorio de trabajo
archivo <- read_excel("OrdenPrueba.xls")
archivo$Serie <- gsub("'","",archivo$Serie)

diainhabil <-  function(fecha){
  fechabase0 <- as.Date("2017-08-06")
  fechabase1 <- as.Date("2017-08-07")
  if(as.integer(fecha - fechabase0 ) %% 7 == 6){
    fecha = fecha + 3 
  }
  if(as.integer(fecha - fechabase1 ) %% 7 == 6){
    fecha = fecha + 2 
  }
  return(fecha)
}

####################################################################################################
#                                      Fondos a Reclasificar                                       #
####################################################################################################

#fondos <- c('+CIGUB','+CIGUMP','+CIGULP','+CIUSD','+CIEQUS','+CIBOLS','AXESEDM')
fondos <- c('+CIGUB','+CIGUMP','+CIGULP','+CIUSD')

####################################################################################################
#                                 Data frame con las viejas series                                 #
####################################################################################################

datosventa <- archivo %>%
  filter(Emisora %in% fondos & CContrato != 22285 & Importe > 0 & Serie != 'BE-0') %>%
  group_by(CContrato, Emisora, Serie) %>%
  summarise(Titulos = sum(Títulos), Importe = sum(Importe))

####################################################################################################
#                                 Data frame con las nuevas series                                 #
####################################################################################################

datoscompra <- archivo %>% 
  filter(Emisora %in% fondos & CContrato != 22285  & Importe > 0 & Serie != 'BE-0') %>%
  group_by(CContrato, Emisora) %>%
  summarise(Titulos = sum(Títulos), Importe = sum(Importe))

#Serie nueva 

serie_nueva <- c()
for(i in seq(1,length(datoscompra$Importe),1)){
  ind1 <- datosventa$CContrato == datoscompra$CContrato[i]
  ind2 <- datosventa$Emisora == datoscompra$Emisora[i]
  indices <- ifelse(ind1 == TRUE,ind2,ind1)
  tipo <- datosventa$Serie[indices]
  if(nchar(tipo) <= 2){
    tipo <- substr(tipo,1,1)
  } else {
    tipo <- strsplit(tipo,"-")[[1]][1]
  }
  serie_nueva <- c(serie_nueva,serie(datoscompra$Importe[i],as.character(tipo)))
}
datoscompra$Serie <- serie_nueva

####################################################################################################
#                                         Reclasificacion                                          #
####################################################################################################

datos <- merge(datosventa,datoscompra,by.x = c('CContrato','Emisora'),by.y = c('CContrato','Emisora'))
ventas <- ifelse(datos$Serie.x == datos$Serie.y, "No Reclasificar","Reclasificar")
datos$Venta <- ventas
datos <- datos %>% filter(Venta == 'Reclasificar')
datos$Titulos.y <- NULL
datos$Importe.y <- NULL
colnames(datos) <- c('Contrato','Fondo','SerieAnterior','Titulos','Importe','SerieNueva','Accion a realizar')

#Contratos a omitir
omitir <- read_excel('contratos.xlsx')
quitar <- array(match(omitir$Contrato,datos$Contrato))
if(length(quitar) == 0){datos_no_omitidos <- datos} else {datos_no_omitidos <- datos[-quitar,]}
#### Creación del documento csv
datos_finales <- datos_no_omitidos
datos_finales$Fondo <- paste0("'",datos_no_omitidos$Fondo)
write.csv(datos_finales,"NuevasPosiciones.csv",col.names=TRUE)

###########################################################################################################
#
#   Paso 2, Compra - Venta de Renta fija
#
###########################################################################################################
#Renta Fija
datos_rentafija <- datos_no_omitidos[datos_no_omitidos$Fondo %in% c('+CIGUB','+CIGUMP','+CIGULP','+CIUSD','+CIPLUS'),]

#Fondo
fondo_venta <- datos_rentafija$Fondo

#Contratos
contratos_venta <- datos_rentafija$Contrato

#Tipo de operacion
operacion_venta <- rep("VTA-SI",length(contratos_venta))

#Serie de la venta
serie_venta <- datos_rentafija$SerieAnterior

#Tipo de valor
tipo_valor_venta <- ifelse(fondo_venta =="+CIEQUS",52,
                           ifelse(fondo_venta =="+CIBOLS",52,
                                  ifelse(fondo_venta == "AXESEDM",52,51)))

#Precio
precios <- read_excel("Precios.xls",skip = 3)
precios <- data.frame(Emisora=precios$Sociedad,Serie=precios$Clase,Precio=precios$`Precio Contable`)
tipo_valor_precios <- ifelse(precios$Emisora =="+CIEQUS",52,
                             ifelse(precios$Emisora =="+CIBOLS",52,
                                    ifelse(precios$Emisora == "AXESEDM",52,51)))
precios$id <- paste0(tipo_valor_precios,"-",precios$Emisora,"-",precios$Serie)

prices <- function(tipo,fund,serie){
  namesp <- precios$id
  namesf <- paste0(tipo,"-",fund,"-",serie)
  price <- precios$Precio[which(namesp == namesf)]
  return(as.numeric(price))
}
precio_venta <- mapply(prices,tipo_valor_venta,fondo_venta,serie_venta)

#Títulos
titulos_venta <- as.character(datos_rentafija$Titulos)

#Importe de la operacion
importe_ventacompra <- as.numeric(titulos_venta)*precio_venta
importe_venta <- as.character(importe_ventacompra)

#Fecha de Operacion
fecha_operacion <- format(Sys.Date(), "%d/%m/%Y")
foperacion <- rep(fecha_operacion,length(operacion_venta))

#Fecha de liquidacion (en días)
liq <- cbind(c("+CIGUB","+CIPLUS","+CIGUMP","+CIGULP","+CIUSD","+CIEQUS","+CIBOLS","AXESEDM"),c(0,0,2,2,2,2,2,3))
liquidacion <- function(valor){
  vector <- match(valor,liq)
  fliq <- liq[vector,2]
  fechas <- Sys.Date()+as.numeric(fliq)
  fechas <- lapply(fechas,diainhabil)
  fechas <- do.call("c",fechas)
  fliquidacion <- format(fechas,"%d/%m/%Y")
  return(fliquidacion)
}
fecha_liquidacion <- liquidacion(fondo_venta)
fliquidacion <- rep(fecha_liquidacion,2)

#Fecha de Captura
#numero <- ifelse(fondo=="+CIGUB",0,ifelse(fondo=="+CIPLUS",0,-1))
fecha_captura <- format(Sys.Date(), "%d/%m/%Y")
fcaptura <- rep(fecha_captura,length(operacion_venta))

#### Compra

#Tipo de operacion
operacion_compra <- rep("Compra Sociedades Inversio",length(contratos_venta))

#Serie
serie_compra <- datos_rentafija$SerieNueva

#Precio
precio_compra <- mapply(prices,tipo_valor_venta,fondo_venta,serie_compra)

#Títulos
titulos_compra <- as.character(importe_ventacompra%/%precio_compra)

#Importe
importe_compra <- as.character(precio_compra*as.numeric(titulos_compra))

#### Creacion del documento txt
operacion <- c(operacion_venta,operacion_compra)
contratos <- rep(contratos_venta,2)
fondo <- rep(fondo_venta,2)
serie <- c(serie_venta,serie_compra)
tipo <- rep(tipo_valor_venta,2)
titulos <- c(titulos_venta,titulos_compra)
precio <- c(precio_venta,precio_compra)
importe <- c(importe_venta,importe_compra)

zero <- as.character(integer(length(fondo)))
documento <- c("",paste0(operacion,"|",contratos,"|",fondo,"|",serie,"|",tipo,"|",titulos,"|",precio,"|",zero,"|",zero,"|",zero,"|",zero,"|",zero,"|",importe,"|",fliquidacion,"|",zero,"|",fcaptura,"|",zero,"|",zero,"|",importe,"|",foperacion,"|",precio,"|",zero))
x <- capture.output(write.table(documento, row.names = FALSE, col.names = FALSE, quote = FALSE))
cat(paste(x, collapse = "\n"), file = "RentaFija.txt")


###########################################################################################################
#
#   Paso 3, Compra - Venta de Renta Variable
#
###########################################################################################################

#Renta Variable
datos_rentavariable <- datos_no_omitidos[datos_no_omitidos$Fondo %in% c('AXESEDM','+CIBOLS','+CIEQUS'),]

#Fondo
fondo_venta <- datos_rentavariable$Fondo

#Contratos
contratos_venta <- datos_rentavariable$Contrato

#Tipo de operacion
operacion_venta <- rep("VTA-SI",length(contratos_venta))

#Serie de la venta
serie_venta <- datos_rentavariable$SerieAnterior

#Tipo de valor
tipo_valor_venta <- ifelse(fondo_venta =="+CIEQUS",52,
                           ifelse(fondo_venta =="+CIBOLS",52,
                                  ifelse(fondo_venta == "AXESEDM",52,51)))

#Precio
precio_venta <- mapply(prices,tipo_valor_venta,fondo_venta,serie_venta)

#Títulos
titulos_venta <- as.character(datos_rentavariable$Titulos)

#Importe de la operacion
importe_ventacompra <- as.numeric(titulos_venta)*precio_venta
importe_venta <- as.character(importe_ventacompra)

#Fecha de Operacion
fecha_operacion <- format(Sys.Date(), "%d/%m/%Y")
foperacion <- rep(fecha_operacion,length(operacion_venta))

#Fecha de liquidacion (en días)
fecha_liquidacion <- liquidacion(fondo_venta)
fliquidacion <- rep(fecha_liquidacion,2)

#Fecha de Captura
#numero <- ifelse(fondo=="+CIGUB",0,ifelse(fondo=="+CIPLUS",0,-1))
fecha_captura <- format(Sys.Date(), "%d/%m/%Y")
fcaptura <- rep(fecha_captura,length(operacion_venta))

#### Compra

#Tipo de operacion
operacion_compra <- rep("Compra Sociedades Inversio",length(contratos_venta))

#Serie
serie_compra <- datos_rentavariable$SerieNueva

#Precio
precio_compra <- mapply(prices,tipo_valor_venta,fondo_venta,serie_compra)

#Títulos
titulos_compra <- as.character(importe_ventacompra%/%precio_compra)

#Importe
importe_compra <- as.character(precio_compra*as.numeric(titulos_compra))

#### Creacion del documento txt
operacion <- c(operacion_venta,operacion_compra)
contratos <- rep(contratos_venta,2)
fondo <- rep(fondo_venta,2)
serie <- c(serie_venta,serie_compra)
tipo <- rep(tipo_valor_venta,2)
titulos <- c(titulos_venta,titulos_compra)
precio <- c(precio_venta,precio_compra)
importe <- c(importe_venta,importe_compra)

zero <- as.character(integer(length(fondo)))
documento <- c("",paste0(operacion,"|",contratos,"|",fondo,"|",serie,"|",tipo,"|",titulos,"|",precio,"|",zero,"|",zero,"|",zero,"|",zero,"|",zero,"|",importe,"|",fliquidacion,"|",zero,"|",fcaptura,"|",zero,"|",zero,"|",importe,"|",foperacion,"|",precio,"|",zero))
x <- capture.output(write.table(documento, row.names = FALSE, col.names = FALSE, quote = FALSE))
cat(paste(x, collapse = "\n"), file = "RentaVariable.txt")