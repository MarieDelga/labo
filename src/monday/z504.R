# Limpiamos el entorno
rm(list = ls())
gc(verbose = FALSE)

# Librerías necesarias
require("data.table")
require("rpart")
require("ROCR")
require("ggplot2")
require("lubridate")
require("lhs")
require("DiceKriging")
require("mlrMBO")
require("dplyr")
require("caret")

# Poner la carpeta de la materia de SU computadora local
setwd("C:\\Users\\Marie\\Documents\\MasterUBA\\DMEyF")
# Poner sus semillas
semillas <- c(432557, 892597, 998197, 214733, 502321)

# Cargamos el dataset
dataset <- fread("./datasets/competencia1_2022.csv")




#ANALISIS VARIABLES
#rentabilidad banco mes
mrentabilidad
*mrentabilidad_annual

#tarjetas
Master_mfinanciacion_limite #todo lo que puede gastar con la tarjeta
Master_mlimitecompra #cuanto puede gastar en una compra - se compara con valores prom

Master_msaldototal #MES todo lo que le queda por gastar en la tarjeta o lo que 
Master_msaldopesos
Master_msaldodolares 

Master_mconsumototal # mes 
Master_mconsumospesos#  mes todo lo que debito de la tarjeta incluye descuentos
Master_mconsumosdolares # mes incluye descuentos

Master_mpagominimo


Master_mpagado #total
Master_mpagospesos
Master_mpagosdolares


Visa_mfinanciacion_limite
Visa_msaldototal
Visa_msaldopesos
Visa_msaldodolares
Visa_mconsumospesos
Visa_mconsumosdolares
Visa_mlimitecompra

Visa_mpagado
Visa_mpagospesos
Visa_mpagosdolares
Visa_mconsumototal
Visa_mpagominimo

#gastos mes
mautoservicio #debito
mtarjeta_visa_consumo
mtarjeta_master_consumo

mcuenta_debitos_automaticos
mtarjeta_visa_debitos_automaticos
mttarjeta_master_debitos_automaticos
mpagodeservicios
mpagomiscuentas

mcomisiones_mantenimiento
mcomisiones_otras

mtransferencias_emitidas
mextraccion_autoservicio
mcheques_emitidos
mcheques_emitidos_rechazados

Master_madelantopesos
Master_madelantodolares
Visa_madelantopesos
Visa_madelantodolares

#ingresos mes
mpayroll_total = mpayroll + mpayroll2
mpayroll
mpayroll2

mcajeros_propios_descuentos
mtarjeta_visa_descuentos
mtarjeta_master_descuentos

mtransferencias_recibidas
mcheques_depositados
mcheques_depositados_rechazados


#prestamos
mprestamos_personales #total deuda restante
mprestamos_prendarios #total deuda restante
mprestamos_hipotecarios #total deuda restante
*de alguna forma puedo inferir cuantas cuotas le quedan pagar??
  
  #inversiones
  mplazo_fijo_total = mplazo_fijo_dolares + mplazo_fijo_pesos
mplazo_fijo_dolares
mplazo_fijo_pesos
minversion1_total = minversion1_pesos + minversion1_dolares
minversion1_pesos
minversion1_dolares
minversion2 #pesos
*comprar divisa seria una inversion? tendria que ver gastos en dolares?
  
  #compra divisas -> no me juega mucho porque esta todo pesificado. pero que compre o vende me habla de su economia
  mforex_buy
mforex_sell

#cajero
matm #Monto total en pesos  de transacciones que el cliente realizó durante el mes en cajeros automáticos propiedad del banco.
matm_other #Monto total en pesos  de transacciones que el cliente realizó durante el mes en cajeros automáticos que no son propiedad del banco.
*se relaciona con cextraccion_autoservicio


#rentabilidad total
mrentabilidad_total = mcomisiones + mactivos_margen + mpasivos_margen
mcomisiones
mactivos_margen
mpasivos_margen

# plata actual
mcuentas_saldo  #Saldo total de TODAS las cuentas del cliente, cajas de ahorro, cuentas corrientes, pesos y dolares
mcuenta_corriente_total = mcuenta_corriente + mcuenta_corriente_adicional
mcuenta_corriente #premium
mcuenta_corriente_adicional #pesos
mcuenta_corriente_total = mcaja_ahorro_premium + mcaja_ahorro_adicional +mcaja_ahorro_dolares
mcaja_ahorro #premium
mcaja_ahorro_adicional #pesos
mcaja_ahorro_dolares







#transformacion for linea (independiente de otros sucesos)


dataset$credito_delinquency = dataset$Master_delinquency + dataset$Visa_delinquency

dataset$Master_status_simple = ifelse(dataset$Master_status_simple>5, 9,0)
dataset$Visa_status_simple = ifelse(dataset$Visa_status_simple>5, 9,0)
dataset$Credito_status = dataset$Master_status + dataset$Visa_status
dataset$Credito_status_simple = dataset$Master_status_simple + dataset$Visa_status_simple

dataset$Credito_mfinanciacion_limite = dataset$Master_mfinanciacion_limite + dataset$Visa_mfinanciacion_limite
dataset$Credito_mlimitecompra = dataset$Master_mlimitecompra + dataset$Visa_mlimitecompra


dataset$Credito_msaldopesos = dataset$Master_msaldopesos + dataset$Visa_msaldopesos 
dataset$Credito_msaldodolares = dataset$Master_msaldodolares + dataset$Visa_msaldodolares
dataset$Credito_msaldototal = dataset$Master_msaldototal + dataset$Visa_msaldototal

dataset$Credito_mconsumospesos = dataset$Master_mconsumospesos + dataset$Visa_mconsumospesos
dataset$Credito_mconsumosdolares = dataset$Master_mconsumosdolares + dataset$Visa_mconsumosdolares
dataset$Credito_mconsumos_total = dataset$Credito_mconsumospesos + dataset$Credito_mconsumosdolares


dataset$Master_mconsulmopesos_sobresaldo = dataset$Master_mconsumospesos/ dataset$Master_msaldopesos
dataset$Master_mconsulmosolares_sobresaldo = dataset$Master_mconsumosdolares/ dataset$Master_msaldodolares
dataset$Master_mconsulmo_total_sobresaldo = dataset$Master_mconsumototal / dataset$Master_msaldototal
dataset$Visa_mconsulmopesos_sobresaldo = dataset$Visa_mconsumospesos/ dataset$Visa_msaldopesos 
dataset$Visa_mconsulmosolares_sobresaldo = dataset$Visa_mconsulmosolares/ dataset$Visa_msaldodolares
dataset$Visa_mconsulmo_total_sobresaldo = dataset$Visa_mconsumototal / dataset$Visa_msaldototal
dataset$Credito_mconsumospesos_sobresaldo = dataset$Credito_mconsumospesos / dataset$Credito_msaldopesos
dataset$Credito_mconsumosdolares_sobresaldo = dataset$Credito_mconsumosdolares / dataset$Credito_msaldodolares
dataset$Credito_mconsumos_total_sobresaldo = dataset$Credito_mconsumos_total / dataset$Credito_msaldototal

dataset$Master_madelanto_total = dataset$Master_madelantopesos + dataset$Master_madelantodolares
dataset$Visa_madelanto_total = dataset$Visa_madelantopesos + dataset$visa_madelantodolares
dataset$Credito_madelantopesos = dataset$Master_madelantopesos + dataset$Visa_madelantopesos
dataset$Credito_madelantodolares = dataset$Master_madelantodolares + dataset$Visa_madelantodolares
dataset$Credito_madelanto_total = dataset$Credito_madelantopesos + dataset$Credito_madelantodolares


dataset$Credito_mpagospesos = dataset$Master_mpagospesos + dataset$Visa_mpagospesos
dataset$Credito_mpagosdolares = dataset$Master_mpagosdolares + dataset$Visa_mpagosdolares
dataset$Credito_mpagado = dataset$Master_mpagado + dataset$Visa_mpagado

dataset$Visa_mpagospesos_prom = dataset$Visa_mpagospesos / ifelse(dataset$Visa_fechaalta>0 , dataset$Visa_fechaalta, 1)
dataset$Visa_mpagosdolares_prom = dataset$Visa_mpagosdolares / ifelse(dataset$Visa_fechaalta>0 , dataset$Visa_fechaalta, 1)
dataset$Visa_mpagado_prom = dataset$Visa_mpagado / ifelse(dataset$Visa_fechaalta>0 , dataset$Visa_fechaalta, 1)
dataset$Master_mpagospesos_prom = dataset$Master_mpagospesos / ifelse(dataset$Master_fechaalta>0 , dataset$Master_fechaalta, 1)
dataset$Master_mpagosdolares_prom = dataset$Master_mpagosdolares / ifelse(dataset$Master_fechaalta>0 , dataset$Master_fechaalta, 1)
dataset$Master_mpagado_prom = dataset$Master_mpagado / ifelse(dataset$Master_fechaalta>0 , dataset$Master_fechaalta, 1)
dataset$Credito_mpagospesos_prom = dataset$Visa_mpagospesos_prom + dataset$Master_mpagospesos_prom 
dataset$Credito_mpagosdolares_prom = dataset$Visa_mpagosdolares_prom + dataset$Master_mpagosdolares_prom 
dataset$Credito_mpagado_prom = dataset$Visa_mpagado_prom  + dataset$Master_mpagado_prom 

dataset$Master_cconsumos_prom = dataset$Master_cconsumos/dataset$Master_mconsumototal
dataset$Visa_cconsumos_prom = dataset$Visa_cconsumos/dataset$Visa_mconsumototal

dataset$Visa_mpagospesos_mesvstotal = dataset$Visa_mconsumospesos/dataset$Visa_mpagospesos_prom
dataset$Master_mpagospesos_mesvstotal = dataset$Master_mconsumospesos/dataset$Master_mpagospesos_prom


dataset$mtarjeta_credito_consumo = dataset$mtarjeta_visa_consumo + dataset$mtarjeta_master_consumo
dataset$mtarjeta_credito_debitos_automaticos = dataset$mtarjeta_visa_debitos_automaticos + dataset$mttarjeta_master_debitos_automaticos


dataset$mrentabilidad_total= dataset$mcomisiones + dataset$mactivos_margen + dataset$mpasivos_margen

dataset$mrentabilidad_total_prom = dataset$mrentabilidad_total / dataset$cliente_antiguedad
dataset$mrentabilidad_mesvstotal = dataset$mrentabilidad / ifelse(dataset$mrentabilidad_total_prom>0, dataset$mrentabilidad_total_prom,1)

dataset$mcuenta_corriente_total = dataset$mcuenta_corriente + dataset$mcuenta_corriente_adicional
dataset$mcuenta_corriente_prom = dataset$mcuenta_corriente_total / ifelse(dataset$ccuenta_corriente>0, dataset$ccuenta_corriente,1)
dataset$mcaja_ahorro_total = dataset$mcaja_ahorro + dataset$mcaja_ahorro_adicional + dataset$mcaja_ahorro_dolares
dataset$mcaja_ahorro_prom = dataset$mcaja_ahorro_total / ifelse(dataset$ccaja_ahorro>0, dataset$ccaja_ahorro,1)

dataset$mautoservicio_prom = dataset$mautoservicio / ifelse(dataset$ctarjeta_debito_transacciones>0, dataset$ctarjeta_debito_transacciones,1)
dataset$mtarjeta_visa_consumo_prom = dataset$mtarjeta_visa_consumo / ifelse(dataset$ctarjeta_visa_transacciones>0, dataset$ctarjeta_visa_transacciones,1)
dataset$mtarjeta_master_consumo_prom = dataset$mtarjeta_master_consumo / ifelse(dataset$ctarjeta_master_transacciones>0, dataset$ctarjeta_master_transacciones,1)

dataset$mplazo_fijo_total = dataset$mplazo_fijo_dolares  + dataset$mplazo_fijo_pesos
dataset$mplazo_fijo_prom = dataset$mplazo_fijo_total / ifelse(dataset$cplazo_fijo>0, dataset$cplazo_fijo,1)

dataset$minversion1_total = dataset$minversion1_pesos  + dataset$minversion1_dolares
dataset$minversion1_prom = dataset$minversion1_total / ifelse(dataset$cinversion1>0, dataset$cinversion1,1)

dataset$mpayroll_total = dataset$mpayroll + dataset$mpayroll2
dataset$mpayroll_prom = dataset$mpayroll / ifelse(dataset$cpayroll_trx>0, dataset$cpayroll_trx,1)
dataset$mpayroll2_prom = dataset$mpayroll2 / ifelse(dataset$cpayroll2_trx>0, dataset$cpayroll2_trx,1)
dataset$mpayroll_total_prom = dataset$mpayroll_prom + dataset$mpayroll2_prom

dataset$mcuenta_debitos_automaticos_prom = dataset$cuenta_debitos_automaticos / ifelse(dataset$ccuenta_debitos_automaticos>0, dataset$ccuenta_debitos_automaticos,1)
dataset$mtarjeta_visa_debitos_automaticos = dataset$mttarjeta_visa_debitos_automaticos
dataset[, mttarjeta_visa_debitos_automaticos := NULL]
dataset$mtarjeta_visa_debitos_automaticos_prom = dataset$mtarjeta_visa_debitos_automaticos / ifelse(dataset$ctarjeta_visa_debitos_automaticos>0, dataset$ctarjeta_visa_debitos_automaticos,1)
dataset$mtarjeta_master_debitos_automaticos = dataset$mttarjeta_master_debitos_automaticos
dataset[, mttarjeta_master_debitos_automaticos := NULL]
dataset$mtarjeta_master_debitos_automaticos_prom = dataset$mtarjeta_master_debitos_automaticos / ifelse(dataset$ctarjeta_master_debitos_automaticos>0, dataset$ctarjeta_master_debitos_automaticos,1)
dataset$mpagodeservicios_prom = dataset$mpagodeservicios / ifelse(dataset$cpagodeservicios>0, dataset$cpagodeservicios,1)
dataset$mpagomiscuentas_prom = dataset$mpagomiscuentas / ifelse(dataset$cpagomiscuentas>0, dataset$cpagomiscuentas,1)

dataset$mcajeros_propios_descuentos_prom = dataset$mcajeros_propios_descuentos / ifelse(dataset$ccajeros_propios_descuentos>0, dataset$ccajeros_propios_descuentos,1)
dataset$mtarjeta_visa_descuentoss_prom = dataset$mtarjeta_visa_descuentos / ifelse(dataset$ctarjeta_visa_descuentos>0, dataset$ctarjeta_visa_descuentos,1)
dataset$mtarjeta_master_descuentoss_prom = dataset$mtarjeta_master_descuentos / ifelse(dataset$ctarjeta_master_descuentos>0, dataset$ctarjeta_master_descuentos,1)
dataset$mcomisiones_mantenimiento_prom = dataset$mcomisiones_mantenimiento / ifelse(dataset$ccomisiones_mantenimiento>0, dataset$ccomisiones_mantenimiento,1)
dataset$mcomisiones_otras_prom = dataset$mcomisiones_otras / ifelse(dataset$ccomisiones_otras>0, dataset$ccomisiones_otras,1)

dataset$mtransferencias_recibidas_prom = dataset$mtransferencias_recibidas / ifelse(dataset$ctransferencias_recibidas>0, dataset$ctransferencias_recibidas,1)
dataset$mtransferencias_emitidas_prom = dataset$mtransferencias_emitidas/ ifelse(dataset$ctransferencias_emitidas>0, dataset$ctransferencias_emitidas,1)
dataset$mextraccion_autoservicio_prom = dataset$mextraccion_autoservicio / ifelse(dataset$cextraccion_autoservicio>0, dataset$cextraccion_autoservicio,1)
dataset$mcheques_depositados_prom = dataset$mcheques_depositados / ifelse(dataset$ccheques_depositados>0, dataset$ccheques_depositados,1)
dataset$mcheques_emitidos_prom = dataset$mcheques_emitidos / ifelse(dataset$ccheques_emitidos>0, dataset$ccheques_emitidos,1)
dataset$mcheques_depositados_rechazados_prom = dataset$mcheques_depositados_rechazados / ifelse(dataset$ccheques_depositados_rechazados>0, dataset$ccheques_depositados_rechazados,1)
dataset$mcheques_emitidos_rechazados_prom = dataset$mcheques_emitidos_rechazados / ifelse(dataset$ccheques_emitidos_rechazados>0, dataset$ccheques_emitidos_rechazados,1)

dataset$matm_prom = dataset$matm / ifelse(dataset$catm_trx>0, dataset$catm_trx,1)
dataset$matm_other_prom = dataset$matm_other / ifelse(dataset$catm_trx_other>0, dataset$catm_trx_other,1)

dataset$gastosmes =   dataset$mautoservicio + dataset$mtarjeta_visa_consumo + dataset$mtarjeta_master_consumo +
  dataset$mcuenta_debitos_automaticos + dataset$mtarjeta_visa_debitos_automaticos + dataset$mttarjeta_master_debitos_automaticos +
  dataset$mpagodeservicios + dataset$mpagomiscuentas + dataset$mcomisiones_mantenimiento + dataset$mcomisiones_otras +
  dataset$mtransferencias_emitidas + dataset$mextraccion_autoservicio + dataset$mcheques_emitidos + dataset$mcheques_emitidos_rechazados +
  +dataset$Master_madelantopesos + dataset$Master_madelantodolares + dataset$Visa_madelantopesos + dataset$Visa_madelantodolares

dataset$ingresosmes = dataset$mpayroll_total + dataset$mpayroll + dataset$mpayroll2 +
  dataset$mcajeros_propios_descuentos + dataset$mtarjeta_visa_descuentos + dataset$mtarjeta_master_descuentos +
  dataset$mtransferencias_recibidas + dataset$mcheques_depositados + dataset$mcheques_depositados_rechazados

dataset$balancemes = dataset$mcuentas_saldo + dataset$ingresosmes - dataset$gastosmes

dataset$porc_uso_Master = dataset$Master_msaldototal / dataset$Master_mfinanciacion_limite #porcentaje usa de la tarjeta
dataset$porc_uso_Visa = dataset$Vida_msaldototal / dataset$Visa_mfinanciacion_limite

dataset$prop_compra_master = dataset$mtarjeta_master_consumo_prom / dataset$Master_mlimitecompra #las compra que hago estan cerca del limite de compra?
dataset$prop_compra_visa = dataset$mtarjeta_visa_consumo_prom / dataset$Visa_mlimitecompra






# lista de variables a las que queremos transformar
variables_pesos <- c(
  
  "mtarjeta_credito_debitos_automaticos", #pesos
  "mtarjeta_credito_consumo", #pesos
  "Credito_mfinanciacion_limite", #pesos
  "Credito_mlimitecompra", #pesos
  "Credito_msaldopesos", #pesos
  "Credito_msaldodolares", #pesos
  "Credito_msaldototal", #pesos
  "Credito_mconsumospesos", #pesos
  "Credito_mconsumosdolares", #pesos
  "Credito_mconsumos_total", #pesos
  "Visa_madelanto_total", #pesos
  "Master_madelanto_total", #pesos
  "Credito_madelanto_total", #pesos
  "Credito_madelantodolares", #pesos
  "Credito_madelantopesos", #pesos
  
  "Credito_mpagado", #pesos
  "Credito_mpagospesos", #pesos
  "Credito_mpagosdolares", #pesos
  
  "Visa_mpagospesos_prom", #pesos
  "Visa_mpagosdolares_prom", #pesos
  "Visa_mpagado_prom", #pesos
  "Master_mpagospesos_prom", #pesos
  "Master_mpagosdolares_prom", #pesos
  "Master_mpagado_prom", #pesos
  "Credito_mpagospesos_prom", #pesos
  "Credito_mpagosdolares_prom", #pesos
  "Credito_mpagado_prom", #pesos
  
  "Master_cconsumos_prom", #pesos
  "Visa_cconsumos_prom", #pesos
  
  "mrentabilidad_total", #pesos
  "mrentabilidad_total_prom", #pesos
  
  
  "mcuenta_corriente_total", #pesos
  "mcuenta_corriente_prom", #pesos
  
  "mcaja_ahorro_total", #pesos
  "mcaja_ahorro_prom", #pesos
  
  "mautoservicio_prom", #pesos
  "mtarjeta_visa_consumo_prom", #pesos
  "mtarjeta_master_consumo_prom", #pesos
  
  "mplazo_fijo_total", #pesos
  "mplazo_fijo_prom", #pesos
  
  "minversion1_total", #pesos
  "minversion1_prom", #pesos
  
  "mpayroll_total", #pesos
  "mpayroll_prom", #pesos
  "mpayroll2_prom", #pesos
  "mpayroll_total_prom", #pesos
  
  "mcuenta_debitos_automaticos_prom", #pesos
  "mtarjeta_visa_debitos_automaticos_prom", #pesos
  "mtarjeta_master_debitos_automaticos", #pesos
  "mtarjeta_master_debitos_automaticos_prom", #pesos
  "mpagodeservicios_prom", #pesos
  "mpagomiscuentas_prom", #pesos
  
  "mcajeros_propios_descuentos_prom", #pesos
  "mtarjeta_visa_descuentoss_prom", #pesos
  "mtarjeta_master_descuentoss_prom", #pesos
  "mcomisiones_mantenimiento_prom", #pesos
  "mcomisiones_otras_prom", #pesos
  
  "mtransferencias_recibidas_prom", #pesos
  "mtransferencias_emitidas_prom", #pesos
  "mextraccion_autoservicio_prom", #pesos
  "mcheques_depositados_prom", #pesos
  "mcheques_emitidos_prom", #pesos
  "mcheques_depositados_rechazados_prom", #pesos
  "mcheques_emitidos_rechazados_prom", #pesos
  
  "matm_prom", #pesos
  "matm_other_prom", #pesos
  "gastosmes",
  "ingresosmes",
  "balancemes",
  
  
  "mrentabilidad", #pesos
  "mrentabilidad_annual",#pesos
  "mcomisiones",#pesos
  "mactivos_margen",#pesos
  "mpasivos_margen",#pesos
  "mcuenta_corriente_adicional",#pesos
  "mcuenta_corriente",#pesos
  "mcaja_ahorro",#pesos
  "mcaja_ahorro_adicional",#pesos
  "mcaja_ahorro_dolares",#pesos
  "mcuentas_saldo",#pesos
  "mautoservicio",#pesos
  "mtarjeta_visa_consumo",#pesos
  "mtarjeta_master_consumo",#pesos
  "mprestamos_personales",#pesos
  "mprestamos_prendarios",#pesos
  "mprestamos_hipotecarios",#pesos
  "mplazo_fijo_dolares",#pesos
  "mplazo_fijo_pesos",#pesos
  "minversion1_pesos",#pesos
  "minversion1_dolares",#pesos
  "minversion2",#pesos
  "mpayroll",#pesos
  "mpayroll2",#pesos
  "mcuenta_debitos_automaticos",#pesos
  "mtarjeta_visa_debitos_automaticos",#pesos
  "mpagodeservicios",#pesos
  "mpagomiscuentas",#pesos
  "mcajeros_propios_descuentos",#pesos
  "mtarjeta_visa_descuentos",#pesos
  "mtarjeta_master_descuentos",#pesos
  "mcomisiones_mantenimiento",#pesos
  "mcomisiones_otras",#pesos
  "mforex_buy",#pesos
  "mforex_sell",#pesos
  "mtransferencias_recibidas",#pesos
  "mtransferencias_emitidas",#pesos
  "mextraccion_autoservicio",#pesos
  "mcheques_depositados",#pesos
  "mcheques_emitidos",#pesos
  "mcheques_depositados_rechazados",#pesos
  "mcheques_emitidos_rechazados",#pesos
  "matm",#pesos
  "matm_other",#pesos
  "Master_mfinanciacion_limite",#pesos
  "Master_msaldototal",#pesos
  "Master_msaldopesos",#pesos
  "Master_msaldodolares",#pesos
  "Master_mconsumospesos",#pesos
  "Master_mconsumosdolares",#pesos
  "Master_mlimitecompra",#pesos
  "Master_madelantopesos",#pesos
  "Master_madelantodolares",#pesos
  "Master_mpagado",#pesos
  "Master_mpagospesos",#pesos
  "Master_mpagosdolares",#pesos
  "Master_mconsumototal",#pesos
  "Master_mpagominimo",#pesos
  "Visa_mfinanciacion_limite",#pesos
  "Visa_msaldototal",#pesos
  "Visa_msaldopesos",#pesos
  "Visa_msaldodolares",#pesos
  "Visa_mconsumospesos",#pesos
  "Visa_mconsumosdolares",#pesos
  "Visa_mlimitecompra",#pesos
  "Visa_madelantopesos",#pesos
  "Visa_madelantodolares",#pesos
  "Visa_mpagado",#pesos
  "Visa_mpagospesos",#pesos
  "Visa_mpagosdolares",#pesos
  "Visa_mconsumototal",#pesos
  "Visa_mpagominimo" #pesos
)

#guardo dataset set original con variables lineales para no estarlo corriendo todo el tiempo

##BUSQUEDA DE VARIABLES CON CANARITOS (saco variables ruido)
set.seed(semillas[4])
dataset_train_canaritos<-copy(dataset) # dataset train lo destruyo, le agrego pajaritos 
tracemem(dataset_train_canaritos) == tracemem(dataset) #me aseguro que no estes en la misma memoria


dataset_train_canaritos[, clase_binaria := ifelse(
  clase_ternaria == "BAJA+2",
  "evento",
  "noevento"
)]



#agrego tantos canaritos un tercio de las variables 
for( i in 1:(dim(dataset_train_canaritos)[2]/3) ) dataset_train_canaritos[ , paste0("canarito", i ) :=  runif( nrow(dataset_train_canaritos)) ]


dataset_train_canaritos<- dataset_train_canaritos[foto_mes == 202101]


# A todas las vamos a rankear

prefix <- "ntile_"
for (var in variables_pesos) {
  dataset_train_canaritos[, (paste(prefix, var, sep = "")) := ntile(get(var), 1000)]
  dataset_train_canaritos[, (var) := NULL]}



#borramos numero de cliente
dataset_train_canaritos = subset(dataset_train_canaritos, select = -c(numero_de_cliente) )


#Primero  veo como quedan mis arboles
modelo_original <- rpart(
  formula= clase_binaria ~ . - clase_ternaria,
  data= dataset_train_canaritos,
  model= TRUE,
  xval= 0,
  cp= -1,
  minsplit= 2, # dejo que crezca y corte todo lo que quiera
  minbucket= 1,
  maxdepth= 30 )

#hago el pruning de los canaritos
#haciendo un hackeo a la estructura  modelo_original$frame
# -666 es un valor arbritrariamente negativo que jamas es generado por rpart
modelo_original$frame[ modelo_original$frame$var %like% "canarito", "complexity"] <- -666
modelo_pruned  <- prune(  modelo_original, -666 )

print(modelo_pruned$variable.importance)
### solo me quedo con las de mayor imprtnacia del arbol cortado por los canaritos
vector_importantes <- names( modelo_pruned$variable.importance )

vector_importantes <- c(vector_importantes, "clase_binaria", "clase_ternaria") #ojo que para entrenar hay que sacarle la ternaria
mis_variables = vector_importantes [! vector_importantes %like% "canarito" ]

print(mis_variables)
#mis_variables
length(mis_variables)
#44




###OPTIMIZACION HIPERPARAMETROS

dataset_train_optHiperparam <- copy(dataset)
dataset_train_optHiperparam  <- dataset_optHiperparam[ foto_mes==202101 ]

dataset_train_optHiperparam[, clase_binaria := ifelse(
  clase_ternaria == "BAJA+2",
  "evento",
  "noevento"
)]




# funcion ganancia
ganancia <- function(probabilidades, clase) {
  return(sum(
    (probabilidades >= 0.025) * ifelse(clase == "BAJA+2", 78000, -2000))
  )
}


# Armamos una función para modelar con el fin de simplificar el código futuro
modelo_rpart <- function(train, test, cp =  -1, ms = 20, mb = 1, md = 10) {
  modelo <- rpart(clase_binaria ~ . - clase_ternaria, data = train,
                  xval = 0,
                  cp = cp,
                  minsplit = ms,
                  minbucket = mb,
                  maxdepth = md)
  
  test_prediccion <- predict(modelo, test, type = "prob")
  ganancia(test_prediccion[, "evento"], test$clase_ternaria) / 0.3
  
}



# Una función auxiliar para los experimentos
experimento_rpart <- function(dataset_train , semillas, cp = -1, ms = 20, mb = 1, md = 10) {
  gan <- c()
  for (s in semillas) {
    set.seed(s)
    in_training <- caret::createDataPartition(dataset_train $clase_binaria, p = 0.70,
                                              list = FALSE)
    train  <-  dataset_train [in_training, ]
    test   <-  dataset_train [-in_training, ]
    #train_sample <- tomar_muestra(train)
    
    # A todas las vamos a rankear
    
    prefix <- "ntile_"
    for (var in variables_pesos) {
      train[, (paste(prefix, var, sep = "")) := ntile(get(var), 1000)]
      train[, (var) := NULL]
      
      test[, (paste(prefix, var, sep = "")) := ntile(get(var), 1000)]
      test[, (var) := NULL]
    }
    #tiene que estar despues del ntile
    train = train[,..mis_variables]
    test = test[,..mis_variables]
    
    r <- modelo_rpart(train, test, 
                      cp = cp, ms = ms, mb = mb, md = md)
    gan <- c(gan, r)
  }
  mean(gan)
}



obj_fun_md_ms <- function(x) {
  experimento_rpart(dataset_train_optHiperparam , semillas
                    , md = x$maxdepth
                    , ms = x$minsplit
                    , mb = floor(x$fraccionminbucket*x$minsplit))
}

obj_fun <- makeSingleObjectiveFunction( #Generator for single-objective target functions.
  minimize = FALSE,
  fn = obj_fun_md_ms,
  par.set = makeParamSet(
    makeIntegerParam("maxdepth",  lower = 4L, upper = 30L),
    makeIntegerParam("minsplit",  lower = 1L, upper = 5000L),
    makeNumericParam("fraccionminbucket",  lower = 0L, upper = 2L)
    # makeNumericParam <- para parámetros continuos
  ),
  # noisy = TRUE,
  has.simple.signature = FALSE
)


ctrl <- makeMBOControl() #Creates a control object for MBO optimization.
ctrl <- setMBOControlTermination(ctrl, iters = 500L) #modificar mas mejor
ctrl <- setMBOControlInfill(
  ctrl,
  crit = makeMBOInfillCritEI(),
  opt = "focussearch",
  opt.focussearch.points = 20 # le dejo porque va mas rapido y con mas no mejor la ganancia
)

lrn <- makeMBOLearner(ctrl, obj_fun)

surr_km <- makeLearner("regr.km", predict.type = "se", covtype = "matern3_2")
t0 <- Sys.time()
print(t0)
run_md_ms <- mbo(obj_fun, learner = surr_km, control = ctrl, )
print(run_md_ms)
t1 <- Sys.time()
print(t1 - t0)






#Time difference of 6.475016 hours
#objective: y = 21136000.000
#maxdepth=8; minsplit=376; fraccionminbucket=1.12


##veo las features mas importantes para el modelo ganador


set.seed(semillas[4])

dataset_train  <- copy(dataset)
dataset_train  <- dataset_train[ foto_mes==202101 ]

dataset_train[, clase_binaria := ifelse(
  clase_ternaria == "BAJA+2",
  "evento",
  "noevento"
)]
dataset_train = subset(dataset_train, select = -c(clase_ternaria) )


for (var in variables_pesos) {
  dataset_train[, (paste(prefix, var, sep = "")) := ntile(get(var), 1000)]
  dataset_train[, (var) := NULL]}

dataset_train = dataset_train[,..mis_variables]


modelo <- rpart(clase_binaria ~ . , data = dataset_train ,
                xval = 0,
                cp = -1,
                minsplit = 376,
                minbucket = 376*1.12,
                maxdepth = 8)


print(modelo$variable.importance)


#busco punto de corte optimo
leaves_table <- function(model, train, target, prefix = "") {
  leaves_train_table <- data.table(
    # Devuelve en que hoja cae un caso
    leaves = rpart.predict.leaves(model, train, type = "where"),
    classes = train[, clase_ternaria],
    target = train[, get(target)]
  )
  leaves <- dcast(
    leaves_train_table,
    leaves ~ classes, length,
    value.var = "target")
  leaves <- leaves[
    dcast(
      leaves_train_table,
      leaves ~ target, length,
      value.var = "target"),
    on = .(leaves)]
  leaves[, n := evento + noevento]
  leaves[, p := round(evento / n,4)]
  leaves <- leaves[order(-p),]
  leaves[, gan := `BAJA+2` * 78000 - (CONTINUA + `BAJA+1`) * 2000]
  leaves[, ':='(evento = NULL, noevento = NULL)]
  setnames(leaves, old = c("BAJA+1", "BAJA+2", "CONTINUA", "n", "p", "gan"),
           new = c(paste0(prefix, "b1"),
                   paste0(prefix, "b2"),
                   paste0(prefix, "cont"),
                   paste0(prefix, "n"),
                   paste0(prefix, "p"),
                   paste0(prefix, "gan")))
  leaves[]
}


train_bin <- leaves_table(modelo, dataset_train, "clase_ternaria")
print(train_bin)



#evaluamos modelo
dataset_test  <- dataset[ foto_mes==202101 ]

for (var in variables_pesos) {
  dataset_test[, (paste(prefix, var, sep = "")) := ntile(get(var), 1000)]
  dataset_test[, (var) := NULL]}

dataset_test = dataset_test[,..mis_variables]




### en 202101 se fueron 850 baja+2 y seria 22100000 de ganancia si les pegara a todos
sum(dataset_train$clase_ternaria == "BAJA+2")
850*78000/3

#bajamas2
true_positive = sum((dataset_train$clase_ternaria == "BAJA+2")   *( predict(modelo, dataset_train)[, "evento"]>= 0.025))
#529
false_positive = sum((dataset_train$clase_ternaria != "BAJA+2")   *( predict(modelo, dataset_train)[, "BAJA+2"]>= 0.025))
#8260

#hubo 664 baja+1
sum((dataset_train$clase_ternaria == "BAJA+1")   *( predict(modelo, dataset_train)[, "BAJA+2"]>= 0.025))
#391

# Nos quedamos solo con el 202101
dapply  <- dataset [ foto_mes==202103 ]
dapply_train = dapply

prefix <- "ntile_"
for (var in variables_pesos) {
  dapply_train[, (paste(prefix, var, sep = "")) := ntile(get(var), 1000)]
  dapply_train[, (var) := NULL]}

dapply_train = subset(dapply_train, select = -c(numero_de_cliente) )

dapply_train = dapply_train[,..mis_variables]

#aplico el modelo a los datos nuevos
prediccion  <- predict( object= modelo,
                        newdata= dapply_train,
                        type = "prob")

#prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
#cada columna es el vector de probabilidades 

#agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[ , prob_baja2 := prediccion[, "BAJA+2"] ]

#solo le envio estimulo a los registros con probabilidad de BAJA+2 mayor  a  1/40
dapply[ , Predicted := as.numeric( prob_baja2 > 1/40 ) ]

#genero el archivo para Kaggle
#primero creo la carpeta donde va el experimento
dir.create( "./exp/" )
dir.create( "./exp/KA2001" )

fwrite( dapply[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
        file= "./exp/KA2001/K502_001.csv",
        sep=  "," )
print("listo")
