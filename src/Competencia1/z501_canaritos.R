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

# Poner la carpeta de la materia de SU computadora local
setwd("C:\\Users\\Marie\\Documents\\MasterUBA\\DMEyF")
# Poner sus semillas
semillas <- c(432557, 892597, 998197, 214733, 502321)

# Cargamos el dataset
dataset <- fread("./datasets/competencia1_2022.csv")

#transformacion for linea (independiente de otros sucesos)

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

dataset$prop_compra_master = dataset$Master_mconsumototal/dataset$Master_mlimitecompra #las compra que hago estan cerca del limite de compra?
dataset$prop_compra_visa = dataset$Visa_mconsumototal/dataset$Visa_mlimitecompra






# lista de variables a las que queremos transformar
variables_pesos <- c("mrentabilidad_total", #pesos
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



#agrego tamtos canaritos un tercio de las variables 
for( i in 1:(dim(dataset)[2]/3) ) dataset[ , paste0("canarito", i ) :=  runif( nrow(dataset)) ]

# Nos quedamos solo con el 202101
dapply  <- dataset[ foto_mes==202103 ]
dapply_train = dapply
dataset <- dataset[foto_mes == 202101]

# A todas las vamos a rankear

prefix <- "ntile_"
for (var in variables_pesos) {
  dataset[, (paste(prefix, var, sep = "")) := ntile(get(var), 1000)]
  dataset[, (var) := NULL]
  dapply_train[, (paste(prefix, var, sep = "")) := ntile(get(var), 1000)]
  dapply_train[, (var) := NULL]}



#borramos numero de cliente
dataset = subset(dataset, select = -c(numero_de_cliente) )
dapply_train = subset(dapply_train, select = -c(numero_de_cliente) )

#Primero  veo como quedan mis arboles
modelo_original <- rpart(
  formula= clase_ternaria ~ .,
  data= dataset,
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

mis_variables <- c("clase_ternaria",
                    "ctrx_quarter",
                   "ntile_mcuentas_saldo",
                   "ccomisiones_otras",
                   "active_quarter",
                   "Visa_status",
                   "cdescubierto_preacordado",
                   "ntile_mcuenta_corriente_total",
                   "ntile_mcuenta_corriente",
                   "ntile_mcuenta_corriente_prom",
                   "ntile_mcomisiones_mantenimiento",
                   "ntile_mcomisiones_mantenimiento_prom",
                   "cprestamos_personales",
                   "ntile_mcomisiones_otras_prom",
                   "ntile_mprestamos_personales",
                   "ntile_mcomisiones",
                   "ntile_mcomisiones_otras",
                   "ntile_mcaja_ahorro",
                   "ntile_Visa_mlimitecompra",
                   "Master_status",
                   "ntile_Visa_mfinanciacion_limite",
                   "ntile_mactivos_margen",
                   "ntile_mtarjeta_visa_debitos_automaticos_prom",
                   "ctarjeta_master",
                   "ntile_Visa_msaldototal",
                   "ntile_Visa_msaldopesos",
                   "cproductos",
                   "ctarjeta_visa",
                   "cliente_antiguedad",
                   "mrentabilidad_mesvstotal",
                   "ntile_mpasivos_margen",
                   "Visa_fechaalta",
                   "ntile_mtarjeta_visa_consumo",
                   "ntile_mtarjeta_visa_debitos_automaticos",
                   "ntile_mtarjeta_visa_consumo_prom",
                   "ntile_Visa_mpagominimo",
                   "ccomisiones_mantenimiento",
                   "ntile_ingresosmes",
                   "ntile_Master_mfinanciacion_limite",
                   "ntile_Master_mlimitecompra",
                   "cliente_edad",
                   "ctarjeta_visa_debitos_automaticos",
                   "ntile_mrentabilidad",
                   "ntile_mcaja_ahorro_total",
                   "ntile_mcaja_ahorro_prom",
                   "ctarjeta_debito_transacciones",
                   "tcuentas",
                   "ntile_mrentabilidad_total",
                   "ctarjeta_visa_transacciones",
                   "ctarjeta_master_debitos_automaticos",
                   "Visa_Fvencimiento",
                   "ntile_mpayroll",
                   "ntile_mpayroll_total",
                   "ntile_mpayroll_prom",
                   "ntile_mpayroll_total_prom",
                   "internet",
                   "Visa_fultimo_cierre",
                   "ntile_mtransferencias_recibidas",
                   "ctarjeta_debito",
                   "ctransferencias_emitidas",
                   "cinversion2",
                   "catm_trx_other",
                   "porc_uso_Master",
                   "ntile_mrentabilidad_annual",
                   "ntile_mtarjeta_master_debitos_automaticos",
                   "ccaja_ahorro",
                   "ntile_mtarjeta_master_debitos_automaticos_prom",
                   "ccaja_seguridad"
)





dataset = dataset[,..mis_variables]
dapply_train = dapply_train[,..mis_variables]







# Cargamos el dataset
dataset <- fread("./datasets/competencia1_2022.csv")







# funcion ganancia
ganancia <- function(probabilidades, clase) {
  return(sum(
    (probabilidades >= 0.025) * ifelse(clase == "BAJA+2", 78000, -2000))
  )
}


# Armamos una función para modelar con el fin de simplificar el código futuro
modelo_rpart <- function(train, test, cp =  -1, ms = 20, mb = 1, md = 10) {
  modelo <- rpart(clase_ternaria ~ ., data = train,
                  xval = 0,
                  cp = cp,
                  minsplit = ms,
                  minbucket = mb,
                  maxdepth = md)
  
  test_prediccion <- predict(modelo, test, type = "prob")
  ganancia(test_prediccion[, "BAJA+2"], test$clase_ternaria) / 0.3
  
}



# Una función auxiliar para los experimentos
experimento_rpart <- function(dataset, semillas, cp = -1, ms = 20, mb = 1, md = 10) {
  gan <- c()
  for (s in semillas) {
    set.seed(s)
    in_training <- caret::createDataPartition(dataset$clase_ternaria, p = 0.70,
                                              list = FALSE)
    train  <-  dataset[in_training, ]
    test   <-  dataset[-in_training, ]
    #train_sample <- tomar_muestra(train)
    
    
    
    
    
    # A todas las vamos a rankear
    

    
    
    r <- modelo_rpart(train, test, 
                      cp = cp, ms = ms, mb = mb, md = md)
    gan <- c(gan, r)
  }
  mean(gan)
}



obj_fun_md_ms <- function(x) {
  experimento_rpart(dataset, semillas
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
run_md_ms <- mbo(obj_fun, learner = surr_km, control = ctrl, )
print(run_md_ms)
t1 <- Sys.time()
print(t1 - t0)






#Time difference of 2.100825 hours
#
#objective: y = 20900000.000
#maxdepth=21; minsplit=1210; fraccionminbucket=0.208


##veo las features mas importantes para el modelo ganador


set.seed(semillas[1])

modelo <- rpart(clase_ternaria ~ ., data = dataset,
                xval = 0,
                cp = -1,
                minsplit = 1210,
                minbucket = 1210*0.208,
                maxdepth = 21)


print(modelo$variable.importance)


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
        file= "./exp/KA2001/K501_001.csv",
        sep=  "," )
print("listo")
