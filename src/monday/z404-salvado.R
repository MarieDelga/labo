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

# agregamos variables (solo relaciones por linea)

dataset$mrentabilidad_annual_pondmes = dataset$mrentabilidad_annual / ifelse(dataset$cliente_antiguedad>12, 12,dataset$cliente_antiguedad)
dataset$mrentabilidad_sobre_annual_pondmes = dataset$mrentabilidad / ifelse(dataset$mrentabilidad_annual_pondmes>0, dataset$mrentabilidad_annual_pondmes,1)

dataset$mrentabilidad_total = dataset$mcomisiones + dataset$mactivos_margen + dataset$mpasivos_margen
dataset$mrentabilidad_total_pondmes = dataset$mrentabilidad_total / dataset$cliente_antiguedad
dataset$mrentabilidad_sobre_total_pondmes = dataset$mrentabilidad / ifelse(dataset$mrentabilidad_total_pondmes>0, dataset$mrentabilidad_total_pondmes,1)

#las sobre estan muy correlacionadas

dataset$mcuenta_corriente_total = dataset$mcuenta_corriente + dataset$mcuenta_corriente_adicional
dataset$mcuenta_corriente_difadic = dataset$mcuenta_corriente_adicional - dataset$mcuenta_corriente #cuanto mas grande, no usa la premium, se va a ir
dataset$mcaja_ahorro_total = dataset$mcaja_ahorro + dataset$mcaja_ahorro_adicional + dataset$mcaja_ahorro_dolares
dataset$mcaja_ahorro_difadic = dataset$mcaja_ahorro_adicional - dataset$mcaja_ahorro #cuanto mas grande, no usa la premium, se va a ir
#total ya esta  mcuentas_saldo

dataset$mautoservicio_prom_transacciones = dataset$mautoservicio / ifelse(dataset$ctarjeta_debito_transacciones>0, dataset$ctarjeta_debito_transacciones,1)
dataset$mtarjeta_visa_consumo_prom_transacciones = dataset$mtarjeta_visa_consumo / ifelse(dataset$ctarjeta_visa_transacciones>0, dataset$ctarjeta_visa_transacciones,1)
dataset$mtarjeta_master_consumo_prom_transacciones = dataset$mtarjeta_master_consumo / ifelse(dataset$ctarjeta_master_transacciones>0, dataset$ctarjeta_master_transacciones,1)

dataset$mprestamos_personales_prom = dataset$mprestamos_personales / ifelse(dataset$cprestamos_personales>0, dataset$cprestamos_personales,1)
dataset$mprestamos_prendarioss_prom = dataset$mprestamos_prendarios / ifelse(dataset$cprestamos_prendarios>0, dataset$cprestamos_personales,1)
dataset$mprestamos_hipotecarios_prom = dataset$mprestamos_hipotecarios / ifelse(dataset$cprestamos_hipotecarios>0, dataset$cprestamos_hipotecarios,1)
dataset$mprestamos_total=dataset$mprestamos_personales  + dataset$mprestamos_prendarios  +  dataset$mprestamos_hipotecarios
dataset$mprestamos_total_prom = dataset$mprestamos_personales_prom + dataset$mprestamos_prendarioss_prom + dataset$mprestamos_hipotecarios_prom 

dataset$mplazo_fijo_total= dataset$mplazo_fijo_dolares  + dataset$mplazo_fijo_pesos
dataset$mplazo_fijo_prom = dataset$mplazo_fijo_total / ifelse(dataset$cplazo_fijo>0, dataset$cplazo_fijo,1)
dataset$minversion1_total = dataset$minversion1_pesos + dataset$minversion1_dolares
dataset$minversion1_prom = dataset$minversion1_total / ifelse(dataset$cinversion1>0, dataset$cinversion1,1)
dataset$minversion2_prom =dataset$minversion2 / ifelse(dataset$cinversion2>0, dataset$cinversion2,1)
dataset$minversion_total = dataset$minversion1_total + dataset$minversion2

dataset$mpayroll_prom = dataset$mpayroll / ifelse(dataset$cpayroll_trx>0, dataset$cpayroll_trx,1)
dataset$mpayroll2_prom = dataset$mpayroll2 / ifelse(dataset$cpayroll2_trx>0, dataset$cpayroll2_trx,1)

dataset$mcuenta_debitos_automaticos_prom = dataset$mcuenta_debitos_automaticos / ifelse(dataset$ccuenta_debitos_automaticos>0, dataset$ccuenta_debitos_automaticos,1 )
dataset$mtarjeta_visa_debitos_automaticos_prom = dataset$mtarjeta_visa_debitos_automaticos / ifelse(dataset$ctarjeta_visa_debitos_automaticos>0, dataset$ctarjeta_visa_debitos_automaticos,1 )
dataset$mttarjeta_master_debitos_automaticos_prom = dataset$mttarjeta_master_debitos_automaticos / ifelse(dataset$ctarjeta_master_debitos_automaticos>0, dataset$ctarjeta_master_debitos_automaticos,1 )

dataset$mpagodeservicios_prom = dataset$mpagodeservicios / ifelse(dataset$cpagodeservicios>0, dataset$cpagodeservicios,1 )
dataset$mpagomiscuentas_prom = dataset$mpagomiscuentas / ifelse(dataset$cpagomiscuentas>0, dataset$cpagomiscuentas,1 )

dataset$mcajeros_propios_descuentos_prom = dataset$mcajeros_propios_descuentos / ifelse(dataset$ccajeros_propios_descuentos>0, dataset$ccajeros_propios_descuentos,1 )
dataset$mtarjeta_visa_descuentos_prom = dataset$mtarjeta_visa_descuentos / ifelse(dataset$ctarjeta_visa_descuentos>0, dataset$ctarjeta_visa_descuentos,1 )
dataset$mtarjeta_master_descuentos_prom = dataset$mtarjeta_master_descuentos / ifelse(dataset$ctarjeta_master_descuentos>0, dataset$ctarjeta_master_descuentos,1 )

dataset$mcomisiones_mantenimiento_prom = dataset$mcomisiones_mantenimiento / ifelse(dataset$ccomisiones_mantenimiento>0, dataset$ccomisiones_mantenimiento,1 )
dataset$mcomisiones_otras_prom = dataset$mcomisiones_otras / ifelse(dataset$ccomisiones_otras>0, dataset$ccomisiones_otras,1 )
dataset$mcomisiones_difotras = dataset$mcomisiones_otras - dataset$mcomisiones_mantenimiento #cuanto mas grande, no usa la premium, se va a ir
dataset$mcomisiones_difotras_prom = dataset$mcomisiones_otras_prom - dataset$mcomisiones_mantenimiento_prom #cuanto mas grande, no usa la premium, se va a ir

dataset$mforex_sell_prom = dataset$mforex_sell / ifelse(dataset$cforex_sell>0, dataset$cforex_sell,1 )
dataset$mforex_buy_prom = dataset$mforex_buy / ifelse(dataset$cforex_buy>0, dataset$cforex_buy,1 )

# Nos quedamos solo con el 202101
dapply  <- dataset[ foto_mes==202103 ]
dapply_train=dapply
dataset <- dataset[foto_mes == 202101]

#borramos numero de cliente
dapply_train = subset(dapply_train, select = -c(numero_de_cliente) )
dataset = subset(dataset, select = -c(numero_de_cliente) )

#ANALISIS VARIABLES
#estado cliente

#plata que tiene 
mcuentas_saldo (TODAS las cuentas del cliente, cajas de ahorro, cuentas corrientes, pesos y dolares)

mcuenta_corriente_total = corriente_premium+corriente_adicional (puede ser negativo)
mcuenta_corriente #premium
mcuenta_corriente_adicional

mcaja_ahorro_total= ahorro_premium+ahorro_adicional+ahorro_dolares
mcaja_ahorro #premium
mcaja_ahorro_adicional
mcaja_ahorro_dolares

# - gastos mes 
mautoservicio (mes tdebito)
mtarjeta_visa_consumo (Monto total de los consumos efectuados durante el mes con la tarjeta de crédito VISA)
mtarjeta_master_consumo (mes master)

mcuenta_debitos_automaticos (Monto total de  débitos automáticos debitados durante el mes en las cuentas  ( no tarjetas de crédito ) )
mtarjeta_visa_debitos_automaticos (Monto total de  débitos automáticos debitados durante el mes en la tarjeta de crédito VISA)
mttarjeta_master_debitos_automaticos (Monto total de  débitos automáticos debitados durante el mes en la tarjeta de crédito MasterCard)

mpagodeservicios
mpagomiscuentas
mcomisiones_mantenimiento
mcomisiones_otras

mtransferencias_emitidas
mextraccion_autoservicio
mcheques_emitidos
mcheques_emitidos_rechazados

# beneficios mes (valor negativo)
mcajeros_propios_descuentos
mtarjeta_visa_descuentos
mtarjeta_master_descuentos

# + ingreso mes
mpayroll_total = mpayroll + mpayroll2
mpayroll
mpayroll2

mtransferencias_recibidas
mcheques_depositados
mcheques_depositados_rechazados

#- - prestamos
mprestamos_total = personales+prendarios+hipotecarios
mprestamos_personales (deuda total )
mprestamos_prendarios (deuda total )
mprestamos_hipotecarios (deuda total )

#+ + plazos fijos/inversiones
mplazo_fijo_total = pesos+dolares
mplazo_fijo_dolares
mplazo_fijo_pesos

minversion_total = minversion1_total+minversion2
minversion1_total = pesos+dolares
minversion1_pesos
minversion1_dolares
minversion2

# cambio divisas
mforex_buy
mforex_sell

# limites financiacion
Master_mfinanciacion_limite

Master_msaldototal
Master_msaldopesos
Master_msaldodolares
Master_mconsumospesos

#rentabilidad banco - comisiones 
#
#banco total
mrentabilidad_total = mcomisiones+mactivos_margen+mpasivos_margen
mcomisiones #ganancia comisiones
mactivos_margen #ganancia por intereses
mpasivos_margen #ganancia por inversiones
#
#banco anual
mrentabilidad_annual #ganancia total
#
#banco mensual
mrentabilidad



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
    
  
    
      # lista de variables a las que queremos transformar
    mis_variables <- c("mrentabilidad", #pesos
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
                       "mttarjeta_visa_debitos_automaticos",#pesos
                       "mttarjeta_master_debitos_automaticos",#pesos
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
                       "Visa_mpagominimo"#pesos
    )
    
    
    # A todas las vamos a rankear
    
    prefix <- "ntile_"
    for (var in mis_variables) {
      train[, (paste(prefix, var, sep = "")) := ntile(get(var), 1000)]
      train[, (var) := NULL]
      
      test[, (paste(prefix, var, sep = "")) := ntile(get(var), 1000)]
      test[, (var) := NULL]
    }
    
    
    
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




#Time difference of 4.692948 hours
#maxdepth=13; minsplit=1592; fraccionminbucket=0.189
#objective: y = 21273333.333


##veo las features mas importantes para el modelo ganador

# Seteamos nuestra primera semilla
set.seed(semillas[1])

in_training <- caret::createDataPartition(dataset$clase_ternaria,
                                          p = 0.70, list = FALSE)
train  <-  dataset[in_training, ]
test   <-  dataset[-in_training, ]

# lista de variables a las que queremos transformar
mis_variables <- c("mrentabilidad", #pesos
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
                   "mttarjeta_visa_debitos_automaticos",#pesos
                   "mttarjeta_master_debitos_automaticos",#pesos
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
                   "Visa_mpagominimo"#pesos
)


# A todas las vamos a rankear

prefix <- "ntile_"
for (var in mis_variables) {
  train[, (paste(prefix, var, sep = "")) := ntile(get(var), 10)]
  train[, (var) := NULL]
  
  test[, (paste(prefix, var, sep = "")) := ntile(get(var), 10)]
  test[, (var) := NULL]
}

for (var in mis_variables) {
  dapply_train[, (paste(prefix, var, sep = "")) := ntile(get(var), 10)]
  dapply_train[, (var) := NULL]}


modelo <- rpart(clase_ternaria ~ ., data = train,
                xval = 0,
                cp = -1,
                minsplit = 1592,
                minbucket = 1592*0.189,
                maxdepth = 13)


#test_prediccion <- predict(modelo, test, type = "prob")
#ganancia(test_prediccion[, "BAJA+2"], test$clase_ternaria) / 0.3


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
        file= "./exp/KA2001/K101_001.csv",
        sep=  "," )
print("listo")
