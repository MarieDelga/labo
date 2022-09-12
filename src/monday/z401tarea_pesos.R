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

# Poner la carpeta de la materia de SU computadora local
setwd("C:\\Users\\Marie\\Documents\\MasterUBA\\DMEyF")
# Poner sus semillas
semillas <- c(432557, 892597, 998197, 214733, 502321)

# Cargamos el dataset
dataset <- fread("./datasets/competencia1_2022.csv")

# Nos quedamos solo con el 202101
dataset <- dataset[foto_mes == 202101]

# Creamos una clase binaria
dataset[, clase_binaria := ifelse(
  clase_ternaria == "BAJA+2",
  "evento",
  "noevento"
)]
# Borramos el target viejo
dataset[, clase_ternaria := NULL]

#borramos numero de cliente
dataset = subset(dataset, select = -c(numero_de_cliente) )





# Seteamos nuestra primera semilla
set.seed(semillas[1])

# funcion ganancia
ganancia <- function(probabilidades, clase) {
  return(sum(
    (probabilidades >= 0.025) * ifelse(clase == "evento", 78000, -2000))
  )
}



# Armamos una función para modelar con el fin de simplificar el código futuro
modelo_rpart <- function(train, test, cp =  0, ms = 20, mb = 1, md = 10) {
  modelo <- rpart(clase_binaria ~ ., data = train,
                  xval = 0,
                  cp = cp,
                  minsplit = ms,
                  minbucket = mb,
                  maxdepth = md)
  
  test_prediccion <- predict(modelo, test, type = "prob")
  ganancia(test_prediccion[, "evento"], test$clase_binaria) / 0.3
  
}


# Una función auxiliar para los experimentos
experimento_rpart <- function(ds, semillas, cp = -1, ms = 20, mb = 1, md = 10) {
  gan <- c()
  for (s in semillas) {
    set.seed(s)
    in_training <- caret::createDataPartition(ds$clase_binaria, p = 0.70,
                                              list = FALSE)
    train  <-  ds[in_training, ]
    test   <-  ds[-in_training, ]
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
      train[, (paste(prefix, var, sep = "")) := ntile(get(var), 10)]
      train[, var := NULL]
      
      test[, (paste(prefix, var, sep = "")) := ntile(get(var), 10)]
      test[, var := NULL]
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
    makeNumericParam("fraccionminbucket",  lower = 0L, upper = 1L)
    # makeNumericParam <- para parámetros continuos
  ),
  # noisy = TRUE,
  has.simple.signature = FALSE
)

start_time <- Sys.time()
t0 <- Sys.time()
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

run_md_ms <- mbo(obj_fun, learner = surr_km, control = ctrl, )
print(run_md_ms)
t1 <- Sys.time()
print(t1 - t0)

#Time difference of 6.819921 hours




##veo las features mas importantes para el modelo ganador

set.seed(semillas[1])
# Cargamos el dataset
dataset <- fread("./datasets/competencia1_2022.csv")

# Nos quedamos solo con el 202101
dataset <- dataset[foto_mes == 202101]

# Creamos una clase binaria
dataset[, clase_binaria := ifelse(
  clase_ternaria == "BAJA+2",
  "evento",
  "noevento"
)]
# Borramos el target viejo
dataset[, clase_ternaria := NULL]

#borramos numero de cliente
dataset = subset(dataset, select = -c(numero_de_cliente) )


in_training <- caret::createDataPartition(dataset$clase_binaria,
                                          p = 0.70, list = FALSE)
dtrain  <-  dataset[in_training, ]
dtest   <-  dataset[-in_training, ]

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
  dtrain[, (paste(prefix, var, sep = "")) := ntile(get(var), 10)]
  dtest[, (paste(prefix, var, sep = "")) := ntile(get(var), 10)]
  dtrain[, var := NULL]
  dtest[, var := NULL]
}

modelo <- rpart(clase_binaria ~ ., data = dtrain,
                xval = 0,
                cp = -1,
                minsplit = aja,
                minbucket = aja,
                maxdepth = aja)

test_prediccion <- predict(modelo, dtest, type = "prob")
ganancia(test_prediccion[, "evento"], dtest$clase_binaria) / 0.3


print(modelo$variable.importance)
