#Arbol elemental con libreria  rpart
#Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot

#cargo las librerias que necesito
install.packages( "data.table", dependencies=TRUE )
install.packages("rpart")
install.packages("rpart.plot")
require("data.table")
require("rpart")
require("rpart.plot")

#Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("C:\\Users\\Marie\\Documents\\MasterUBA\\DMEyF")  #Establezco el Working Directory

#cargo el dataset
dataset  <- fread("./datasets/competencia1_2022.csv")

dtrain  <- dataset[ foto_mes==202101 ]  #defino donde voy a entrenar
dapply  <- dataset[ foto_mes==202103 ]  #defino donde voy a aplicar el modelo

# Supongamos que tenemos una lista de variables a las que queremos transformar
mis_variables <- c("cliente_edad",
                   "cliente_antiguedad",
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
  daply[, (paste(prefix, var, sep = "")) := ntile(get(var), 10)]
}



#genero el modelo,  aqui se construye el arbol
modelo  <- rpart(formula=   "clase_ternaria ~ .",  #quiero predecir clase_ternaria a partir de el resto de las variables
                 data=      dtrain,  #los datos donde voy a entrenar
                 xval=      0,
                 cp=       -1,   #esto significa no limitar la complejidad de los splits
                 minsplit=  16,     #minima cantidad de registros para que se haga el split
                 minbucket= 9,     #tamaño minimo de una hoja
                 maxdepth=  22 )    #profundidad maxima del arbol


#grafico el arbol
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)


#aplico el modelo a los datos nuevos
prediccion  <- predict( object= modelo,
                        newdata= dapply,
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
