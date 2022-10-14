
#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("lightgbm")
require("ggplot2")


#defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
PARAM <- list()
PARAM$experimento  <- "KA7242"


PARAM$input$training      <- c( 202103 )
PARAM$input$future        <- c( 202105 )

PARAM$finalmodel$max_bin           <-     31
PARAM$finalmodel$learning_rate     <-      0.0107  
PARAM$finalmodel$num_iterations    <-    328  #615
PARAM$finalmodel$num_leaves        <-     427
PARAM$finalmodel$min_data_in_leaf  <-   2115
PARAM$finalmodel$feature_fraction  <-     0.241
PARAM$finalmodel$semilla           <- 102191
PARAM$finalmodel$envios <- 8193


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa
#setwd("C:\\Users\\Marie\\Documents\\MasterUBA\\DMEyF")

setwd("~/buckets/b1/")   #Establezco el Working Directory

#cargo el dataset donde voy a trabajar
dataset  <- fread("./datasets/competencia3_2022.csv.gz", stringsAsFactors= TRUE)



#--------------------------------------

#paso la clase a binaria que tome valores {0,1}  enteros
#set trabaja con la clase  POS = { BAJA+1, BAJA+2 } 

#paso la clase a binaria que tome valores {0,1}  enteros
dataset[ , clase01 := ifelse( clase_ternaria=="CONTINUA", 0L, 1L) ]


#--------------------------------------

#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01","numero_de_cliente" ) )

#--------------------------------------


#establezco donde entreno
dataset[ , train  := 0L ]
dataset[ foto_mes %in% PARAM$input$training, train  := 1L ]

#--------------------------------------
#creo las carpetas donde van los resultados
#creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( paste0("./exp/", PARAM$experimento, "/" ), showWarnings = FALSE )
setwd( paste0("./exp/", PARAM$experimento, "/" ) )   #Establezco el Working Directory DEL EXPERIMENTO



#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[ train==1L, campos_buenos, with=FALSE]),
                        label= dataset[ train==1L, clase01] )


#aplico el modelo a los datos sin clase
dapply  <- dataset[ foto_mes== PARAM$input$future ]




#funcion ganacia
ganancia <- function(probabilidades, clase) {
  return(sum(
    (probabilidades >= 0.05) * ifelse(clase == 1, 78000, -2000))
  )
}

resultados_nofe <- c()
n <- 100


set.seed(7)
t0 <- Sys.time()
for (i in 1:n) {
  
  
  #genero el modelo
  #estos hiperparametros  salieron de una laaarga Optmizacion Bayesianai
  
  modelo  <- lgb.train( data= dtrain,
                        param= list( objective=          "binary",
                                     max_bin=            PARAM$finalmodel$max_bin,
                                     learning_rate=      PARAM$finalmodel$learning_rate,
                                     num_iterations=     PARAM$finalmodel$num_iterations,
                                     num_leaves=         PARAM$finalmodel$num_leaves,
                                     min_data_in_leaf=   PARAM$finalmodel$min_data_in_leaf,
                                     feature_fraction=   PARAM$finalmodel$feature_fraction,
                                     seed=               i
                        )
  )
  
  
  #aplico el modelo a los datos nuevos
  prediccion  <- predict( modelo, 
                          data.matrix( dapply[, campos_buenos, with=FALSE ])    )
  
  
  tb_entrega  <-  dapply[ , list( numero_de_cliente, foto_mes,clase01 ) ]
  tb_entrega[  , prob := prediccion ]
  
  
  
  #ordeno por probabilidad descendente
  setorder( tb_entrega, -prob )
  
  
  tb_entrega[  , Predicted := 0L ]
  tb_entrega[ 1:PARAM$finalmodel$envios, Predicted := 1L ]
  
  
  gan <- ganancia(tb_entrega$Predicted, tb_entrega$clase01 ) 
  
  resultados_nofe <- c(resultados_nofe, gan)
}


time_nofe<-list(Sys.time() - t0)
fwrite( time_nofe, 
        file= "time_nofe.csv", 
        sep= "\t" )


ggplot() + aes(resultados_nofe) + geom_density()


fwrite( list(resultados_nofe), 
        file= "resultados_nofe.csv", 
        sep= "\t" )

tb_importancia  <-  as.data.table( lgb.importance(modelo) ) 
fwrite( tb_importancia, 
        file= "imponofe.csv", 
        sep= "\t" )

