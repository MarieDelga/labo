#Necesita para correr en Google Cloud
# 128 GB de memoria RAM
# 256 GB de espacio en el disco local
#   8 vCPU

# ZZ final que necesita de UNDERSAMPLING

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")

require("lightgbm")

require("primes")


t0 = Sys.time() 
#Parametros del script
PARAM  <- list()
PARAM$experimento  <- "ZZFINAL_semillero_MODEL6" #"ZZ9420"
PARAM$exp_input  <-  "HTFINAL_MODEL0.1_CV" # "HT9420"

PARAM$modelos  <- 1
# FIN Parametros del script

ksemilla  <- 432557

PARAM$semilla_primos <- 697157
PARAM$semillerio <- 100 # ¿De cuanto será nuestro semillerio?
PARAM$indice_inicio_semilla <- 1
PARAM$indice_fin_semilla <- 10



#------------------------------------------------------------------------------
options(error = function() { 
  traceback(20); 
  options(error = NULL); 
  stop("exiting after script error") 
})
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa

base_dir <- "~/buckets/b1/"

#creo la carpeta donde va el experimento
dir.create( paste0( base_dir, "exp/", PARAM$experimento, "/"), showWarnings = FALSE )
setwd(paste0( base_dir, "exp/", PARAM$experimento, "/"))   #Establezco el Working Directory DEL EXPERIMENTO

#leo la salida de la optimizaciob bayesiana
arch_log  <- paste0( base_dir, "exp/", PARAM$exp_input, "/BO_log.txt" )
tb_log  <- fread( arch_log )
setorder( tb_log, -ganancia )

#leo el nombre del expermento de la Training Strategy
arch_TS  <- paste0( base_dir, "exp/", PARAM$exp_input, "/TrainingStrategy.txt" )
TS  <- readLines( arch_TS, warn=FALSE )

#leo el dataset donde voy a entrenar el modelo final
arch_dataset  <- paste0( base_dir, "exp/", TS, "/dataset_train_final.csv.gz" )
dataset  <- fread( arch_dataset )

#leo el dataset donde voy a aplicar el modelo final
arch_future  <- paste0( base_dir, "exp/", TS, "/dataset_future.csv.gz" )
dfuture <- fread( arch_future )

#MAR leo el dataset donde voy a testear el modelo final
arch_test  <- paste0( base_dir, "exp/", TS, "/dataset_test.csv.gz" )
dtest <- fread( arch_test )


#defino la clase binaria
dataset[ , clase01 := ifelse( clase_ternaria %in% c("BAJA+1","BAJA+2"), 1, 0 )  ]

campos_buenos  <- setdiff( colnames(dataset), c( "clase_ternaria", "clase01") )

#---------------------------------
# genero un vector de una cantidad de PARAM$semillerio  de semillas,  buscando numeros primos al azar
primos <- generate_primes(min = 100000, max = 1000000) # genero TODOS los numeros primos entre 100k y 1M
set.seed(PARAM$semilla_primos) # seteo la semilla que controla al sample de los primos
ksemillas <- sample(primos)[1:PARAM$semillerio] # me quedo con  PARAM$semillerio primos al azar


# Guardo las semillas Y EL ORDEN en que son usadas
write.csv(ksemillas, file = "ksemillas.csv", row.names = FALSE)

#-------------------------------------------------------
#genero un modelo para cada uno de las modelos_qty MEJORES iteraciones de la Bayesian Optimization ... para cada semilla del semillero

for( i in  1:PARAM$modelos )
{
  for( ksemilla in ksemillas[PARAM$indice_inicio_semilla:PARAM$indice_fin_semilla] )
  {

    # optimización: si los archivos ya existen, puedo hacer skip de esta semilla
    nom_submit <- paste0(
      PARAM$experimento,
      "_",
      sprintf("%d", ksemilla),
      ".csv"
    )
    
    nom_submit_rank <- paste0(
      PARAM$experimento,
      "_",
      sprintf("%d", ksemilla),
      "_rank.csv"
    )
    
    nom_resultados <- paste0(
      PARAM$experimento,
      "_",
      sprintf("%d", ksemilla),
      "_resultados.csv"
    )
    
    # Salteo las semillas ya procesadas
    if (file.exists(nom_submit) && file.exists(nom_submit_rank) && file.exists(nom_resultados)) {
      next # si, podría ser mas sofisticado, pero queda para el refactor
    }
    
    message("procesando semilla ", ksemilla) # un poco de debug
  
  
  
    parametros  <- as.list( copy( tb_log[ i ] ) )
    iteracion_bayesiana  <- parametros$iteracion_bayesiana
    
    
  
    arch_modelo  <- paste0( "modelo_" ,
                            sprintf( "%02d", i ),
                            "_",
                            sprintf( "%03d", iteracion_bayesiana ),
                            ".model" )
    message("Creando dataset ")
    timestamp()  
  
    #creo CADA VEZ el dataset de lightgbm
    dtrain  <- lgb.Dataset( data=    data.matrix( dataset[ , campos_buenos, with=FALSE] ),
                            label=   dataset[ , clase01],
                            weight=  dataset[ , ifelse( clase_ternaria %in% c("BAJA+2"), 1.0000001, 1.0)],
                            free_raw_data= FALSE
                          )
    timestamp()
  
    ganancia  <- parametros$ganancia
    
    prob_corte <-parametros$prob_corte
  
    #elimino los parametros que no son de lightgbm
    parametros$experimento  <- NULL
    parametros$cols         <- NULL
    parametros$rows         <- NULL
    parametros$fecha        <- NULL
    parametros$prob_corte   <- NULL
    parametros$estimulos    <- NULL
    parametros$ganancia     <- NULL
    parametros$iteracion_bayesiana  <- NULL
  
    if( ! ("leaf_size_log" %in% names(parametros) ) )  stop( "El Hyperparameter Tuning debe tener en BO_log.txt  el pseudo hiperparametro  lead_size_log.\n" )
    if( ! ("coverage" %in% names(parametros) ) ) stop( "El Hyperparameter Tuning debe tener en BO_log.txt  el pseudo hiperparametro  coverage.\n" )
    
    #Primero defino el tamaño de las hojas
    parametros$min_data_in_leaf  <- pmax( 1,  round( nrow(dtrain) / ( 2.0 ^ parametros$leaf_size_log ))  )
    #Luego la cantidad de hojas en funcion del valor anterior, el coverage, y la cantidad de registros
    parametros$num_leaves  <-  pmin( 131072, pmax( 2,  round( parametros$coverage * nrow( dtrain ) / parametros$min_data_in_leaf ) ) )
    cat( "min_data_in_leaf:", parametros$min_data_in_leaf,  ",  num_leaves:", parametros$num_leaves, "\n" )
  
    #ya no me hacen falta
    parametros$leaf_size_log  <- NULL
    parametros$coverage  <- NULL
  
    #Utilizo la semilla definida en este script
    parametros$seed  <- ksemilla
    
    #genero el modelo entrenando en los datos finales
    set.seed( parametros$seed )
    modelo_final  <- lightgbm( data= dtrain,
                               param=  parametros,
                               verbose= -100 )
    
    timestamp()
    

    #grabo el modelo, achivo .model
    lgb.save( modelo_final,
              file= arch_modelo )
  
    #creo y grabo la importancia de variables
    #tb_importancia  <- as.data.table( lgb.importance( modelo_final ) )
    #fwrite( tb_importancia,
    #        file= paste0( "impo_", 
    #                      sprintf( "%02d", i ),
    #                      "_",
    #                      sprintf( "%03d", iteracion_bayesiana ),
    #                      ".txt" ),
    #        sep= "\t" )
  
    message("Prediciendo")
    timestamp()
    
    #genero la prediccion, Scoring
    prediccion  <- predict( modelo_final,
                            data.matrix( dfuture[ , campos_buenos, with=FALSE ] ) )
  
    tb_prediccion  <- dfuture[  , list( numero_de_cliente, foto_mes ) ]
    tb_prediccion[ , prob := prediccion ]
  
    timestamp()
    
    nom_pred  <- paste0( "pred-future--modelRank_",
                         sprintf( "%02d", i ),
                         "--iter_",
                         sprintf( "%03d", iteracion_bayesiana),
                         "--seed_",
                         sprintf("%07d",ksemilla),
                         ".csv"  )
  
    fwrite( tb_prediccion,
            file= nom_pred,
            sep= "\t" )
  
    #genero la prediccion test, Scoring #MAR
    prediccion_test  <- predict( modelo_final,
                                 data.matrix( dtest[ , campos_buenos, with=FALSE ] ) )
    
    tb_prediccion_test  <- dtest[  , list( numero_de_cliente, foto_mes) ]
    tb_prediccion_test[ , prob := prediccion_test ]
    
    
    nom_pred  <- paste0( "pred_test--modelRank_",
                         sprintf( "%02d", i ),
                         "--iter_",
                         sprintf( "%03d", iteracion_bayesiana),
                         "--seed_",
                         sprintf("%07d",ksemilla),
                         ".csv"  )
    
    fwrite( tb_prediccion_test,
            file= nom_pred,
            sep= "\t" )
    
    # genero archivo con corte optimo de BO
    
    setorder( tb_prediccion, -prob )
    
    tb_prediccion[  , Predicted := 0L ]
    tb_prediccion[ 1:prob_corte, Predicted := 1L ]
    
    nom_submit  <- paste0( "Entrega_future--modelRank_",
                           sprintf( "%02d", i ),
                           "--iter_",
                           sprintf( "%03d", iteracion_bayesiana),
                           "--corteOpt_",
                           sprintf( "%05f", prob_corte ),
                           "--seed_",
                           sprintf("%07d",ksemilla),
                           ".csv" )
    
    fwrite(  tb_prediccion[ , list( numero_de_cliente, Predicted ) ],
             file= nom_submit,
             sep= "," )
    
    # genero archivo con corte optimo de BO test
    
    setorder( tb_prediccion_test, -prob )
    
    tb_prediccion_test[  , Predicted := 0L ]
    tb_prediccion_test[ 1:prob_corte, Predicted := 1L ]
    
    nom_submit_test  <- paste0( "E_test--modelRank_",
                                sprintf( "%02d", i ),
                                "--iter_",
                                sprintf( "%03d", iteracion_bayesiana),
                                "--corteOpt_",
                                sprintf( "%05f", prob_corte ),
                                "--seed_",
                                sprintf("%07d",ksemilla),
                                ".csv" )
    
    fwrite(  tb_prediccion_test[ , list( numero_de_cliente, Predicted ) ],
             file= nom_submit_test,
             sep= "," )
    
    
    
    ##genero los archivos para Kaggle
    #cortes  <- seq( from=  7000,
    #                to=   20000,
    #                by=     500 )
  
  
    #setorder( tb_prediccion, -prob )
  
    #for( corte in cortes )
    #{
    #  tb_prediccion[  , Predicted := 0L ]
    #  tb_prediccion[ 1:corte, Predicted := 1L ]
  
    #  nom_submit  <- paste0( PARAM$experimento, 
    #                         "_",
    #                         sprintf( "%02d", i ),
    #                         "_",
    #                         sprintf( "%03d", iteracion_bayesiana ),
    #                         "_",
    #                         sprintf( "%05d", corte ),
    #                         ".csv" )
  
    #  fwrite(  tb_prediccion[ , list( numero_de_cliente, Predicted ) ],
    #           file= nom_submit,
    #           sep= "," )
    #}


    
    
  #borro y limpio la memoria para la vuelta siguiente del for
  rm( tb_prediccion )
  rm( tb_prediccion_test )
  #rm( tb_importancia )
  rm( modelo_final)
  rm( parametros )
  rm( dtrain )
  gc()
  }
  

}

##MAR guardar archivo de real test
nom_submit_test  <- paste0( "Real_test--modelRank",
                            sprintf( "%02d", i ),
                            "--iter_",
                            sprintf( "%03d", iteracion_bayesiana ),
                            "--corteOpt_",
                            sprintf( "%05f", prob_corte ),
                            ".csv" )

fwrite(  dtest[ , list( numero_de_cliente, clase_ternaria ) ],
         file= nom_submit_test,
         sep= "," )

time<-list(Sys.time() - t0)

fwrite( time, 
        file= "time.csv", 
        sep= "," )