# source( "~/labo/src/lightgbm/z723_lightgbm_binaria_BO.r" )
# Este script esta pensado para correr en Google Cloud
#   8 vCPU
#  32 GB memoria RAM
# 256 GB espacio en disco

# se entrena con POS =  { BAJA+1, BAJA+2 }
# Optimizacion Bayesiana de hiperparametros de  lightgbm, con el metodo TRADICIONAL de los hiperparametros originales de lightgbm
# 5-fold cross validation
# la probabilidad de corte es un hiperparametro

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rlist")
require("Rcpp")
require("ranger")



require("lightgbm")

#paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")

options(error = function() { 
  traceback(20); 
  options(error = NULL); 
  stop("exiting after script error") 
})



#Aqui se cargan los hiperparametros
hs <- makeParamSet( 
         makeNumericParam("learning_rate",    lower=    0.005, upper=    0.3),
         makeNumericParam("feature_fraction", lower=    0.2  , upper=    1.0), #0.8
         makeIntegerParam("min_data_in_leaf", lower=    500L   , upper=  15000L),
         makeIntegerParam("num_leaves",       lower=   16L   , upper=  1024L),
         makeIntegerParam("envios",           lower= 5000L   , upper= 15000L)
        )
#max_bin =31
#baging_fraction
#extra_tree
#regularizaciones

#defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
PARAM  <- list()

PARAM$experimento  <- "HT7233"

PARAM$input$dataset       <- "./datasets/competencia3_2022.csv.gz"
PARAM$input$training      <- c( 202103 )

PARAM$trainingstrategy$undersampling  <-  1.0   # un undersampling de 0.1  toma solo el 10% de los CONTINUA
PARAM$trainingstrategy$semilla_azar   <- 432557  #Aqui poner la propia semilla

PARAM$hyperparametertuning$iteraciones <- 500
PARAM$hyperparametertuning$xval_folds  <- 5
PARAM$hyperparametertuning$POS_ganancia  <- 78000
PARAM$hyperparametertuning$NEG_ganancia  <- -2000

PARAM$hyperparametertuning$semilla_azar  <- 892597  #Aqui poner la propia semilla, PUEDE ser distinta a la de trainingstrategy


PARAM$lag1  <- TRUE
PARAM$lag2  <- TRUE
PARAM$Tendencias  <- TRUE

#------------------------------------------------------------------------------
#graba a un archivo los componentes de lista
#para el primer registro, escribe antes los titulos

loguear  <- function( reg, arch=NA, folder="./exp/", ext=".txt", verbose=TRUE )
{
  archivo  <- arch
  if( is.na(arch) )  archivo  <- paste0(  folder, substitute( reg), ext )

  if( !file.exists( archivo ) )  #Escribo los titulos
  {
    linea  <- paste0( "fecha\t", 
                      paste( list.names(reg), collapse="\t" ), "\n" )

    cat( linea, file=archivo )
  }

  linea  <- paste0( format(Sys.time(), "%Y%m%d %H%M%S"),  "\t",     #la fecha y hora
                    gsub( ", ", "\t", toString( reg ) ),  "\n" )

  cat( linea, file=archivo, append=TRUE )  #grabo al archivo

  if( verbose )  cat( linea )   #imprimo por pantalla
}
#------------------------------------------------------------------------------
#esta funcion calcula internamente la ganancia de la prediccion probs

fganancia_logistic_lightgbm  <- function( probs, datos) 
{
  vpesos   <- get_field(datos, "weight")

  #vector de ganancias
  vgan  <- ifelse( vpesos == 1.0000002, PARAM$hyperparametertuning$POS_ganancia, 
                   ifelse( vpesos == 1.0000001, PARAM$hyperparametertuning$NEG_ganancia, 
                           PARAM$hyperparametertuning$NEG_ganancia / PARAM$trainingstrategy$undersampling ) )

  tbl  <- as.data.table( list( "vprobs" = probs, "vgan" = vgan ) )
  setorder( tbl,  -vprobs )
  ganancia <- tbl[ 1:GLOBAL_envios, sum( vgan ) ]

  return( list( "name"= "ganancia", 
                "value"=  ganancia,
                "higher_better"= TRUE ) )
}
#------------------------------------------------------------------------------
#esta funcion solo puede recibir los parametros que se estan optimizando
#el resto de los parametros se pasan como variables globales, la semilla del mal ...

EstimarGanancia_lightgbm  <- function( x )
{
  gc()  #libero memoria

  #llevo el registro de la iteracion por la que voy
  GLOBAL_iteracion  <<- GLOBAL_iteracion + 1

  #para usar en fganancia_logistic_lightgbm 
  GLOBAL_envios <<- as.integer(x$envios/PARAM$hyperparametertuning$xval_folds)   #asigno la variable global

  kfolds  <- PARAM$hyperparametertuning$xval_folds   # cantidad de folds para cross validation

  param_basicos  <- list( objective= "binary",
                          metric= "custom",
                          first_metric_only= TRUE,
                          boost_from_average= TRUE,
                          feature_pre_filter= FALSE,
                          verbosity= -100,
                          max_depth=  -1,         # -1 significa no limitar,  por ahora lo dejo fijo
                          min_gain_to_split= 0.0, #por ahora, lo dejo fijo
                          lambda_l1= 0.0,         #por ahora, lo dejo fijo
                          lambda_l2= 0.0,         #por ahora, lo dejo fijo
                          max_bin= 31,            #por ahora, lo dejo fijo
                          num_iterations= 9999,   #un numero muy grande, lo limita early_stopping_rounds
                          force_row_wise= TRUE,   #para que los alumnos no se atemoricen con tantos warning
                          seed= PARAM$hyperparametertuning$semilla_azar
                        )

  #el parametro discolo, que depende de otro
  param_variable  <- list(  early_stopping_rounds= as.integer(50 + 5/x$learning_rate) )

  param_completo  <- c( param_basicos, param_variable, x )

  set.seed( PARAM$hyperparametertuning$semilla_azar )
  modelocv  <- lgb.cv( data= dtrain,
                       eval= fganancia_logistic_lightgbm,
                       stratified= TRUE, #sobre el cross validation
                       nfold= kfolds,    #folds del cross validation
                       param= param_completo,
                       verbose= -100
                      )

  #obtengo la ganancia
  ganancia  <- unlist(modelocv$record_evals$valid$ganancia$eval)[ modelocv$best_iter ]

  ganancia_normalizada  <-  ganancia* kfolds     #normailizo la ganancia

  param_completo$num_iterations <- modelocv$best_iter  #asigno el mejor num_iterations
  param_completo["early_stopping_rounds"]  <- NULL     #elimino de la lista el componente  "early_stopping_rounds"
  
  #Voy registrando la importancia de variables
  if( ganancia_normalizada >  GLOBAL_gananciamax )
  {
    GLOBAL_gananciamax  <<- ganancia_normalizada
    modelo  <- lgb.train( data= dtrain,
                          param= param_completo,
                          verbose= -100
                         )

    tb_importancia  <- as.data.table( lgb.importance(modelo ) )
    archivo_importancia  <- paste0( "impo_", GLOBAL_iteracion,".txt")
    fwrite( tb_importancia,
            file= archivo_importancia,
            sep= "\t" )
  }


  #el lenguaje R permite asignarle ATRIBUTOS a cualquier variable
  attr(ganancia_normalizada ,"extras" )  <- list("num_iterations"= modelocv$best_iter)  #esta es la forma de devolver un parametro extra

  #logueo 
  xx  <- param_completo
  xx$ganancia  <- ganancia_normalizada   #le agrego la ganancia
  xx$iteracion <- GLOBAL_iteracion
  loguear( xx, arch= klog )

  return( ganancia_normalizada )
}
#------------------------------------------------------------------------------



#-----------------------------------------------------------------------------------------------------------------
#magia de gustavo


#------------------------------------------------------------------------------
#se calculan para los 6 meses previos el minimo, maximo y tendencia calculada con cuadrados minimos
#la formula de calculo de la tendencia puede verse en https://stats.libretexts.org/Bookshelves/Introductory_Statistics/Book%3A_Introductory_Statistics_(Shafer_and_Zhang)/10%3A_Correlation_and_Regression/10.04%3A_The_Least_Squares_Regression_Line
#para la maxíma velocidad esta funcion esta escrita en lenguaje C, y no en la porqueria de R o Python

cppFunction('NumericVector fhistC(NumericVector pcolumna, IntegerVector pdesde ) 
{
  /* Aqui se cargan los valores para la regresion */
  double  x[100] ;
  double  y[100] ;
  int n = pcolumna.size();
  NumericVector out( 5*n );
  for(int i = 0; i < n; i++)
  {
    //lag
    if( pdesde[i]-1 < i )  out[ i + 4*n ]  =  pcolumna[i-1] ;
    else                   out[ i + 4*n ]  =  NA_REAL ;
    int  libre    = 0 ;
    int  xvalor   = 1 ;
    for( int j= pdesde[i]-1;  j<=i; j++ )
    {
       double a = pcolumna[j] ;
       if( !R_IsNA( a ) ) 
       {
          y[ libre ]= a ;
          x[ libre ]= xvalor ;
          libre++ ;
       }
       xvalor++ ;
    }
    /* Si hay al menos dos valores */
    if( libre > 1 )
    {
      double  xsum  = x[0] ;
      double  ysum  = y[0] ;
      double  xysum = xsum * ysum ;
      double  xxsum = xsum * xsum ;
      double  vmin  = y[0] ;
      double  vmax  = y[0] ;
      for( int h=1; h<libre; h++)
      { 
        xsum  += x[h] ;
        ysum  += y[h] ; 
        xysum += x[h]*y[h] ;
        xxsum += x[h]*x[h] ;
        if( y[h] < vmin )  vmin = y[h] ;
        if( y[h] > vmax )  vmax = y[h] ;
      }
      out[ i ]  =  (libre*xysum - xsum*ysum)/(libre*xxsum -xsum*xsum) ;
      out[ i + n ]    =  vmin ;
      out[ i + 2*n ]  =  vmax ;
      out[ i + 3*n ]  =  ysum / libre ;
    }
    else
    {
      out[ i       ]  =  NA_REAL ; 
      out[ i + n   ]  =  NA_REAL ;
      out[ i + 2*n ]  =  NA_REAL ;
      out[ i + 3*n ]  =  NA_REAL ;
    }
  }
  return  out;
}')

#------------------------------------------------------------------------------
#calcula la tendencia de las variables cols de los ultimos 6 meses
#la tendencia es la pendiente de la recta que ajusta por cuadrados minimos
#La funcionalidad de ratioavg es autoria de  Daiana Sparta,  UAustral  2021

TendenciaYmuchomas  <- function( dataset, cols, ventana=6, tendencia=TRUE, minimo=TRUE, maximo=TRUE, promedio=TRUE, 
                                 ratioavg=FALSE, ratiomax=FALSE)
{
  gc()
  #Esta es la cantidad de meses que utilizo para la historia
  ventana_regresion  <- ventana
  
  last  <- nrow( dataset )
  
  #creo el vector_desde que indica cada ventana
  #de esta forma se acelera el procesamiento ya que lo hago una sola vez
  vector_ids   <- dataset$numero_de_cliente
  
  vector_desde  <- seq( -ventana_regresion+2,  nrow(dataset)-ventana_regresion+1 )
  vector_desde[ 1:ventana_regresion ]  <-  1
  
  for( i in 2:last )  if( vector_ids[ i-1 ] !=  vector_ids[ i ] ) {  vector_desde[i] <-  i }
  for( i in 2:last )  if( vector_desde[i] < vector_desde[i-1] )  {  vector_desde[i] <-  vector_desde[i-1] }
  
  for(  campo  in   cols )
  {
    nueva_col     <- fhistC( dataset[ , get(campo) ], vector_desde ) 
    
    if(tendencia)  dataset[ , paste0( campo, "_tend", ventana) := nueva_col[ (0*last +1):(1*last) ]  ]
    if(minimo)     dataset[ , paste0( campo, "_min", ventana)  := nueva_col[ (1*last +1):(2*last) ]  ]
    if(maximo)     dataset[ , paste0( campo, "_max", ventana)  := nueva_col[ (2*last +1):(3*last) ]  ]
    if(promedio)   dataset[ , paste0( campo, "_avg", ventana)  := nueva_col[ (3*last +1):(4*last) ]  ]
    if(ratioavg)   dataset[ , paste0( campo, "_ratioavg", ventana)  := get(campo) /nueva_col[ (3*last +1):(4*last) ]  ]
    if(ratiomax)   dataset[ , paste0( campo, "_ratiomax", ventana)  := get(campo) /nueva_col[ (2*last +1):(3*last) ]  ]
  }
  
}







#-------------------------------------------




#------------------------------------------------------------------------------
#Aqui empieza el programa

#Aqui se debe poner la carpeta de la computadora local
#setwd("~/buckets/b1/")   #Establezco el Working Directory
setwd("C:\\Users\\Marie\\Documents\\MasterUBA\\DMEyF")
#cargo el dataset donde voy a entrenar el modelo
dataset  <- fread( PARAM$input$dataset, stringsAsFactors= TRUE)

print(dim(dataset))

#ayudo trabajando con menor datos
dataset <- dataset[dataset$foto_mes %in% c( "202009", "202010", "202011", "202012", "202101", "202102", "202103", "202104", "202105"), ]

print(dim(dataset))

setorderv( dataset, c("numero_de_cliente","foto_mes") ) #ordeno

#---------------------------------------------------



#creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( paste0( "./exp/", PARAM$experimento, "/"), showWarnings = FALSE )
setwd( paste0( "./exp/", PARAM$experimento, "/") )   #Establezco el Working Directory DEL EXPERIMENTO



#en estos archivos quedan los resultados
kbayesiana  <- paste0( PARAM$experimento, ".RDATA" )
klog        <- paste0( PARAM$experimento, ".txt" )


GLOBAL_iteracion  <- 0   #inicializo la variable global
GLOBAL_gananciamax <- -1 #inicializo la variable global

#si ya existe el archivo log, traigo hasta donde llegue
if( file.exists(klog) )
{
  tabla_log  <- fread( klog )
  GLOBAL_iteracion  <- nrow( tabla_log )
  GLOBAL_gananciamax  <- tabla_log[ , max( ganancia ) ]
}



#--------------------------------------
#Aplicando la magia
#estas son las columnas a las que se puede agregar lags o media moviles ( todas menos las obvias )
cols_lagueables  <- copy(  setdiff( colnames(dataset), c("numero_de_cliente", "foto_mes", "clase_ternaria")  ) )

#ordeno el dataset por <numero_de_cliente, foto_mes> para poder hacer lags
#  es MUY  importante esta linea
setorder( dataset, numero_de_cliente, foto_mes )




if( PARAM$lag1 )
{
  #creo los campos lags de orden 1
  dataset[ , paste0( cols_lagueables, "_lag1") := shift(.SD, 1, NA, "lag"), 
           by= numero_de_cliente, 
           .SDcols= cols_lagueables ]
  
  #agrego los delta lags de orden 1
  for( vcol in cols_lagueables )
  {
    dataset[ , paste0(vcol, "_delta1") := get(vcol)  - get(paste0( vcol, "_lag1"))  ]
  }
}


if( PARAM$lag2 )
{
  #creo los campos lags de orden 2
  dataset[ , paste0( cols_lagueables, "_lag2") := shift(.SD, 2, NA, "lag"), 
           by= numero_de_cliente, 
           .SDcols= cols_lagueables ]
  
  #agrego los delta lags de orden 2
  for( vcol in cols_lagueables )
  {
    dataset[ , paste0(vcol, "_delta2") := get(vcol)  - get(paste0( vcol, "_lag2"))  ]
  }
}


#--------------------------------------
#agrego las tendencias

if( PARAM$Tendencias )
{
  TendenciaYmuchomas( dataset, 
                      cols= cols_lagueables,
                      ventana=   3,      # 6 meses de historia
                      tendencia= TRUE,
                      minimo=    TRUE,
                      maximo=    TRUE,
                      promedio=  TRUE,
                      ratioavg=  TRUE,
                      ratiomax=  TRUE  )
}


if( PARAM$Tendencias )
{
  TendenciaYmuchomas( dataset, 
                      cols= cols_lagueables,
                      ventana=   3,      # 6 meses de historia
                      tendencia= TRUE,
                      minimo=    TRUE,
                      maximo=    TRUE,
                      promedio=  TRUE,
                      ratioavg=  FALSE,
                      ratiomax=  FALSE  )
}

if( PARAM$Tendencias )
{
  TendenciaYmuchomas( dataset, 
                      cols= cols_lagueables,
                      ventana=   6,      # 6 meses de historia
                      tendencia= TRUE,
                      minimo=    FALSE,
                      maximo=    FALSE,
                      promedio=  TRUE,
                      ratioavg=  FALSE,
                      ratiomax=  FALSE  )
}


print(dim(dataset))

#grabo el dataset
fwrite( dataset,
        "dataset_fe.csv.gz",
        logical01= TRUE,
        sep= "," )

#paso la clase a binaria que tome valores {0,1}  enteros
dataset[ foto_mes %in% PARAM$input$training, clase01 := ifelse( clase_ternaria=="CONTINUA", 0L, 1L) ]


#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01", "azar", "training","numero_de_cliente"  ) ) #le quita estos campos

set.seed( PARAM$trainingstrategy$semilla_azar )
dataset[  , azar := runif( nrow( dataset ) ) ] #agrego columna random para undersamplin, en este caso lo lo uso
dataset[  , training := 0L ] # marca las muestras a usar (tambien del undersampling)
dataset[ foto_mes %in% PARAM$input$training & 
          ( azar <= PARAM$trainingstrategy$undersampling | clase_ternaria %in% c( "BAJA+1", "BAJA+2" ) ),
         training := 1L ]

#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[ training == 1L, campos_buenos, with=FALSE]),
                        label= dataset[ training == 1L, clase01 ],
                        weight=  dataset[ training == 1L, ifelse( clase_ternaria=="BAJA+2", 1.0000002, ifelse( clase_ternaria=="BAJA+1",  1.0000001, 1.0) )],
                        free_raw_data= FALSE  )



#Aqui comienza la configuracion de la Bayesian Optimization
funcion_optimizar  <- EstimarGanancia_lightgbm   #la funcion que voy a maximizar

configureMlr( show.learner.output= FALSE)

#configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
#por favor, no desesperarse por lo complejo
obj.fun  <- makeSingleObjectiveFunction(
              fn=       funcion_optimizar, #la funcion que voy a maximizar
              minimize= FALSE,   #estoy Maximizando la ganancia
              noisy=    TRUE,
              par.set=  hs,     #definido al comienzo del programa
              has.simple.signature = FALSE   #paso los parametros en una lista
             )

ctrl  <- makeMBOControl( save.on.disk.at.time= 600,  save.file.path= kbayesiana)  #se graba cada 600 segundos
ctrl  <- setMBOControlTermination(ctrl, iters= PARAM$hyperparametertuning$iteraciones )   #cantidad de iteraciones
ctrl  <- setMBOControlInfill(ctrl, crit= makeMBOInfillCritEI() )

#establezco la funcion que busca el maximo
surr.km  <- makeLearner("regr.km", predict.type= "se", covtype= "matern3_2", control= list(trace= TRUE))

#inicio la optimizacion bayesiana
if( !file.exists( kbayesiana ) ) {
  run  <- mbo(obj.fun, learner= surr.km, control= ctrl)
} else {
  run  <- mboContinue( kbayesiana )   #retomo en caso que ya exista
}


quit( save="no" )

