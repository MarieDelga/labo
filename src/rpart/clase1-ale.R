#Arbol elemental con libreria  rpart
#Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot

#cargo las librerias que necesito
install.packages( "data.table", dependencies=TRUE )
install.packages("rpart")
install.packages("rpart.plot")
install.packages("tidyverse")
install.packages("caret", dependencies = TRUE)
install.packages("ggplot2")
install.packages("colorspace")
install.packages("geometry")

require("data.table")
require("rpart")
require("rpart.plot")


library(data.table)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(tidyverse)
library(caret)

#Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("C:\\Users\\Marie\\Documents\\MasterUBA\\DMEyF")  #Establezco el Working Directory

#cargo el dataset
dataset  <- fread("./datasets/competencia1_2022.csv")

dtrain  <- dataset[ foto_mes==202101 ]  #defino donde voy a entrenar
dapply  <- dataset[ foto_mes==202103 ]  #defino donde voy a aplicar el modelo

#genero el modelo,  aqui se construye el arbol
modelo  <- rpart(formula=   "clase_ternaria ~ .",  #quiero predecir clase_ternaria a partir de el resto de las variables
                 data=      dtrain,  #los datos donde voy a entrenar
                 xval=      0,
                 cp=       -0.3,   #esto significa no limitar la complejidad de los splits
                 minsplit=  0,     #minima cantidad de registros para que se haga el split
                 minbucket= 5,     #cantidad de elementos minimo de una hoja
                 maxdepth=  4 )  #profundidad maxima del arbol

#al cortar en mas de 2 aumenta mucho la complejidad computacional. aumenta la ganancia, pero se puede imitar con mayor profundidad.
#combiene cuando son variables categoricas??

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
dir.create( "./exp/KA2002" )


fwrite( dapply[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
        file= "./exp/KA2002/K102_001.csv",
        sep=  "," )

# La siguiente función devuelve todas las hojas (nodos terminales) en una tabla
# para poder analizar mejor nuestro árbol.
tablahojas <- function(modelo, datos, target = "clase_ternaria") {
  # Tomamos la columna con el target
  target_vector <- datos[, get(target)]
  # Tomamos las clases de nuestro target
  classes <- unique(target_vector)
  # Tomamos las posicion de las hojas que aplican a los registro de nuestro ds
  row_leaf <- unique(modelo$where)
  leaves <- data.table(row_frame = row_leaf)
  setkey(leaves,row_frame)
  # Relacion target ~ hojas
  leaves_target <- dcast(
    data.table(
      target = target_vector,
      leaf = modelo$where),
    leaf ~ target, length,
    value.var = "target")
  setkey(leaves_target, leaf)
  # Juntamos todo
  leaves_target <- leaves_target[leaves, nomatch = 0]
  # Sumamos algunas columnas calculadas
  colnames(leaves_target[, classes, with = FALSE])[apply(
    leaves_target[, classes, with = FALSE], 1, which.max)]
  # Clase mayoritaria
  leaves_target[, y := colnames(
    leaves_target[, classes, with = FALSE]
  )[apply(leaves_target[, classes, with = FALSE],
          1, which.max)]]
  # Cantidad de elementos de la hoja
  leaves_target[, TOTAL := unlist(Reduce(function(a, b) Map(`+`, a, b), .SD)),
                .SDcols = classes]
  leaves_target
}

# Ejecutamos la función sobre nuestro modelo, con nuestros datos
hojas <- tablahojas(modelo, dtrain)
View(hojas)

## Preguntas
## - ¿Con qué criterio eligió la clase de cada hoja que determino la
##   clasificación de los registros?
## - ¿Cuántas hojas con BAJAS+2 hay?


## ---------------------------
## Step 3: Calculando la ganancia de cada hoja
## ---------------------------

# Agregamos un nuevo campo de nombre ganancia
hojas[, ganancia := `BAJA+2` * 78000 - 2000 * (CONTINUA + `BAJA+1`)]
View(hojas)

## Pregunta
## - ¿Cuantás hojas que no son BAJA+2 tienen aún así ganancia positiva?

## ---------------------------
## Step 4: Sumarizando el envío
## ---------------------------

View(hojas[ganancia > 0, .(
  ganancia = sum(ganancia),
  enviados = sum(TOTAL),
  sevan = sum(`BAJA+2`))])

#cuanto mejora el modelo vs el azar, se llama lift. un lift de 10

## Preguntas
## Si enviaramos todos los casos de las hojas con ganancia positiva
## - ¿Cuánta ganancia tendríamos?
## - ¿Cuánta personas estimularíamos?
## - ¿A cuántas personas acertaríamos?

## ---------------------------
## Step 5: Binarizando la salida (en tu cara RAE)
## ---------------------------

# Creamos un nuevo target binario porque es mas facil medir metricas binarias que terciarias
dtrain[, clase_binaria := ifelse(
  clase_ternaria == "BAJA+2",
  "evento",
  "noevento"
)]
# Borramos el target viejo
dtrain[, clase_ternaria := NULL]

arbolbinario <- rpart("clase_binaria ~ .",
                      data =      dtrain,
                      xval =      0,
                      cp =       -0.3,
                      minsplit =  0,
                      minbucket = 5, #nodos con pocos elementos
                      maxdepth =  4)
# Transformamos las hojas a una tabla
hojasbinario <- tablahojas(arbolbinario, dtrain, "clase_binaria")

# Y agregamos la ganancia de cada hoja
hojasbinario[, ganancia := evento * 78000 - 2000 * noevento]
View(hojasbinario)
# Por último sumarizamos
View(hojasbinario[ganancia > 0,
                  .(ganancia = sum(ganancia), enviados = sum(TOTAL), sevan = sum(evento))])

## Pregunta
## - ¿Considera que la agrupación de clases fue positiva para la  ganancia?
#jugar con las clases ayuda a los modelos
#solo se mantiene puro el universo y la metrica

# ---------------------------
## Step 6: Salida probabilísticas
## ---------------------------

# Calculamos la probabilidad de evento en cada hoja
hojasbinario[, p_evento := evento / (evento + noevento)]
View(hojasbinario)
# Ordenamos de forma descendiente las probabilidades, ya que nos interesan
# ante todo las probabilidades más altas
hojasordenadas <- hojasbinario[order(-p_evento),]

# Calculamos la ganancia acumulada, desde con la probabilidad desde la primera
# fila con probabilidad más alta hasta la fila N, para cada fila.
hojasordenadas[, gan_acum := cumsum(ganancia)]

View(hojasordenadas)

# TAREAS:
# - Calculé la probabilidad de NO evento
# - Puede pasar que dos hojas tengan la misma probabilidad, escriba una query
#   que las agrupe.

## Preguntas
## - ¿Cómo ve la relación entre la probabilidad ordenada y la hojas con
##   ganancia?
## - ¿Cuál es la máxima ganancia posible es nuestro árbol?
## - ¿Cuál es el `punto de corte` que sugiere?
## - ¿Por qué es distinto al teórico?
## - ¿Es nuestro `punto de corte` es igual de útil?

## ---------------------------
## Step 7: Graficando la ganancia
## ---------------------------

ggplot(hojasordenadas, aes(x = p_evento ,y = gan_acum)) +
  scale_x_reverse() +
  geom_line(size = 1)

## Pregunta
## ¿Cómo interpretamos este gráfico?

## ---------------------------
## Step 8: No todo es plata en la vida
## ---------------------------

## NOTA:
## Existen más formas de medir la calidad del modelo a través de las
## probabilidades que nos entrega. A nivel global podemos usar `AUC`: área bajo
## la curva ROC:https://en.wikipedia.org/wiki/Receiver_operating_characteristic
## que nos muestra el comportamiento global de la performance del modelo.
##
## Para la **curva ROC** vamos a necesitar construir una Matriz de confusión
## https://en.wikipedia.org/wiki/Confusion_matrix#Table_of_confusion por cada
## punto de corte posible.

# Vamos a sumar las variables `tp`, `tn`, `fp` y `fn`
hojasordenadas[, c("evento_acum","noevento_acum") :=
                 list(cumsum(evento),cumsum(noevento))]
total_evento <- hojasordenadas[, sum(evento)]
total_noevento <- hojasordenadas[, sum(noevento)]
hojasordenadas[, c("evento_restantes", "noevento_restantes") :=
                 list(total_evento - evento_acum, total_noevento - noevento_acum)]

hojasordenadas[, tp := evento_acum]
hojasordenadas[, tn := noevento_restantes]
hojasordenadas[, fp := noevento_acum]
hojasordenadas[, fn := evento_restantes]

# Para validar los cálculos anteriores vamos a visualizar solo los campos
# importantes
View(hojasordenadas[, .(p_evento, evento, noevento, tp, tn, fp, fn)])

## ---------------------------
## Step 9: Armando nuestra curva ROC
## ---------------------------

# Calculamos las variables necesarios para la curva ROC
hojasordenadas[, tpr := (tp / (tp + fn))]
hojasordenadas[, fpr := (fp / (fp + tn))]

# La graficamos
ggplot(hojasordenadas, aes(x = fpr, y = tpr)) +
  # Agregamos la función identidad
  geom_abline(intercept = 0, slope = 1) +
  geom_line(lwd = 1)

## Pregunta
## ¿Qué representa la curva ROC?

## ---------------------------
## Step 10: Calculando el área bajo la curva
## ---------------------------

## NOTA: Como es muy complejo reflejar en palabras una curva, se suele calcular
## el área bajo su curva (auc) y reflejar ese valor como métrica de la
## calidad del modelo.

# Calculamos su área, necesita instalar el siguiente paquete
# install.packages("geometry")
require("geometry")

x <- c(hojasordenadas$fpr,1)
y <- c(hojasordenadas$tpr, 0)
# El valor de la auc
View(polyarea(x, y))


## Preguntas
## -¿AUC es una métrica global o local?
## -¿Pueden dos curvas distintas tener un mismo valor de AUC?


## ---------------------------
## Step 11: No limitarnos a la ROC
## ---------------------------

# Podemos construir una curva para el accuraccy
hojasordenadas[, acc := ((tp + tn) / (tp + tn + fp + fn))]

# Y graficarla
ggplot(hojasordenadas, aes(x = p_evento, y = acc)) +
  geom_line(lwd = 1)

## Preguntas
## - ¿Se ajusta esta curva a nuestra necesidad de negocio?
## -¿Cuál es el threshold optimo según la curva de accuracy?
## - Si hubiéramos elegido nuestro modelo usando el accuracy, ¿Cuanta plata
##   hubiera ganado o perdido la empresa?
## - ¿Es necesario que la salida del modelo sea un probabilidad para aplicar
##   estos conceptos?

## TAREA:
## - Construya la curva correspondiente al F1 Score.
## - La métrica F1, es criticado por dar un mismo peso a recall y al precision.
##   Por esto mismo, a alguien se le ocurrió el F-Beta. Construya esta última
##   para varios Betas.
## - ¿Hay algún Beta que tenga un **punto de corte** similar al nuestro?


# Podemos construir una curva para el accuraccy
hojasordenadas[, precision :=  ((tp)/(tp + fp ))  ]
hojasordenadas[, recall := (tp/(tp+fn)) ]
hojasordenadas[, F1 := (2*precision*recall)/(precision+recall) ]

# Y graficarla
ggplot(hojasordenadas, aes(x = p_evento, y = F1)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_line(lwd = 1)

hojasordenadas[, Fbp5 := ((1+0.5)/( (0.5*0.5/recall)+ (1/precision))) ]
hojasordenadas[, Fbp3 := ((1+0.3)/( (0.3*0.3/recall)+ (1/precision))) ]
hojasordenadas[, Fbp1 := ((1+0.1)/( (0.1*0.1/recall)+ (1/precision)))  ]
hojasordenadas[, Fbp7 := ((1+0.7)/( (0.7*0.7/recall)+ (1/precision))) ]
hojasordenadas[, Fbp9 := ((1+0.9)/( (0.9*0.9/recall)+ (1/precision))) ]


ggplot(hojasordenadas, aes(x = p_evento, y = Fbp5)) +
  ggplot(hojasordenadas, aes(x = p_evento, y = Fbp3)) +
  ggplot(hojasordenadas, aes(x = p_evento, y = Fbp1)) +
  ggplot(hojasordenadas, aes(x = p_evento, y = Fbp7)) +
  ggplot(hojasordenadas, aes(x = p_evento, y = Fbp9)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_line(lwd = 1)



## Sobre el Azar
##
## ---------------------------
## Step 1: El simple y viejo Train / Test
## ---------------------------
##
## If you torture the data long enough, it will confess.
## --- Ronald Coase
##

# Limpiamos el entorno
rm(list = ls())
gc(verbose = FALSE)

# Librerías necesarias
require("data.table")
require("rpart")
require("ROCR")
require("ggplot2")


# Poner la carpeta de la materia de SU computadora local
setwd("C:\\Users\\Marie\\Documents\\MasterUBA\\DMEyF")  #Establezco el Working Directory

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

# Seteamos nuestra primera semilla
set.seed(semillas[1])

# Particionamos de forma estratificada
in_training <- caret::createDataPartition(dataset$clase_binaria,
                                          p = 0.70, list = FALSE)
dtrain  <-  dataset[in_training, ]
dtest   <-  dataset[-in_training, ]

## Preguntas
## - ¿Por qué separamos en train/test?
## - Son números aleatorios los que nos dan las computadoras
## - ¿Por qué usamos semillas?
## - ¿Qué es una partición estratificada?

## TAREA:
## - Comparar la distribución del target de una partición estratificada en
##   nuestro dataset con una que no la sea.
## - ¿Tiene realemente alguna ventaja la partición estratificada ?

## ---------------------------
## Step 2: Armando el primer modelo particionado
## ---------------------------

# Medimos cuanto tarda nuestro modelo en ajustar
start_time <- Sys.time()
modelo <- rpart(clase_binaria ~ .,
                data = dtrain,
                xval = 0,
                cp = 0,
                minsplit = 20,
                minbucket = 1,
                maxdepth = 5)
print(Sys.time() - start_time)

pred_training <- predict(modelo, dtrain, type = "prob")
pred_testing <- predict(modelo, dtest, type = "prob")




## Preguntas:
## - ¿Qué tan importante mirar las métricas de train?

## ---------------------------
## Step 3: Mirando la ganancia
## ---------------------------

# Armamos una función que nos calcule la ganancia, usando el punto de corte de
# 0.025
ganancia <- function(probabilidades, clase) {
  return(sum(
    (probabilidades >= 0.025) * ifelse(clase == "evento", 78000, -2000))
  )
}

# La ganancia en testing NORMALIZADA
print(ganancia(pred_testing[, "evento"], dtest$clase_binaria) / 0.3)

## Activida:
## Comparta en Zulip el número que le dio de ganancia y cuando error estima que
## puede haber con el resto de sus compañeros
## Ejemplo: 18000000, 1000000



## ---------------------------
## Step 4: Probando más muchas más semillas
## ---------------------------

# Almacenaremos los resultados en una tabla
resultados_n_gan <- c()

# Calcule en función del tiempo de ejecución anterior, cuantos árboles puede
# hacer en 5 minutos y ponga ese número en la siguiente variable
n <- 100

set.seed(semillas[1])
t0 <- Sys.time()
for (i in 1:n) {
  
  in_training <- caret::createDataPartition(dataset[, get("clase_binaria")],
                                            p = 0.70, list = FALSE)
  dtrain  <-  dataset[in_training, ]
  dtest   <-  dataset[-in_training, ]
  
  modelo <- rpart(clase_binaria ~ .,
                  data = dtrain,
                  xval = 0,
                  cp = 0,
                  minsplit = 20,
                  minbucket = 1,
                  maxdepth = 5)
  
  pred_testing <- predict(modelo, dtest, type = "prob")
  
  gan <- ganancia(pred_testing[, "evento"], dtest$clase_binaria) / 0.3
  
  resultados_n_gan <- c(resultados_n_gan, gan)
}
print(Sys.time() - t0)

# La menor ganancia conseguida en test
print(min(resultados_n_gan))

# La mayor ganancia
print(max(resultados_n_gan))

# La media de la ganancia
print(mean(resultados_n_gan))

# Veamos la dispersión de la ganancia
ggplot() + aes(resultados_n_gan) + geom_density()

## Preguntas:
## ¿Cree que puede cambiar mucho la ganancia en **test** para dos semillas
## distintas?

cantidad_arboles <- 5

resultados_n_mcv <- c()
set.seed(semillas[1])
for (i in 1:50) {
  resultados_n_mcv <- c(resultados_n_mcv,
                        mean(resultados_n_gan[sample(n, cantidad_arboles)]))
}

# La menor ganancia conseguida en test
print(min(resultados_n_mcv))

# La mayor ganancia
print(max(resultados_n_mcv))

# La media de la ganancia
print(mean(resultados_n_mcv))

# Veamos la dispersión de la ganancia
ggplot() + aes(resultados_n_mcv) + geom_density()

## NOTA: Esta técnica es conocida como Montecarlo Cross Validation
##
## Preguntas
## - ¿Qué efecto observa cuando se toma como medición el promedio de 5 árboles?
## - ¿Desapareció el error?
## - ¿Si se hubieran tomado más valores que efectos esperaría?
## - ¿Que ventaja y desventaja ve en esta técnica comparada al Cross Validation?



## ---------------------------
## Step 7: Midiendo nuestras semillas
## ---------------------------

resultados_mis_semillas <- c()

t0 <- Sys.time()
for (s in semillas) {
  set.seed(s)
  in_training <- caret::createDataPartition(dataset[, get("clase_binaria")],
                                            p = 0.70, list = FALSE)
  dtrain  <-  dataset[in_training, ]
  dtest   <-  dataset[-in_training, ]
  
  modelo <- rpart(clase_binaria ~ .,
                  data = dtrain,
                  xval = 0,
                  cp = 0,
                  minsplit = 20,
                  minbucket = 1,
                  maxdepth = 5)
  
  pred_testing <- predict(modelo, dtest, type = "prob")
  
  gan <- ganancia(pred_testing[, "evento"], dtest$clase_binaria) / 0.3
  
  resultados_mis_semillas <- c(resultados_mis_semillas, gan)
  
}
print(Sys.time() - t0)

print(mean(resultados_mis_semillas))

## Preguntas
## - ¿Cuán lejos se encontró la media de sus semillas respecto a los resultados
##    anteriores?
## - ¿Usaría semillas que le den un valor promedio más alto?
## - ¿Usaría más semillas?
## - ¿Que ventaja y desventaja ve en usar más semillas?










## ---------------------------
## Step 8: Buscando un mejor modelo
## ---------------------------

resultados_grid_search <- data.table()

# Complete los valores que se van a combinar para cada parámetro a explorar
t0 <- Sys.time()
for (cp in c(-1, 0.1, 0.05, 0.01)) {
  for (md in c(5, 10, 30)) {
    for (ms in c(1,5, 10, 30, 50)) {
      for (mb in c(1, as.integer(ms / 2))) {
        
        t0 <- Sys.time()
        gan_semillas <- c()
        for (s in semillas) {
          set.seed(s)
          in_training <- caret::createDataPartition(dataset[,
                                                            get("clase_binaria")],
                                                    p = 0.70, list = FALSE)
          dtrain  <-  dataset[in_training, ]
          dtest   <-  dataset[-in_training, ]
          
          modelo <- rpart(clase_binaria ~ .,
                          data = dtrain,
                          xval = 0,
                          cp = cp,
                          minsplit = ms,
                          minbucket = mb,
                          maxdepth = md)
          
          pred_testing <- predict(modelo, dtest, type = "prob")
          gan <- ganancia(pred_testing[, "evento"], dtest$clase_binaria) / 0.3
          
          gan_semillas <- c(gan_semillas, gan)
        }
        tiempo <-  as.numeric(Sys.time() - t0, units = "secs")
        
        resultados_grid_search <- rbindlist(list(
          resultados_grid_search,
          data.table(
            tiempo = tiempo,
            cp = cp,
            mb = mb,
            ms = ms,
            md = md,
            gan = mean(gan_semillas))
        ))
      }
    }
  }
}
print(Sys.time() - t0)
# Visualizo los parámetros de los mejores parámetros
View(resultados_grid_search[gan == max(gan), ])

# cp -1  mb 25  ms 50  md 5  gan 19856000



## TAREA:
## Una vez que tenga sus mejores parámetros, haga una copia del script
## rpart/z101_PrimerModelo.R, cambie los parámetros dentro del script,
## ejecutelo y suba a Kaggle su modelo.

## Preguntas
## - ¿Cuál es la diferencia entre **test** y **validation**?
## - ¿Cuántas veces podemos usar el conjunto de **test** sin
##   convertirlo en **validation**?
##
## La GRAN pregunta:
## - ¿Qué otra cosita de la materia tiene una partición 70 / 30?
## - Todo lo que hemos visto ¿Va a afectar a esa cosita?



# Complete los valores que se van a combinar para cada parámetro a explorar
t0 <- Sys.time()
for (cp in c(-1, 0.99, 0.95, 0.9, 0.85, 0.8, 0.75)) {
  for (md in c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)) {
    for (ms in c(1,5, 10,15,20,25, 30,35,40,45, 50,55,60,65,70,75,80,85,90)) {
      for (mb in c(1, as.integer(ms / 2),as.integer(ms / 3),as.integer(ms / 4),as.integer(ms / 5))) {
        
        t0 <- Sys.time()
        gan_semillas <- c()
        for (s in semillas) {
          set.seed(s)
          in_training <- caret::createDataPartition(dataset[,
                                                            get("clase_binaria")],
                                                    p = 0.70, list = FALSE)
          dtrain  <-  dataset[in_training, ]
          dtest   <-  dataset[-in_training, ]
          
          modelo <- rpart(clase_binaria ~ .,
                          data = dtrain,
                          xval = 0,
                          cp = cp,
                          minsplit = ms,
                          minbucket = mb,
                          maxdepth = md)
          
          pred_testing <- predict(modelo, dtest, type = "prob")
          gan <- ganancia(pred_testing[, "evento"], dtest$clase_binaria) / 0.3
          
          gan_semillas <- c(gan_semillas, gan)
        }
        tiempo <-  as.numeric(Sys.time() - t0, units = "secs")
        
        resultados_grid_search <- rbindlist(list(
          resultados_grid_search,
          data.table(
            tiempo = tiempo,
            cp = cp,
            mb = mb,
            ms = ms,
            md = md,
            gan = mean(gan_semillas))
        ))
      }
    }
  }
}
print(Sys.time() - t0)


View(resultados_grid_search)

# Visualizo los parámetros de los mejores parámetros
View(resultados_grid_search[gan == max(gan), ])

