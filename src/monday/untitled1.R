
#chekeo punto de corte

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

# Nos quedamos solo con el 202101
dapply  <- dataset[ foto_mes==202103 ]
dapply_train = dapply
dataset <- dataset[foto_mes == 202101]



modelo <- rpart(clase_ternaria ~ ., data = dataset,
                xval = 0,
                cp = -1,
                minsplit = 1592,
                minbucket = 1592*0.189,
                maxdepth = 13)

# Vamos a probar que tal anda un modelo con las clases combinadas 
set.seed(semillas[1])
in_training <- caret::createDataPartition(dataset$clase_ternaria,
                                          p = 0.70, list = FALSE)
dtrain  <-  dataset[in_training, ]
dtest   <-  dataset[-in_training, ]






# funcion ganancia
ganancia <- function(probabilidades, clase) {
  return(sum(
    (probabilidades >= 0.025) * ifelse(clase == "BAJA+2", 78000, -2000))
  )
}

# La siguiente función devuelve todas las hojas (nodos terminales) en una tabla
# sobre un modelo. Es una "mejora" sobre la función dada en el script z201:
# - El target ahora debe ser binario: evento/noevento
# - Calcula la ganancia en base
# - Ordena de mayor a menor ganancia
# - Agrega un prefijo para poder juntar diferentes resultados.

# hoja train/test(continua baja+1 baja+2 evento noevento prob ganancia)
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

# Examinamos las nuevas hojas de nuestro modelo para entender las nuevas
# probabilidad. Primero sobre TRAIN

train_bin2 <- leaves_table(modelo, dtrain, "clase_ternaria")
print(train_bin2)

## Preguntas
## ¿Sigue siendo el punto de corte optimo 0.025?
## ¿Dejamos plata sobre la mesa?

## ---------------------------
## Step 4: Contando la plata que nos dejamos
## ---------------------------

train_bin2[, gan_acum := cumsum(gan) / 0.7]
train_bin2[, n_acum := cumsum(n) / 0.7]

print(train_bin2)

# La ganancia en train para el punto de corte de 0.025 es
train_bin2[p >= 0.05, sum(gan) / 0.7]

# Podemos buscar el punto de corte optimo con un par de sentencias de R
pos_max_gan <- which.max(train_bin2$gan_acum)
# La ganancia máxima
train_bin2[pos_max_gan, gan_acum]

# La probabilidad que da esa ganancia
train_bin2[pos_max_gan, p]

# La cantidad de envíos normalizados para esa ganancia 
train_bin2[pos_max_gan, n_acum / 0.7]

## Preguntas
## ¿Es útil con su semilla este mezcla de clases?
## ¿Qué desafíos ve en este punto?
