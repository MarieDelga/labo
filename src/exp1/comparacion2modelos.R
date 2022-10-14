

#si corro los archivos finales sin borrar memoria



confe <- data.frame(
  gan = resultados_confe,
  type= "confe")

sinfe <- data.frame(
  gan = resultados_nofe,
  type= "sinfe")

final =rbind(confe, sinfe)

p<-ggplot(final, aes(x=gan , color=type)) +
  geom_density()

p


#si borré memoria

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection



PARAM <- list()
PARAM$experimento  <- "KA7242"

setwd("~/buckets/b1/")   #Establezco el Working Directory


time_confe  <- fread( "./exp/KA7242/time_confe.csv" )
print("tiempo de test con fe: ")
print(time_confe[1])


impo_confe  <- fread( "./exp/KA7242/impoconfe.csv" )
print("importancia variables con fe: ")
print(impo_confe)

resultados_confe  <- fread( "./exp/KA7242/resultados_confe.csv" )

time_nofe  <- fread( "./exp/KA7242/time_nofe.csv" )
print("tiempo de test sin fe: ")
print(time_nofe)
impo_nofe  <- fread( "./exp/KA7242/imponofe.csv" )
print("importancia variables sin fe: ")
print(impo_nofe)
resultados_nofe  <- fread( "./exp/KA7242/resultados_nofe.csv" )

confe <- data.frame(
  gan = resultados_confe,
  type= "confe")

sinfe <- data.frame(
  gan = resultados_nofe,
  type= "sinfe")

final =rbind(confe, sinfe)

p<-ggplot(final, aes(x=V1 , color=type)) +
  geom_density()

p