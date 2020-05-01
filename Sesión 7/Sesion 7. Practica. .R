# Laboratorio 7. 
# Juvenal Campos. 

# Librerias ----
pacman::p_load(moderndive, tidyverse)

# Primer ejercicio: ----
## Prueba de hipotesis de diferencia de medias. 

## Juvenal y Sebastián van a revisar el segundo examen parcial de la materia de estadística del CIDE para que los alumnos sepan su calificación antes de su tercer exámen, y, de esta manera, le hechen ganas al estudio si van muy mal. 

## Sin embargo, los alumnos tienen miedo de que los califique Juvenal, dado que sienten que el es más exigente a la hora de calificar que el profe Sebastian. Dados los siguientes datos obtenidos, ¿podríamos comprobar si esto es cierto o falso?

## El nivel de confianza a utilizar va a ser del 95%.

## Definicion de la hipotesis

# Hipotesis nula: 
## H_0: Los profesores califican igual 
## (mediaJuvenal = mediaSebastian) 

# Hipotesis alternativas: 

## H_a: El profesor Sebastian califica diferente que Juvenal 
## (mediaSebastian) != (mediaJuvenal)
## PRUEBA DE DOS COLAS! PORQUE CHECA SI HAY DIFERENCIA POR ARRIBA Y POR ABAJO. ES MAAAAS ESTRICTA. 

## H_a: El profesor Sebastian califica mas alto que Juvenal 
## (mediaSebastian) > (mediaJuvenal)
## PRUEBA DE COLA DERECHA, SE BUSCA UN EFECTO HACIA ARRIBA. 

## H_a: El profesor Sebastian califica mas estricto que Juvenal 
## (mediaSebastian) < (mediaJuvenal)
## PRUEBA DE COLA IZQUIERDA, PORQUE SE BUSCA UN EFECTO HACIA ABAJO. 
## Generalmente, elegimos la de dos colas si no sabemos el sentido del efecto de un tratamiento o condición. Aparte, es mas estricta. 

# Datos ----
# califJuve <-  rnorm(n = 10, mean = 8, sd = 0.7) %>% 
#   round(digits = 1)
# califSebas <- rnorm(n = 10, mean = 8.1, sd = 0.5) %>% 
#   round(digits = 1)

# Datos
tb <- tibble(calificaciones = append(califJuve, califSebas),
             calificador = c(rep("Juvenal", 10), 
                             rep("Sebastian", 10)))

write.csv(tb, "calificacionesExamen.csv", 
          na = "", row.names = F, fileEncoding = "UTF-8")

# Sacamos la diferencia de medias: 
dif_medias <- tb %>% 
  group_by(calificador) %>% 
  summarise(media = mean(calificaciones)) %>% 
  summarise(diff_medias = diff(media))

dif_medias

# De aqui se ve que la diferencia de medias es de 0.38 puntos a favor de Sebastian. 

# Para probar la hipotesis de dos colas, vamos a utilizar el metodo de permutaciones para recrear la hipotesis nula: 

dist <- tb %>% 
  rep_sample_n(size = nrow(tb), 
               replace = F, 
               reps = 1000) %>% 
  mutate(califPerm = sample(calificaciones,
                            replace = FALSE)) %>% 
  group_by(replicate, calificador) %>% 
  summarise(mediaCalifPerm = mean(califPerm)) %>% 
  ungroup() %>% 
  group_by(replicate) %>% 
  summarise(diffMediasPerm = diff(mediaCalifPerm)) 


# Graficamos la distribucion de las diferencias
dist %>% 
  ggplot() + 
  geom_density(aes(x = diffMediasPerm)) + 
  geom_vline(xintercept = dif_medias$diff_medias,
             linetype = "dashed",
             color = "red") + 
  geom_vline(xintercept = quantile(dist$diffMediasPerm, 0.975),
             color = "blue") + 
  geom_vline(xintercept = quantile(dist$diffMediasPerm, 0.025),
             color = "blue") + 
  theme_classic()

# Preguntas. 
# Hay alguna diferencia significativa entre las maneras de calificar de Juvenal y Sebastian, dados los datos? 

# Cual es el p-value? 

# Como deberiamos responder a la pregunta de la prueba de Hipotesis? 


## Ejercicio 2. Prueba de medias. ----

## En la Universidad Autonoma del Estado de Morelos queremos conocer el porcentaje de participación de los jóvenes de 18 años en las elecciones intermedias locales. 

## Un investigador piensa que los jovenes de esa edad no les interesa votar por sus diputados locales o federales, mientras que otro investigador tiene la hipótesis de que, a esa edad, estan tan emocionados de tener credencial que votan en gran proporción y en grandes cantidades. La hipótesis de este investigador es que, al menos el 60% de los jóvenes de 18 años votaron en las ultimas intermedias. 

## HIPÓTESIS A PROBAR. 

# HIPOTESIS NULA: 
## H_0: Los jóvenes de 18 años tuvieron una participación del 18% 

# HIPOTESIS ALTERNA: 

## H_a: Los jóvenes de 18 años tuvieron una participación distinta del 60% 

## H_a: Los jóvenes de 18 años tuvieron una participación MENOR al 60% 

## H_a: Los jóvenes de 18 años tuvieron una participación MAYOR al 60% 

# Datos: 

LN <- round(runif(n = 200, min = 10, max = 500), 0)
norm <- round(rnorm(n = 200, mean = 0, sd = 1), 3)
prop <- round((norm - min(norm))/max(norm - min(norm)),2)
SV <- round(LN * prop, 0)
NV <- LN - SV
SECCION <- sample(x = 1:1000, size = 200, replace = F) %>% sort()

datos <- tibble(EDAD = 18, 
                SECCION = SECCION, 
                LN = LN, SV = SV, NV = NV) 
write.csv(datos, "datosSeccionElectoralMorelos.csv", 
          na = "", row.names = F, fileEncoding = "UTF-8")

# %>% 
#   mutate(PART = (SV/LN)*100)

# Estadisticas Descriptivas
mean(datos$PART)
sd(datos$PART)


# BOOTSTRAP: 
dist_bootstrap <- datos %>% 
  rep_sample_n(size = nrow(datos), 
               replace = TRUE, 
               reps = 1000) %>% 
  group_by(replicate) %>% 
  summarise(media_bootstrap = mean(PART))

# Error Estandar Bootstrap
sd(dist_bootstrap$media_bootstrap)

# Grafica dist Bootstrap
ggplot(dist_bootstrap, aes(x = media_bootstrap)) + 
  geom_density() + 
  geom_vline(xintercept = mean(datos$PART), color = "red", linetype = 10) + 
  geom_vline(xintercept = quantile(dist_bootstrap$media_bootstrap, 0.025), color = "blue", linetype = 1) + 
  geom_vline(xintercept = quantile(dist_bootstrap$media_bootstrap, 0.975), color = "blue", linetype = 1) + 
  geom_vline(xintercept = 60, color = "green", linetype = 1)

# De aqui se ve que la evidencia no soporta la hipotesis nula (H_0) de que la media de la participacion electoral en jovenes de 18 anios es del 60%, con un 95% de confianza. 

# Igualmente, se ve que la estimacion de la media de la participacion de los jovenes de 18 anios, a partir de nuestra muestra, es de aprox. 55% con un intervalo de confianza al 95% de (52.22, 57.52).

