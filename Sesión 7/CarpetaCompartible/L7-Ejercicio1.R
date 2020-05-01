# Laboratorio 7. 
# Ejercicio 1. 
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
tb <- read_csv("calificacionesExamen.csv")

# Diferencia de medias
diff_medias <- tb %>% 
  group_by(calificador) %>% 
  summarise(media = mean(calificaciones)) %>% 
  summarise(diff_medias = diff(media)) %>% 
  pull()

# Para probar la hipotesis de que hay una diferencia de medias, utilizaremos el metodo de permutaciones. 

dist <-tb %>% 
  rep_sample_n(size = nrow(tb), 
               replace = F, 
               reps = 1000) %>% 
  mutate(calif_perm = sample(calificaciones, 
                             replace = F)) %>% 
  group_by(replicate, calificador) %>% 
  summarise(mediaCalifPerm = mean(calif_perm)) %>% 
  ungroup() %>% 
  group_by(replicate) %>% 
  summarise(diffMediasPerm = diff(mediaCalifPerm))

dist %>% 
  ggplot(aes(x = diffMediasPerm)) + 
  geom_density() + 
  geom_vline(xintercept = diff_medias, 
             color = "green", linetype = 2) + 
  geom_vline(xintercept = mean(dist$diffMediasPerm), 
             color = "red", linetype = 2) + 
  geom_vline(xintercept = quantile(dist$diffMediasPerm, 0.025), color = "blue") + 
  geom_vline(xintercept = quantile(dist$diffMediasPerm, 0.975), color = "blue")

#pvalue 
sum(dist$diffMediasPerm > diff_medias)/1000
0.123 > 0.05

# Hay diferencia significativa entre un revisor y otro? 


