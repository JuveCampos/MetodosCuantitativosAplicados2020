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


