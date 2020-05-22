#·······························#
# Permutacion 2. ···············#
# Jorge Juvenal Campos Ferreira #
# Laboratorio MCA, sesion 6 ····#
#·······························#

#·················#
# P R O B L E M A #
#·················#

# El gobierno, preocupado por la calificación y el mal desempeño obtenido
## tras las ultimas pruebas estandarizadas a jóvenes de preparatoria
## deciden llevar a cabo una política pública entre jóvenes de tercer
## año de bachillerato. Para evitar que estos tengan que trabajar, y 
## garantizar que tienen el material necesario para estudiar, se les 
## proporciona a un grupo aleatorio de jóvenes una transferencia 
## monetaria para que cubran sus gastos diarios, de $20,000 pesos semestrales, 
## mientras que a otro grupo de ellos, no se les da nada (grupo de control)

## Se evalúa a estos dos grupos a partir de un exámen estandarizado 
## (igual para todos) para ver si el apoyo económico tiene un rendimiento 
## sobre su desempeño académico, obteniendose la base de datos que 
## se muestra a continuación.

## ¿Como determinaría usted, si la política pública tuvo algún 
## efecto, positivo o negativo, sobre el rendimiento de los estudiantes?

# ______________

# Librerias ----
library(tidyverse)
library(moderndive)

# La base de datos: ----
# Calificaciones de los estudiantes
(calificaciones <- c(6, 5.6, 8.4, 10, 9.4, 6.7, 9,
                     5.4, 8.5, 5.7, 7.5, 8.5, 6, 8.4,
                     9, 10, 3.6, 8.3, 10, 8.6, 3.8,
                     8.8, 6.7, 6.4, 8.1, 7.7, 4.6, 9.7,
                     8.8, 8.8, 7.7, 6.6, 8.8, 9.7, 9.3,
                     5.6, 5.5, 8.9, 8.5, 3.8, 5.6, 8.3,
                     5.3, 3.8, 7.7, 7.4, 9.9, 8.6, 8.9,
                     5.7, 5.5, 4.8, 6, 10, 9.2, 5.3,
                     6.3, 7.2, 7.1, 5.3, 6.8, 6.5, 5.6,
                     2.3, 5.5, 7.8, 5.2, 5, 10, 7))

# Categorias del experimento
(condicion <- c(rep("Transferencia", 35), rep("Control", 35)))

# Pegamos todo en un tibble
(datos <- tibble(ID = 1:70, 
                 calificaciones = calificaciones, 
                 condicion = condicion))

# Visualizamos los datos
datos %>% 
  print(n = Inf)


#···········#
# Análisis! #
#···········#

# Sacamos la media por condicion
datos %>% 
  group_by(condicion) %>% 
  summarise(media_grupo = mean(calificaciones)) %>% 
  summarise(diferencia_medias = diff(media_grupo))

datos %>% 
  group_by(__________) %>% 
  summarise(media_grupo = mean(__________)) %>% 
  summarise(diferencia_medias = diff())


# La diferencia es de 1 punto en la calificaci'on. 

# AHORA, SUPONGAMOS QUE UN CR'ITICO DE LA POL'ITICA NOS DICE
## QUE LAS TRANSFERENCIAS EN EFECTIVO NO SIRVEN DE NADA, Y QUE ESA 
## DIFERENCIA DE UN PUNTO ES POR PURO AZAR. 

## COMO PODEMOS, A PARTIR DE LO QUE SABEMOS, DEMOSTRAR LO CONTRARIO? 
# R: PARA ELLO, UTILIZAMOS UN METODO DE GENERACION DE MUESTRAS 
## LLAMADO PERMUTACION. 

dist <- datos %>% 
  rep_sample_n(size = nrow(datos), 
               replace = F, 
               reps = 10000) %>%
  mutate(califPerm = sample(calificaciones,
                            replace = F)) %>%
  group_by(replicate, condicion) %>%
  summarise(mediaCalif = mean(calificaciones),
            mediaCalifPerm = mean(califPerm)) %>% 
  ungroup() %>% 
  group_by(replicate) %>% 
  summarise(diffMedias = diff(mediaCalif), 
            diffMediasPerm = diff(mediaCalifPerm)) # Distribucion


# Graficamos la distribucion de las diferencias
dist %>% 
  ggplot() + 
  geom_density(aes(diffMediasPerm)) + 
  geom_vline(xintercept = 1.07, linetype = "dashed", color = "red") + geom_vline(xintercept = quantile(dist$diffMediasPerm, 0.975), color = "blue") + 
  geom_vline(xintercept = quantile(dist$diffMediasPerm, 0.025), color = "blue")

# Probabilidad de que sea igual o mayor 
casosPositivos <- dist %>% 
  filter(dist$diffMediasPerm > 1.07) %>% 
  nrow()

# Probabilidad de que esto ocurra (o que ocurra un efecto mas fuerte)
casosPositivos/10000

# Entonces, de aqui vemos que el evento es altamente improbable que pase por puro azar, y podemos decir que la evidencia no apoya la aseveracion de que el azar es el causante de dicho comportamiento en las calificaciones. 
