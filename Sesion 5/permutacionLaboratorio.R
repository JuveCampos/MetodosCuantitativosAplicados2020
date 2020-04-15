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

## __________________________________________________________________

# Librerias ----
library(__________) # Para generar muestreos
library(__________) # Para manipular bases de datos y usar la pipa


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

# PASO 1. Obtenemos la diferencia de medias por categoria. ----
# (Estimación puntual)

datos %>% 
  group_by(__________) %>% 
  summarise(media_grupo = mean(__________)) %>% 
  summarise(diferencia_medias = diff(_________))


# La diferencia es de ___ punto(s) en la calificacion. 
# El grupo _____________ tiene un mayor promedio. 


# PASO 2. Determinamos la prueba de Hipótesis. ----

# AHORA, SUPONGAMOS QUE UN CRITICO DE LA POLÍTICA (EL DR. JUAN NULO)
## NOS DICE QUE LAS TRANSFERENCIAS EN EFECTIVO NO SIRVEN DE 
## NADA, Y QUE ESA DIFERENCIA ES POR PURO AZAR. 

## ¿CUAL SERÍA LA PRUEBA DE HIPÓTESIS QUE TENDRÍAMOS QUE FORMULAR
## PARA VER SI LOS DATOS APOYAN LA HIPÓTESIS DEL DR. JUAN NULO? 

## H_0: ____________________________________________

## H_a: ____________________________________________

## ¿QUÉ TENEMOS QUE HACER PARA LLEVAR A CABO LA PRUEBA DE HIPÓTESIS? 

## _________________________________________________________________


# PASO 3. MÉTODO DE GENERACIÓN DE MUESTRAS. ----

## ¿CUALES SON LOS DOS MÉTODOS DE GENERACIÓN DE MUESTRAS VISTOS 
## EN CLASE HASTA AHORA? 

# __________________________________________________________________

# PARA RESOLVER EL PRESENTE PROBLEMA, VAMOS A UTILIZAR EL MÉTODO
# DE PERMUTACIONES! 😉 ESTE MÉTODO, AL REVOLVER POR EFECTO DEL AZAR
# LAS OBSERVACIONES QUE OBTENEMOS, LO QUE HACE ES SIMULAR EL MUNDO 
# DEL DR. JUAN NULO, DE QUE TODO ESTO SE DIO POR AZAR. 

# Vamos a guardar en el objeto <dist> la distribución
# de las muestras permutadas.

# Vamos a generar 10,000 muestras permutadas

dist <- datos %>% 
# Generamos la muestra  
  rep_sample_n(size = nrow(datos), 
               replace = _____, 
               reps = _____) %>%
  # Generamos una nueva columna con los valores permutados
  _______(califPerm = ________(calificaciones,
                            replace = _____)) %>%
  # Agrupamos por muestra y por grupo del experimento
  group_by(replicate, __________) %>%
  # Obtenemos las medias de las calificaciones y de las calif. permutadas
  summarise(mediaCalif = mean(_____________),
            mediaCalifPerm = mean(____________)) %>% 
  # Desagrupamos (para que hacemos esto?)
  ungroup() %>% 
  # Sacamos la diferencia de promedios permutados
  group_by(replicate) %>% 
  summarise(diffMedias = diff(_____________), 
            diffMediasPerm = diff(_____________)) # Distribucion nula


# Graficamos la distribucion de las diferencias
dist %>% 
  ggplot() + 
  geom_density(aes(________________)) + 
  # Metemos una linea en donde nos dió la diferencia de medias original
  geom_vline(xintercept = ___________, linetype = "dashed", color = "red") + 
  # Metemos una linea a partir de la cual queden fuera los ultimos 2.5% de los datos
  geom_vline(xintercept = quantile(dist$diffMediasPerm, 0.975), color = "blue") + 
  # Metemos una linea a partir de la cual queden fuera los primeros 2.5% de los datos
  geom_vline(xintercept = quantile(dist$diffMediasPerm, 0.025), color = "blue")

# OBTENEMOS EL p-value, la probabilidad de que obtener un valor igual o más extremo 
casosPositivos <- dist %>% 
  filter(dist$diffMediasPerm > _________) %>% 
  nrow()

# Probabilidad de que esto ocurra (o que ocurra un efecto mas fuerte)
casosPositivos/10000

# Entonces, de aqui vemos que el evento es altamente 
# _____________ que pase por puro azar, y podemos decir ç
# que la evidencia __ apoya la aseveracion de que el azar 
# es el causante de dicho comportamiento en las calificaciones, con 
# un __ % de confianza. 
