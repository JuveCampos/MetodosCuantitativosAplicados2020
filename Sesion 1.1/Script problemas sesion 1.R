# Librerias
library(tidyverse)

# Problema 0 ----
# El profe Juvenal desea comprarse el modelo más reciente del iPhone. Sin embargo, tiene miedo de que lo asalten en el camión de Tacubaya. Hablando con varios choferes y checando en datos de la Ciudad de México, la tasa de asaltos por camion es de 1 asalto al año en los viajes de la mañana. ¿Cuál es la probabilidad de que asalten al profesor en el primer viaje despues de comprar su nuevo teléfono? ¿Cual es la probabilidad de que le dure todo el semestre? 

# Numero de observaciones
num_obs <- 1e6

# Probabilidad de un día: 
probabilidad = 1/365

# Probabilidad de asalto tras 180 dias viajando desde Tacubaya: 
# Experimento aleatorio (1e6 simulaciones)
library(dplyr)

experimento <- rbinom(n = 1e6, 
                      size = 180, 
                      prob = probabilidad) %>% 
  tibble()

# Grafica
experimento %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = valores, y = prop)) + 
  geom_col() +
  geom_text(aes(y = prop + 0.01, label = prop))

# Probabilidad acumulada que deseamos! (para el valor 0)
tibble(valores = experimento) %>% 
  summarise(num_cumplen_condicion = sum(valores %in% c(0)),
            prop_cumplen_condicion = num_cumplen_condicion/num_obs)

# Problema 1. ----

## Calcula la probabilidad de que, dado que una mujer tiene tres hijos, al menos dos de estos sean del sexo femenino. 

# Experimento aleatorio (1e6 simulaciones)
experimento <- rbinom(n = 1e6, size = 3, prob = 0.5)

# Grafica
tibble(valores = experimento) %>% 
  count(valores) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = valores, y = prop)) + 
  geom_col() +
  geom_text(aes(y = prop + 0.01, label = prop))

# Probabilidad acumulada que deseamos!
tibble(valores = experimento) %>% 
  summarise(num_cumplen_condicion = sum(valores %in% c(1,2)),
            prop_cumplen_condicion = num_cumplen_condicion/num_obs)
  

# Problema 2. ----

## Compr'e tres cachitos de la loter'ia del avi'on presidencial. En total, se emitieron 6 millones de cachitos, y habr'an 100 cachitos premiados. Cu'al es la probabilidad de ganar al menos con uno de los cachitos? 

# Calculamos la probabilidad
probabilidad <- 100/6e6

# Experimento aleatorio (1e6 simulaciones)
experimento <- rbinom(n = 1e6, size = 3, prob = probabilidad)
table(experimento)

# Grafica
tibble(valores = experimento) %>% 
  count(valores) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = valores, y = prop)) + 
  geom_col() +
  geom_text(aes(y = prop + 0.01, label = prop))

# Probabilidad acumulada que deseamos!
tibble(valores = experimento) %>% 
  summarise(num_cumplen_condicion = sum(valores %in% c(1:3)),
            prop_cumplen_condicion = num_cumplen_condicion/num_obs)


# Problema 3 ----

## En una encuesta realizada en el estado de Michigan por CNN el mes pasado, se pregunt'o los resultados arrojaron que un 57% apoya a Bernie Sanders, y que un 43% apoya a Donald Trump. Si selecciono una muestra aleatoria de 20 personas de todo el estado de Michigan.... Cu'al es la probabilidad de que todas las personas apoyen a Bernie Sanders?

# Experimento aleatorio (1e6 simulaciones)
experimento <- rbinom(n = 1e6, size = 20, prob = 0.57)

# Grafica
tibble(valores = experimento) %>% 
  count(valores) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = valores, y = prop)) + 
  geom_col() +
  geom_text(aes(y = prop + 0.01, label = prop))

# Probabilidad acumulada que deseamos!
tibble(valores = experimento) %>% 
  summarise(num_cumplen_condicion = sum(valores %in% c(20)),
            prop_cumplen_condicion = num_cumplen_condicion/num_obs)

# Problema 4 ----

# Un pais tiene una población adulta cuya estatura promedio es de 1.70 m, con una desviación estándar de 15 cm (0.15 m). ¿Cuál es la probabilidad de seleccionar una persona que mida 1.95 m o más?

# Experimento aleatorio (1e6 simulaciones)
?rnorm
media = 1.7
desv.est = 0.15
Z = 1.95
experimento <- rnorm(n = 1e6, mean = media, sd = desv.est)

# Grafica de continuas
tibble(valores = experimento) %>% 
  ggplot() + 
  geom_density(aes(x = valores)) +
  geom_vline(xintercept = media, color="salmon", linetype = "dashed") +
  geom_vline(xintercept = Z, color="navyblue")

# Probabilidad acumulada que deseamos!
tibble(valores = experimento) %>% 
  summarise(num_cumplen_condicion = sum(valores %in% c(20)),
            prop_cumplen_condicion = num_cumplen_condicion/num_obs)

# Problema 5. ----

# En un pequeño pueblo del Estado de Morelos, fallece una persona cada 2 semanas, por cualquier causa. Suponiendo que la muerte de una persona no influye en la muerte de otra, ¿cual es la probabilidad de que mueran 1, 2 o 3 personas en una semana dada?

# Experimento aleatorio (1e6 simulaciones)
experimento <- rpois(n = 1e6, lambda = 0.5)
table(experimento)

# Grafica
tibble(valores = experimento) %>% 
  count(valores) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = valores, y = prop)) + 
  geom_col() +
  geom_text(aes(y = prop + 0.01, label = prop))

# Probabilidad acumulada que deseamos!
tibble(valores = experimento) %>% 
  count(valores) %>% 
  mutate(prop = n/sum(n), 
         cumsum = cumsum(prop))

# Problema 6.  ----

# Es viernes, son las 10 de la noche y trabajas en un Oxxo. Sabes que la tasa promedio de asistencia durante esta hora es de 100 clientes por hora. A pesar de saber esto, ninguno de tus compañeros acudió para ayudarte a abrir la segunda caja. Si el Oxxo se sale de control si llegan más de 120 clientes... cual es la probabilidad de que pases un mal momento este viernes? (i.e., que lleguen mas de 120 clientes). 

# Experimento aleatorio (1e6 simulaciones)
?rpois

# Experimento 1
experimento <- rpois(n = 1e6, lambda = 100)

# Experimento 2
experimento2 <- rnorm(n = 1e6, mean = 100, sd = 10) %>% as_tibble()


# Grafica
tibble(valores = experimento) %>% 
  count(valores) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot() + 
  geom_col(aes(x = valores, y = prop)) +
  geom_text(aes(x = valores, y = prop + 0.01, label = prop)) + 
  geom_density(data = experimento2, aes(x = value))

# Probabilidad acumulada que deseamos!
tibble(valores = experimento) %>% 
  summarise(num_cumplen_condicion = sum(valores >= 120),
            prop_cumplen_condicion = num_cumplen_condicion/num_obs)
