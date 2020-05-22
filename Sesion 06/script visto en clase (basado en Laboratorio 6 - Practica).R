# Ejemplo practico Bootstrap

# Problema: 
# Se dispone de una muestra formada por 30 observaciones 
# de una variable aleatoria continua. 
# Se quiere inferir el valor medio de dicha variable en 
# la población mediante el cálculo de un intervalo de 
# confianza del 95%.

# Librerias
library(moderndive)
library(tidyverse)

# Datos
# Datos 
datos <- tibble(valores = c(81.372918, 25.700971, 4.942646, 43.020853, 81.690589, 51.195236,
                            55.659909, 15.153155, 38.745780, 12.610385, 22.415094, 18.355721,
                            38.081501, 48.171135, 18.462725, 44.642251, 25.391082, 20.410874,
                            15.778187, 19.351485, 20.189991, 27.795406, 25.268600, 20.177459,
                            15.196887, 26.206537, 19.190966, 35.481161, 28.094252, 30.305922))

# Media
mean(datos$valores)
# 30.96866

# Grafica de densidad de los valores 
datos %>% 
  ggplot(aes(valores)) + 
  geom_density()

# Comprobar si la distribucion es normal 
# Prueba shapiro-wilk 
# H_0 es que la distribucion es normal 
shapiro.test(datos$valores)
?shapiro.test

# Usemos bootstrap
# Generar nuestra distribucion bootstrap
(dist_bootstrap <- datos %>% 
  rep_sample_n(size = nrow(datos), 
               replace = TRUE, 
               reps = 10000) %>% 
  summarise(media_bootstrap = mean(valores)) )

dist_bootstrap %>% 
  ggplot(aes(media_bootstrap)) + 
  geom_density()

# Obtener los intervalos de confianza: 
lim_inf <- quantile(dist_bootstrap$media_bootstrap, 0.025)
lim_sup <- quantile(dist_bootstrap$media_bootstrap, 0.975)  

dist_bootstrap %>% 
  ggplot(aes(media_bootstrap)) + 
  geom_density() + 
  geom_vline(xintercept = lim_inf, 
             color = "red", 
             linetype = "dashed") + 
  geom_vline(xintercept = lim_sup, 
             color = "red", 
             linetype = "dashed") + 
  geom_vline(xintercept = 30.96866, 
             color = "blue")


