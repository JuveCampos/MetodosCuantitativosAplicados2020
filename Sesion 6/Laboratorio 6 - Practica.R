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
datos <- tibble(valores = c(81.372918, 25.700971, 4.942646, 43.020853, 81.690589, 51.195236,
           55.659909, 15.153155, 38.745780, 12.610385, 22.415094, 18.355721,
           38.081501, 48.171135, 18.462725, 44.642251, 25.391082, 20.410874,
           15.778187, 19.351485, 20.189991, 27.795406, 25.268600, 20.177459,
           15.196887, 26.206537, 19.190966, 35.481161, 28.094252, 30.305922))

# Sacamos la media
mean(datos$valores)

# Graficamos
ggplot(data = datos, aes(x = valores)) +
  geom_density()  +
  theme_bw()

# Es normal la distribucion de los datos?
# Hipotesis nula: los datos distribuyen normal 
shapiro.test(datos$valores)
# Como p<0.01, podemos rechazar la hipotesis nula. 
# Por lo tanto, datos no distribuye normal y no podemos
# sacar intervalos de confianza mediante la formula del 1.96

# USEMOS BOOTSTRAP. 
dist_bootstrap <- datos %>% 
  rep_sample_n(size = nrow(datos), 
               replace = TRUE, 
               reps = 10000) %>% 
  summarise(media_bootstrap = mean(valores))

mean(dist_bootstrap$media_bootstrap)
  
dist_bootstrap %>% 
  ggplot(aes(media_bootstrap)) + 
  geom_density() + 
  geom_vline(xintercept = 30.97628, color = "blue") 

# Sacamos los limites de los intervalos de confianza: 
lim_inf <- quantile(x = dist_bootstrap$media_bootstrap, 0.025)
lim_sup <- quantile(x = dist_bootstrap$media_bootstrap, 0.975)

# Los metemos a la grafica
dist_bootstrap %>% 
  ggplot(aes(media_bootstrap)) + 
  geom_density() + 
  geom_vline(xintercept = 30.97628, color = "blue") + 
  geom_vline(xintercept = lim_inf, color = "red", linetype = "dashed") + 
  geom_vline(xintercept = lim_sup, color = "red", linetype = "dashed")

# ................ #
# Respuesta final: #
# ................ #
# La estimacion de la media poblacional es de 30.97628
# y se encuentra entre 24.925 y 37.922 con un nivel de 
# confianza del 95%.

# Nota, el resultado puede variar, dependiendo de la semilla utilizada.  


# OTRA OPCI'ON: FUNCION QUE TE CALCULA TODO DE GOLPE:
# SOLO SE VALE USAR ESTA FUNCION CUANDO YA DOMINAS TODA
# LA INTUICI'ON DEL TEMA. 
library(bootES)
?bootES
bootES(data = datos$valores, 
       ci.conf = 0.95, 
       R = 10000, 
       mean)






