# Laboratorio 7. 
# Juvenal Campos. 

# Librerias ----
pacman::p_load(moderndive, tidyverse)

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
datos <- read_csv("datosSeccionElectoralMorelos.csv") %>% 
  mutate(PART = (SV/LN)*100)

mean(datos$PART)
sd(datos$PART)

# Mediante bootstrap, vamos a sacar el rango de los valores que pueden capturar al estimador puntual 

dist_bootstrap <- datos %>% 
  rep_sample_n(size = nrow(datos), 
               replace = T, 
               reps = 1000) %>% 
  group_by(replicate) %>% 
  summarise(media_bootstrap = mean(PART))

# DIST. BOOTSTRAP NULA
dist_bootstrap_nula <- dist_bootstrap + (60-mean(datos$PART))

ggplot(dist_bootstrap, aes(x = media_bootstrap)) + 
  #geom_density() + 
  geom_density(data = dist_bootstrap_nula, color = "blue") +
  geom_vline(xintercept = mean(datos$PART), color = 'red', linetype = 2) + 
   geom_vline(xintercept = 60, color = 'green', linetype = 1) #+ geom_vline(xintercept = quantile(dist_bootstrap$media_bootstrap, 0.025), color = "blue") 
#+ 
  # geom_vline(xintercept = quantile(dist_bootstrap$media_bootstrap, 0.975), color = "blue")
  
# Ho: media == 60
# Rechazamos la hipotesis nula de que la media sea igual a 60, y por lo tanto, podemos pensar que la media es diferente de 60. 












