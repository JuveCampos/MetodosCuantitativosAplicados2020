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
datos <- read_csv("datosSeccionElectoralMorelos.csv")



