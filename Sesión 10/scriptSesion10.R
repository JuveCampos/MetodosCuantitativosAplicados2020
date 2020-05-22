# Sesion 10. Laboratorio MCA.

# Librerias a utilizar: ----
library(tidyverse)
library(broom)     # Para trabajar con objetos modernos de R 
library(moderndive)

# 1a parte. Base de datos a utilizar: ----
# Pitufos 
bd <- read_csv("https://raw.githubusercontent.com/JuveCampos/PeriodismoDeDatos2020/master/Laboratorio%2010/pitufi2.csv") %>% 
  select(-X1)

# 1. Explorar la base de datos. 
head(bd)

# Formula 
formula <-  p.fresas ~ p.vitam + p.musc +  p.horas
modelo <- lm(formula = formula, 
             data = bd)
class(modelo)
# Imprimimos el objeto lm llamado modelo
modelo
# Mas informacion del modelo de regresion lineal
summary(modelo)

# La prueba de hipotesis que estamos probando pero en ningun 
# momento no lo han especificado porque asumen que ya la sabemos es: 
# H_0: \beta1  = 0
# H_a: \beta1 != 0

# Si hay estrellitas, es que se rechaza la hipotesis de que el beta es igual a cero
# y por lo tanto podemos concluir que hay cierto efecto por parte de la 
# variable explicativa. 
ggplot(bd, aes(y = p.fresas, x = p.vitam)) + 
  geom_point() 

# Recapitulando: 
# El resultado de los coeficientes nos dice el efecto de la 
#  variable X sobre Y. 
# El resultado de los p-values nos sirve para medir la incertidumbre
#  sobre nuestras estimaciones puntuales (para probar la hipotesis
#  que escribimos arriba) y el valor de R2 es para ver la capacidad 
#  explicativa del modelo (R2 va de 0 a 1 donde 0 es _nada explicativo_)
#  y 1 es totalmente explicativo). 


# Segunda base de datos. 
# Pregunta de investigacion:
# Que factores influyen en el precio de las casas en LA, California? 

# 1. Leemos los datos: 
homesLA <- read_csv("https://raw.githubusercontent.com/JuveCampos/PeriodismoDeDatos2020/master/Laboratorio%2009/datos/LAhomes.csv")

# 2. Exploramos los datos: 
head(homesLA)
unique(homesLA$city)

# 3. Creamos un modelo 
formula3 <- price ~ sqft + city

# 4. Exploramos el modelo: 
modelo3 <- lm(formula = formula3, 
              data = homesLA)
summary(modelo3)

# 5. Extraemos los coeficientes. 
# modelo3$coefficients 

# 6. Extraemos los residuales
# modelo3$residuals

# 7. Le metemos la escoba: 
# tidy(modelo3)

# 8. Obtenemos la distribucion muestral y 
# checamos el significado de las estrellitas

# Coeficiente que me interesa analizar
coeficiente_a_probar <- "citySanta Monica"
# Valor de significancia 
significancia <- 0.05

# Calculamos la distribucion muestral
dist_muestral <- homesLA %>% 
  rep_sample_n(size = nrow(homesLA), 
               replace = T, 
               reps = 200) %>% 
  # Aqui sacamos la estimacion puntual 
  do(lm(formula3, data = .) %>% tidy()) %>% 
  ungroup() %>% 
  filter(term == coeficiente_a_probar) 

# sd(dist_muestral$estimate)
# mean(dist_muestral$estimate)
# summary(modelo3)

# Obtenemos la grafica: 
dist_muestral %>% 
  ggplot(aes(x = estimate)) + 
  geom_density() + 
  geom_vline(aes(xintercept = mean(estimate)), 
             color = "red", 
             linetype = 2) + 
  geom_label(aes(label = paste0("Beta = ", prettyNum(modelo3$coefficients[coeficiente_a_probar],
                                                     digits = 2,
                                                     big.mark = ",")),
                 x = mean(estimate),
                 y = 0), color = "red",
             angle = 0) +
  geom_vline(aes(xintercept = quantile(estimate, 
                                       significancia/2)), 
             color = "blue", 
             linetype = 1) + 
  geom_vline(aes(xintercept = quantile(estimate, 
                                       (1-(significancia/2)))), 
             color = "blue", 
             linetype = 1) + 
  geom_vline(xintercept = 0, 
             color = "green", 
             linetype = 10)
