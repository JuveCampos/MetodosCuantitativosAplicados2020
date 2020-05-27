# Sesion 11. Laboratorio MCA.

# Librerias a utilizar: ----
library(tidyverse)
library(broom)     # Para trabajar con objetos modernos de R 
library(moderndive)

#Demostracion de coeficientes
# https://www.uv.es/uriel/material/Morelisi.pdf
# paginas I-5 a I-7

# 1a parte. Base de datos a utilizar: ----
# Pitufos 
pitufi <- read_csv("https://raw.githubusercontent.com/JuveCampos/PeriodismoDeDatos2020/master/Laboratorio%2010/pitufi2.csv") %>% 
  select(-X1)

# Variables: 
# p.fresas - cosecha de fresas en gramos
# p.vitam - vitaminas ingeridas por cada pitufo, en miligramos
# p.musc - indice de masa muscular (va de 0 a 8)
# p.horas - horas de capacitación antes del trabajo

modelo1 <- lm(formula = p.fresas ~ p.vitam, 
              data = pitufi)

# Imprimir el modelo 
modelo1

# Ver mas informacion del modelo 
summary(modelo1)

# Grafica
# dibujamos el modelo 
ggplot(data = pitufi, 
       aes(y = p.fresas, x = p.vitam)) +
  geom_point() + 
  geom_abline(intercept = modelo1$coefficients[1], 
              slope = modelo1$coefficients[2], 
              color = "red", 
              size = 2) + 
  geom_smooth(method = "lm") + 
  scale_x_continuous(limits = c(0, max(pitufi$p.vitam))) + 
  theme_classic()

# Obtener los coeficientes a mano: 
pendiente  <- cov(pitufi$p.vitam,pitufi$p.fresas)/var(pitufi$p.vitam)
intercepto <- mean(pitufi$p.fresas) - pendiente*mean(pitufi$p.vitam)

# Modelo de prediccion 
prediccion <- function(vitaminas){
  prediccion <- intercepto + pendiente*vitaminas
  return(prediccion)
}

prediccion(vitaminas = 5)

# Obtencion de los residuales: 
residuales <- augment(modelo1) %>% 
  select(.resid) 

residuales %>% 
  ggplot(aes(x = .resid)) + 
  geom_density() + 
  geom_vline(xintercept = 0)

# Formula de regresion : 
# formula1 <- p.fresas ~ p.vitam
# Calculamos la distribucion muestral
dist_muestral <- pitufi %>% 
  rep_sample_n(size = nrow(pitufi), 
               replace = T, 
               reps = 10000) %>% 
  # Aqui sacamos la estimacion puntual 
  do(lm(formula = p.fresas ~ p.vitam, data = .) %>% tidy()) %>% 
  ungroup() %>% 
  filter(term == "p.vitam") 

# Distribucion muestral del coeficiente
dist_muestral %>% 
  ggplot(aes(x = estimate)) + 
  geom_density()

# Error est'andar del estimador: 
sd(dist_muestral$estimate)


# Modelo multivariado. ----
# En un modelo multivariado, vamos a introducir multiples 
# variables explicativas al modelo. 

# Las interpretaciones son las mismas que en el univariado. 
formula.multi <- p.fresas ~ p.vitam + p.musc + p.horas
modelo.multi <- lm(formula = formula.multi, 
                   data = pitufi)

summary(modelo.multi)

# Diferencias. 
# El calculo a mano ya es mas complicado (hay que usar matrices)
# No se puede graficar la regresi'on total. 

# Modelo predictivo 
prediccionMulti <- function(vitam, musc, horas){
  cosecha <- modelo.multi$coefficients[1] + 
    modelo.multi$coefficients[2]*vitam + 
    modelo.multi$coefficients[3]*musc + 
    modelo.multi$coefficients[4]*horas
  cosecha <- as.numeric(cosecha)
  return(cosecha)
}

# Formula para predecir la cosecha
prediccionMulti(vitam = 4, musc = 8, horas = 1)
