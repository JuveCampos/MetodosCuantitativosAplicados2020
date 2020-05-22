# Sesion 10 - Laboratorio MCA. 

# Librerias a utilizar: ----
library(tidyverse)
library(broom)
library(moderndive)

# Base de datos a utilizar: ----
# Pitufos
bd <- read_csv("https://raw.githubusercontent.com/JuveCampos/PeriodismoDeDatos2020/master/Laboratorio%2010/pitufi2.csv") %>% 
  select(-X1)

# Paso 1. Explorar bases de datos. 
head(bd)

# Paso 2. Elaboramos un modelo: 
formula1 <- p.fresas ~ p.vitam
formula2 <- p.fresas ~ p.vitam + p.musc + p.horas

modelo1 <- lm(formula = formula1, 
              data = bd)

modelo2 <- lm(formula = formula2, 
              data = bd)

# Paso 3. Exploramos el modelo: 
summary(modelo1)
summary(modelo2)

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
  #city + sqft + bed + bath

# 4. Exploramos el modelo: 
modelo3 <- lm(formula = formula3, 
              data = homesLA)

summary(modelo3)

# 5. Extraemos los coeficientes. 
modelo3$coefficients 

# 6. Extraemos los residuales
modelo3$residuals

# 7. Le metemos la escoba: 
tidy(modelo3)

# 8. Obtenemos la distribuci'on muestral y 
# checamos el significado de las estrellitas
homesLA %>% 
  rep_sample_n(size = nrow(homesLA), 
               replace = T, 
               reps = 500) %>% 
  # Aqui sacamos la estimacion puntual 
  do(lm(formula3, data = .) %>% tidy()) %>% 
  filter(term == "sqft") %>% 
  ggplot(aes(x = estimate)) + 
  geom_density() + 
  geom_vline(aes(xintercept = mean(estimate)), 
             color = "red", 
             linetype = 2) + 
  geom_vline(aes(xintercept = quantile(estimate, 0.005)), 
             color = "blue", 
             linetype = 1) + 
  geom_vline(aes(xintercept = quantile(estimate, 0.995)), 
             color = "blue", 
             linetype = 1) + 
  geom_vline(xintercept = 0, 
             color = "green", 
             linetype = 10)

  





