library(tidyverse)

data(mtcars)

niveles<- function(x) (levels(as.factor(x)))


# Funciones para realizar graficas de barras
?geom_histogram
?geom_bar
?geom_col


muestra <- sample(x = 1:10, size = 1000, replace = TRUE) %>% as.data.frame()

ggplot(data = mtcars, aes(x = gear)) + 
  geom_histogram()
niveles(mtcars$gear)

# geom_histogram()

# Histograma de la base de datos de Diamantes
## Variable carat, kilates.
ggplot(diamonds, aes(carat)) +
  geom_histogram()

# Histograma con los mismos datos, pero con un ancho de bin de 0.01
ggplot(diamonds, aes(carat)) +
  geom_histogram(binwidth = 0.5)

# Histograma con los mismos datos, pero forzando la existencia de 200 bines.
ggplot(diamonds, aes(carat)) +
  geom_histogram(bins = 200)


# geom_col()
data("iris")

# Grafica de Barras de flores por ancho de pétalo
iris %>% 
  group_by(Petal.Width) %>% 
  count() %>% 
  ungroup() %>% 
  ggplot(aes(x = Petal.Width, y = n)) + 
  geom_col()




# geom_bar()
# Recomendado: http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization

# Grafica de barras de la base de datos de Diamantes
## Variable carat, kilates.

# Una barra por cada dato único
ggplot(diamonds, aes(carat)) +
  geom_bar()

# Con stat = "bin" se comporta igual que un geom_histogram()
ggplot(diamonds, aes(carat)) +
  geom_bar(stat = "bin", bins = 200)

ggplot(diamonds, aes(carat)) +
  geom_bar(stat = "bin", binwidth = 0.5)
# En este caso, mejor usar histogram

iris %>% 
  group_by(Petal.Width) %>% 
  count() %>% 
  ungroup() %>% 
  ggplot(aes(x = Petal.Width, y = n)) + 
  geom_bar(stat = "identity")


