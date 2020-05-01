# Mostrar como el tamanio de muestra afecta 
# las estimaciones bootstrap. 

# Librerias
library(moderndive)
library(tidyverse)

# 1. Generamos una poblacion 
pob <- tibble(obs = rnorm(n = 1000000, 
                          mean = 1.6, sd = 0.25))

# Densidad de la poblacion
pob %>% 
  ggplot() + 
  geom_density(aes(obs)) + 
  geom_vline(xintercept = quantile(pob$obs, 0.025), color = "red", linetype = 2) + 
  geom_vline(xintercept = quantile(pob$obs, 0.975), color = "red", linetype = 2) +
  geom_vline(xintercept = mean(pob$obs), color = "blue", linetype = 1) 

(datos_reales <- c(quantile(pob$obs, 0.025), 
           mean = mean(pob$obs), 
           quantile(pob$obs, 0.975)))

# 2. Obtenemos una muestra de 10,000 observaciones 
# de la poblacion 
muestra <- pob %>% 
  rep_sample_n(size = 10000, 
               replace = F, 
               reps = 1) %>% 
  ungroup() %>% 
  select(-replicate)

# 3. Ahora, olvidamos la muestra. 

# 4. Con la informacion unica de esa muestra, 
# vamos a estimar la media y los intervalos de confianza: 

# 1. Metodo tradicional ----
(media = mean(muestra$obs))

# Error estandar
se <- sd(muestra$obs)/sqrt(nrow(muestra))

# Intervalos de confianza: 
lim_inf <- media - 1.96*se
lim_sup <- media + 1.96*se  

(datos_tradicional <- c(quantile(pob$obs, 0.025), 
                        mean = mean(pob$obs), 
                        quantile(pob$obs, 0.975)))

# 2. Metodo Bootstrap ----
(media = mean(muestra$obs))

# Obtenemos la distribucion bootstrap
dist_bootstrap <- muestra %>% 
  rep_sample_n(size = nrow(muestra), 
               replace = TRUE, 
               reps = 10000) %>%  
  summarise(media_bootstrap = mean(obs))

# Graficamos la distribucion bootstrap
dist_bootstrap %>% 
  ggplot() + 
  geom_density(aes(media_bootstrap)) + 
  geom_vline(xintercept = quantile(dist_bootstrap$media_bootstrap, 0.025), color = "red", linetype = 2) + 
  geom_vline(xintercept = quantile(dist_bootstrap$media_bootstrap, 0.975), color = "red", linetype = 2) +
  geom_vline(xintercept = mean(muestra$obs), color = "blue", linetype = 1)

# Datos obtenidos de la estimacion bootstrap:
(datos_bootstrap <- c(quantile(dist_bootstrap$media_bootstrap, 0.025),
                     media = mean(muestra$obs),
                     quantile(dist_bootstrap$media_bootstrap, 0.975), 
                     dif_intervalo = quantile(dist_bootstrap$media_bootstrap, 0.975) - quantile(dist_bootstrap$media_bootstrap, 0.025)))


# Diferencias de la estimacion bootstrap en funcion del tamanio de la muestra 

tamanio_muestra <- 10000


bootstraps_tamanio_muestra <- function(tamanio_muestra){
    print(tamanio_muestra)  
    muestra <- pob %>% 
      rep_sample_n(size = tamanio_muestra, 
                   replace = F, 
                   reps = 1) %>% 
      ungroup() %>% 
      select(-replicate)
    
    # Obtenemos la distribucion bootstrap
    dist_bootstrap <- muestra %>% 
      rep_sample_n(size = nrow(muestra), 
                   replace = TRUE, 
                   reps = 100) %>%  
      summarise(media_bootstrap = mean(obs))
    
    # Graficamos la distribucion bootstrap
    # dist_bootstrap %>% 
    #   ggplot() + 
    #   geom_density(aes(media_bootstrap)) + 
    #   geom_vline(xintercept = quantile(dist_bootstrap$media_bootstrap, 0.025), color = "red", linetype = 2) + 
    #   geom_vline(xintercept = quantile(dist_bootstrap$media_bootstrap, 0.975), color = "red", linetype = 2) +
    #   geom_vline(xintercept = mean(muestra$obs), color = "blue", linetype = 1) + 
    #   labs(title = paste0("I.C. Bootstrap al 95% de confianza con n = ", tamanio_muestra)) + 
    #   scale_x_continuous(limits = c(1.45, 1.80))
    
    # ggsave(paste0("imgs/bootPlot", tamanio_muestra, ".png"))
    
    # # Datos obtenidos de la estimacion bootstrap:
    # (datos_bootstrap <- c(quantile(dist_bootstrap$media_bootstrap, 0.025),
    #                       media = mean(muestra$obs),
    #                       quantile(dist_bootstrap$media_bootstrap, 0.975))
      
    dif_intervalo = quantile(dist_bootstrap$media_bootstrap, 0.975) - quantile(dist_bootstrap$media_bootstrap, 0.025)  
    return(c(tamanio_muestra, dif_intervalo))
}

# Aplicamos el proceso bootstrap variando los tamanios de muestra
l <- lapply(c(2:300), bootstraps_tamanio_muestra)

# Base de datos
bd <- l %>% plyr::ldply() %>% as_tibble()
names(bd) <- c("tamanio_muestra", "amplitud_IC")

# Grafica
ggplot(bd, aes(x = tamanio_muestra, y = amplitud_IC)) + 
  geom_point(color = "navy") + 
  geom_label(aes(label = tamanio_muestra)) + 
  labs(title = "Amplitud del Intervalo de Confianza\nal 95% en función del tamaño de muestra", 
       caption = "Numero en la etiqueta representa el tamaño de muestra\nutilizado para el metodo Bootstrap") + 
  theme_bw()


