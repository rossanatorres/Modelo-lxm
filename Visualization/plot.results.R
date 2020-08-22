rm(list=ls())
options(scipen = 99999)
library(tidyverse)


dir <- "~/Dropbox/Modelo CDMX/Modelo en Github/Results/"

days <- 31 #días simulados en resultados

load(paste0(dir, "raw.4x3.rda"))
load(paste0(dir, "raw.5x2.rda"))
load(paste0(dir, "raw.4x10.rda"))
load(paste0(dir, "raw.6x1.rda"))


mat_list <- list(raw43, raw410, raw52, raw61)


df_list <- lapply(mat_list, as.data.frame)
names(df_list) <- c("Resultados.43", "Resultados.410", "Resultados.52", "ctf")


#-----------------------------------------------------------------------------------------------
#                                           RESTA
#-----------------------------------------------------------------------------------------------
qest = c(0.25, 0.5, 0.75)
resta_list <- list()
for (i in 1:3) {
  
  resta <- df_list[[i]] - df_list$ctf 
  resta <- (t(apply(resta, 1, function (x) quantile(x, na.rm = TRUE, qest))))          
  resta_list[[i]] <- as.data.frame(resta)
  names(resta_list)[i] <- names(df_list)[i]
  rm(resta)

}


ggplot() +
  theme_classic() +
  
  geom_line(data = resta_list$Resultados.43, aes(x = 1:days, y = `50%`, 
                                      color = "4x3", 
                                      linetype = "50%"), group = 1, size = 0.75) +
  
  geom_line(data = resta_list$Resultados.43, aes(x = 1:days, y = `25%`, 
                                      color = "4x3", 
                                      linetype = "25%"), group = 1, size = 0.75) +
  
  geom_line(data = resta_list$Resultados.43, aes(x = 1:days, y = `75%`, 
                                      color = "4x3", 
                                      linetype = "75%"), group = 1, size = 0.75) +
  
  
  geom_line(data = resta_list$Resultados.410, aes(x = 1:days, y = `50%`, 
                                       color = "4x10", 
                                       linetype = "50%"), group = 1, size = 0.75) +
  
  
  geom_line(data = resta_list$Resultados.410, aes(x = 1:days, y = `25%`, 
                                       color = "4x10", 
                                       linetype = "25%"), group = 1, size = 0.75) +
  
  
  geom_line(data = resta_list$Resultados.410, aes(x = 1:days, y = `75%`, 
                                       color = "4x10", 
                                       linetype = "75%"), group = 1, size = 0.75) +
  
  
  geom_line(data = resta_list$Resultados.52, aes(x = 1:days, y = `50%`, 
                                      color = "5x2", 
                                      linetype = "50%"), group = 1, size = 0.75) +
  
  
  geom_line(data = resta_list$Resultados.52, aes(x = 1:days, y = `25%`, 
                                      color = "5x2", 
                                      linetype = "25%"), group = 1, size = 0.75) +
  
  geom_line(data = resta_list$Resultados.52, aes(x = 1:days, y = `75%`, 
                                      color = "5x2", 
                                      linetype = "75%"), group = 1, size = 0.75) +
  
  
  labs(color = "Esquema de cuarentena", linetype = "Percentil") +
  
  scale_linetype_manual(values = c("50%" = "solid", "25%" = "dashed", "75%" = "dotted")) +
  
  ggtitle("Esquemas lxm vs 6x1") +
  
  scale_x_continuous(limits = c(1, days), breaks = seq (0, days, 5)) +
  #scale_y_continuous(limits = c(-2000, 0)) +
  xlab("Días") +
  ylab("Casos que se podrían reducir") +
  
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        title = element_text(size = 18)) #+
#theme(legend.position = c(0.8, 0.3))
#dev.off()


#-----------------------------------------------------------------------------------------------
#                                         Proporción
#-----------------------------------------------------------------------------------------------

prop_list <- list()
for (i in 1:3) {
  
  prop <-  ((df_list[[i]]/df_list$ctf) - 1)*100
  
  #NaN = 0/0 hubo 0 reducción
  #Inf = Number/0 Hubo más casos en el esquema que en ctf 0 reducción
  
  
  prop <- prop %>%
             mutate_if(is.numeric, function(x) ifelse(!is.finite(x), 0, x))
  
  prop <- (t(apply(prop, 1, function (x) quantile(x, na.rm = TRUE, qest))))          
  
  prop_list[[i]] <- as.data.frame(prop)
  
  names(prop_list)[i] <- names(df_list)[i]
  rm(prop)
  
}



ggplot() +
  theme_classic() +
  
  geom_line(data = prop_list$Resultados.43, aes(x = 1:days, y = `50%`, 
                                                 color = "4x3", 
                                                 linetype = "50%"), group = 1, size = 0.75) +
  
  geom_line(data = prop_list$Resultados.43, aes(x = 1:days, y = `25%`, 
                                                 color = "4x3", 
                                                 linetype = "25%"), group = 1, size = 0.75) +
  
  geom_line(data = prop_list$Resultados.43, aes(x = 1:days, y = `75%`, 
                                                 color = "4x3", 
                                                 linetype = "75%"), group = 1, size = 0.75) +
  
  
  geom_line(data = prop_list$Resultados.410, aes(x = 1:days, y = `50%`, 
                                                  color = "4x10", 
                                                  linetype = "50%"), group = 1, size = 0.75) +
  
  
  geom_line(data = prop_list$Resultados.410, aes(x = 1:days, y = `25%`, 
                                                  color = "4x10", 
                                                  linetype = "25%"), group = 1, size = 0.75) +
  
  
  geom_line(data = prop_list$Resultados.410, aes(x = 1:days, y = `75%`, 
                                                  color = "4x10", 
                                                  linetype = "75%"), group = 1, size = 0.75) +
  
  
  geom_line(data = prop_list$Resultados.52, aes(x = 1:days, y = `50%`, 
                                                 color = "5x2", 
                                                 linetype = "50%"), group = 1, size = 0.75) +
  
  
  geom_line(data = prop_list$Resultados.52, aes(x = 1:days, y = `25%`, 
                                                 color = "5x2", 
                                                 linetype = "25%"), group = 1, size = 0.75) +
  
  geom_line(data = prop_list$Resultados.52, aes(x = 1:days, y = `75%`, 
                                                 color = "5x2", 
                                                 linetype = "75%"), group = 1, size = 0.75) +
  
  
  labs(color = "Esquema de cuarentena", linetype = "Percentil") +
  
  scale_linetype_manual(values = c("50%" = "solid", "25%" = "dashed", "75%" = "dotted")) +
  
  ggtitle("Esquemas lxm vs 6x1") +
  
  scale_x_continuous(limits = c(1, days), breaks = seq (0, days, 5)) +
  #scale_y_continuous(breaks = seq(-100, 0, 10)) +
  xlab("Días") +
  ylab("Porcentaje de casos que se podrían reducir") +
  
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        title = element_text(size = 18)) #+
#theme(legend.position = c(0.8, 0.3))
#dev.off()





















Results <- data.frame(((esquema/ctf)-1)*100)

#NaN = 0/0 hubo 0 reducción
#Inf = Number/0 Hubo más casos en el esquema que en ctf 0 reducción

prop_list <- Results %>%
  mutate_if(is.numeric, function(x) ifelse(!is.finite(x), 0, x))


Results52 <- (t(apply(Results, 1, function (x) quantile(x, na.rm = TRUE, qest))))          


