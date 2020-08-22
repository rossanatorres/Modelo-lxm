rm(list=ls())
library(readxl)
library(stringi)
library(survey)
library(lubridate)
library(tidyverse)


load("~/Dropbox/Modelo CDMX/Modelo en Github/Data/data_020820.rda")


days <- seq.Date(as.Date("2020-02-27"),
                 as.Date("2020-02-27") + 90, "day") #91 es el máximo de días que hemos simulado

REPORTADOS <- data_020820 %>%
             filter(RESDEFIN == "SARS-CoV-2", ENTIDAD == "CIUDAD DE MEXICO") %>%
             mutate(FECINGRE= as.Date(FECINGRE, format = "%Y-%m-%d"), 
                    POSITIVO = 1) %>%
             filter(FECINGRE >= "2020-02-27" & FECINGRE <= as.Date("2020-02-27") + 90)   %>%
             group_by(FECINGRE)  %>%
             summarise(CASOS = sum(POSITIVO)) %>%
             mutate(DAY = 1) %>%
             mutate(CASOS.ACUM = cumsum(CASOS))  %>%
             complete(FECINGRE = days,
             fill = list(CASOS = 0, CASOS.ACUM = 0, DAY = 1)) %>%
             mutate(DAY = cumsum(DAY))

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
#                                      Get distribution
#-----------------------------------------------------------------------------------------------

qest = c(0.25, 0.5, 0.75)         
casos_list <- list()
for (i in 1:4) {
  
  casos <- df_list[[i]] 
  casos <- (t(apply(casos, 1, function (x) quantile(x, na.rm = TRUE, qest))))          
  casos_list[[i]] <- as.data.frame(casos)
  names(casos_list)[i] <- names(df_list)[i]
  rm(casos)
  
}


ggplot() +
  
  theme_classic() +
  
  
  geom_line(data = REPORTADOS[1:days, ], aes(x = 1:days, y = CASOS.ACUM, 
                                             color = "Casos reportados en CDMX"), 
            linetype = "solid", group = 1, size = 0.75) +
  
  
  geom_line(data = casos_list$Resultados.43, aes(x = 1:days, y = `50%`, 
                                                 color = "4x3", 
                                                 linetype = "50%"), group = 1, size = 0.75) +
  
  geom_line(data = casos_list$Resultados.43, aes(x = 1:days, y = `25%`, 
                                                 color = "4x3", 
                                                 linetype = "25%"), group = 1, size = 0.75) +
  
  geom_line(data = casos_list$Resultados.43, aes(x = 1:days, y = `75%`, 
                                                 color = "4x3", 
                                                 linetype = "75%"), group = 1, size = 0.75) +
  
  
  geom_line(data = casos_list$Resultados.410, aes(x = 1:days, y = `50%`, 
                                                  color = "4x10", 
                                                  linetype = "50%"), group = 1, size = 0.75) +
  
  
  geom_line(data = casos_list$Resultados.410, aes(x = 1:days, y = `25%`, 
                                                  color = "4x10", 
                                                  linetype = "25%"), group = 1, size = 0.75) +
  
  
  geom_line(data = casos_list$Resultados.410, aes(x = 1:days, y = `75%`, 
                                                  color = "4x10", 
                                                  linetype = "75%"), group = 1, size = 0.75) +
  
  
  geom_line(data = casos_list$Resultados.52, aes(x = 1:days, y = `50%`, 
                                                 color = "5x2", 
                                                 linetype = "50%"), group = 1, size = 0.75) +
  
  
  geom_line(data = casos_list$Resultados.52, aes(x = 1:days, y = `25%`, 
                                                 color = "5x2", 
                                                 linetype = "25%"), group = 1, size = 0.75) +
  
  geom_line(data = casos_list$Resultados.52, aes(x = 1:days, y = `75%`, 
                                                 color = "5x2", 
                                                 linetype = "75%"), group = 1, size = 0.75) +
  
  
  geom_line(data = casos_list$ctf, aes(x = 1:days, y = `50%`, 
                                       color = "6x1", 
                                       linetype = "50%"), group = 1, size = 0.75) +
  
  
  geom_line(data = casos_list$ctf, aes(x = 1:days, y = `25%`, 
                                       color = "6x1", 
                                       linetype = "25%"), group = 1, size = 0.75) +
  
  geom_line(data = casos_list$ctf, aes(x = 1:days, y = `75%`, 
                                       color = "6x1", 
                                       linetype = "75%"), group = 1, size = 0.75) +
  
  
  labs(color = "Esquema de cuarentena", linetype = "Percentil") +
  
  scale_linetype_manual(values = c("50%" = "solid", "25%" = "dashed", "75%" = "dotted")) +
  
  ggtitle("Esquemas lxm") +
  
  scale_x_continuous(limits = c(1, days), breaks = seq (0, days, 10)) +
  #scale_y_continuous(limits = c(0, 1000)) +
  xlab("Días") +
  ylab("Casos acumulados") +
  
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        title = element_text(size = 18))

