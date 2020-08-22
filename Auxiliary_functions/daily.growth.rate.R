rm(list=ls())
library(readxl)
library(tidyverse)

load("~/Dropbox/Modelo CDMX/Modelo en Github/Data/data_020820.rda")

COVID <- data_020820 %>% 
  filter(RESDEFIN == "SARS-CoV-2", ENTIDAD == "CIUDAD DE MEXICO") %>% 
  mutate(FECHA_INGRESO = as.Date(FECINGRE, format = "%Y-%m-%d"),
         COUNT =rep(1, nrow(.))) %>%
  group_by(FECHA_INGRESO) %>%
  summarise(COUNT = sum(COUNT)) %>%
  ungroup(.) %>%
  mutate(CASES = cumsum(COUNT), DAYS =rep(1, nrow(.)), DAYS.AC = cumsum(DAYS)) 

COVID$growth.rate <- (1/as.numeric(COVID$FECHA_INGRESO - lag(COVID$FECHA_INGRESO)))*log(COVID$CASES/lag(COVID$CASES))

growth.rate <- COVID$growth.rate 

save(growth.rate, file = "~/Dropbox/Modelo CDMX/Modelo en Github/lambda_estimates/growth_rate.rda")



#Completar fechas sin casos
days <- seq.Date(as.Date("2020-02-27"),
                 as.Date("2020-02-27") + 90, "day") #91 es el máximo de días que hemos simulado

COVID.complete <- COVID %>%
         complete(FECHA_INGRESO = days,
           fill = list(COUNT = 0, CASES = 0, DAYS = 1, growth.rate = 0)) %>%
         mutate(DAY = cumsum(DAYS)) 


