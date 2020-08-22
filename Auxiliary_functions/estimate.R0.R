rm(list=ls())
library(readxl)
library(tidyverse)
library(R0)

load("~/Dropbox/Modelo CDMX/Modelo en Github/Data/data_020820.rda")

COVID <- data_020820 %>% 
  filter(RESDEFIN == "SARS-CoV-2", ENTIDAD == "CIUDAD DE MEXICO") %>% 
  mutate(FECHA_INGRESO = as.Date(FECINGRE, format = "%Y-%m-%d"),
         COUNT =rep(1, nrow(.))) %>%
  group_by(FECHA_INGRESO) %>%
  summarise(COUNT = sum(COUNT)) %>%
  ungroup(.) %>%
  mutate(CASES = cumsum(COUNT), DAYS =rep(1, nrow(.)), DAYS.AC = cumsum(DAYS)) 


days <- 91 #Máximo de días que hemos simulado

epid <- COVID$CASES[1:days]
GT.flu <-generation.time("lognormal", c(2.14, 2.44))  #https://covid-19.conacyt.mx/jspui/bitstream/1000/805/1/102441.pdf
                                                      #Tabla S3

res.R <-estimate.R(epid, GT=GT.flu, methods=c("TD"), pop.size = 5018) #Checar ese 5018

Rt <- as.numeric(res.R$estimates$TD$R)


save(Rt, file = "~/Dropbox/Modelo CDMX/Modelo en Github/lambda_estimates/Rt_GTlognormal.rda")

sensitivity.analysis(epid, GT.flu, begin=1,end=91, est.method="EG", sa.type="time")


