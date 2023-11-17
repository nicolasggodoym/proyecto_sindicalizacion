rm(list=ls())

# Rev datos DT - total ----------------------------------------------------


# Cargar paquetes ---------------------------------------------------------
pacman::p_load(tidyverse, sjmisc)

# Cargar data -------------------------------------------------------------
files = list.files("input/data/dt", pattern = ".rds")

data = map(files, ~readRDS(paste0("input/data/dt/", .x)))

names(data) = str_remove(files, pattern = ".rds$")

names(data)

names(data[["ooss_cen"]])

data[["ooss_micro"]] %>% 
  mutate(rut_empresa = paste0(rut_empresa, dv)) %>% 
  group_by(rut_empresa, rsu, ano, region, tipo_org, afil_tot) %>% 
  mutate(n=n()) %>% 
  filter(n>1)

nrow(data[["ooss_micro"]])

sum(is.na(data[["ooss_micro"]]))
