rm(list=ls())

# Rev datos DT - total ----------------------------------------------------


# Cargar paquetes ---------------------------------------------------------
pacman::p_load(tidyverse, sjmisc, readxl, data.table)

# Cargar data -------------------------------------------------------------
files = list.files("input/data/dt", pattern = ".rds")

data = map(files, ~readRDS(paste0("input/data/dt/", .x)))

names(data) = str_remove(files, pattern = ".rds$")

llave_cae = read_xlsx("input/data/dt/CAE_DT_armonizado.xlsx") %>% 
  select(id_cae2, ID)

sii = data[["CAE_RUT_FINAL"]] %>% 
  group_by()

names(sii)

a=sii %>% 
  mutate(rut = paste0(rut, "-", dv)) %>% 
  select(rut, id_cae2) %>% 
  merge(., llave_cae, all.x = T, by = "id_cae2") %>% 
  group_by(rut) %>% 
  summarise(id_cae2 = n_distinct(id_cae2),
            ID = n_distinct(ID)) %>% 
  as.data.table()

a = as.data.table()

descr=a[, .(media_id_cae2 = mean(id_cae2),
          cuartil_25_id_cae2 = quantile(id_cae2, 0.25),
          cuartil_50_id_cae2 = quantile(id_cae2, 0.5),
          cuartil_75_id_cae2 = quantile(id_cae2, 0.75),
          min_id_cae2 = min(id_cae2),
          max_id_cae2 = max(id_cae2),
          media_ID = mean(ID),
          cuartil_25_ID = quantile(ID, 0.25),
          cuartil_50_ID = quantile(ID, 0.5),
          cuartil_75_ID = quantile(ID, 0.75),
          min_ID = min(ID),
          max_ID = max(ID))]

descr %>% 
  pivot_longer(everything()) %>% 
  mutate(stat = str_remove(str_extract(name, pattern = "^.*(?=_)"), "_id"),
         name = ifelse(str_detect(name, "id_cae2"), "SII", "Armonizada")) %>% 
  pivot_wider(id_cols = "name",
              names_from = "stat",
              values_from = "value")

names(data[["ooss_cen"]])

data[["ooss_micro"]] %>% 
  mutate(rut_empresa = paste0(rut_empresa, dv)) %>% 
  group_by(rut_empresa, rsu, ano, region, tipo_org, afil_tot) %>% 
  mutate(n=n()) %>% 
  filter(n>1)

nrow(data[["ooss_micro"]])

sum(is.na(data[["ooss_micro"]]))
