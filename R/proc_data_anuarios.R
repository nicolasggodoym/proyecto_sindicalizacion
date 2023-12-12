rm(list=ls())


# Procesamiento data anuarios DT ------------------------------------------


# Cargar paquetes ----------------------------------------------------------
pacman::p_load(tidyverse, readxl, sjmisc)

# Cargar datos ------------------------------------------------------------
afiliados = read_xlsx("input/data/anuario_dt.xlsx", 
                      sheet = "total_sind_original") %>% 
  filter(ano>2010 & actividad != "Total") #Sirve desde 2011 por CAE

#ft uso la de la ENE

nsind = read_xlsx("input/data/anuario_dt.xlsx", 
                  sheet = "nsind") %>% 
  filter(ano>2010 & actividad != "Total") #Sirve desde 2011 por CAE

llave = read_xlsx("input/data/anuario_dt.xlsx", 
                  sheet = "rev3-rev4") %>% 
  mutate(across(c(actividad3, actividad4),
                ~str_extract(., "\\d{1,2}")))

# Procesar ----------------------------------------------------------------

data = merge(afiliados %>% select(-c(ocupados, ano_actividad, actividad_sintesis)), 
             nsind %>% select(-c(ano_actividad, actividad_sintesis)),
             by = c("ano", "actividad"), all = T) %>% 
  mutate(actividad3 = ifelse(ano < 2018, str_extract(actividad, "\\d{1,2}"), NA_character_),
         actividad4 = ifelse(ano >= 2018, str_extract(actividad, "\\d{1,2}"), NA_character_)) %>% 
  merge(.,
        llave %>% select(actividad3, CAENES_1d3=CAENES_1d) %>% unique,
        by = "actividad3", all.x = T) %>% 
  merge(.,
        llave %>% select(actividad4, CAENES_1d4=CAENES_1d) %>% unique,
        by = "actividad4", all.x = T) %>% 
  mutate(CAENES_1d = ifelse(!is.na(CAENES_1d3), CAENES_1d3, CAENES_1d4)) %>% 
  select(ano, CAENES_1d, afiliados, n_sindicatos) %>% 
  na.omit() %>% 
  group_by(CAENES_1d, ano) %>% 
  summarise(across(c(afiliados, n_sindicatos), ~sum(., na.rm=T))) %>% 
  ungroup() 

  
saveRDS(data, "input/data/dt/datos_anuario_armonizado.rds")
