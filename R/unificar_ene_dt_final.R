rm(list=ls())


# Unificar data ENE - DT (OOSS-NC-H) --------------------------------------


# Cargar paquetes ---------------------------------------------------------
pacman::p_load(tidyverse, sjmisc)

# Cargar data -------------------------------------------------------------
dt = readRDS("output/data/data_dt_proc_final.rds")
  
ene = readRDS("output/data/ene_final_ID.rds") %>% 
  mutate(ID = as.numeric(ID),
         ano = as.numeric(ano)) %>% 
  filter(!is.na(ID))

# Unificar ----------------------------------------------------------------
data = list(ene, dt) %>% 
  reduce(full_join, by = c("ID", "ano")) %>% 
  arrange(ano, ID) %>% 
  filter(ano %in% 2010:2023) %>% 
  mutate(across(total:por_cen,
                ~tidyr::replace_na(., 0)),
         tasa_afiliacion = ifelse(if_all(c(n_afil_tot, total), ~.!=0), round((n_afil_tot/total)*100,3), 0),
         por_cobertura = ifelse(if_all(c(cubiertos_tot, total), ~.!=0), round((cubiertos_tot/total)*100,3), 0),
         por_cobertura_cont = ifelse(if_all(c(cubiertos_cont, total), ~.!=0), round((cubiertos_cont/total)*100,3), 0),
         por_cobertura_otro = ifelse(if_all(c(cubiertos_otro, total), ~.!=0), round((cubiertos_otro/total)*100,3), 0),
         n_sind_mil = ifelse(if_all(c(n_sind, total), ~.!=0), round((n_sind/(total/1000))*100,3), 0)) %>% 
  arrange(ID, ano) %>%
  #group_by(ID, ano) %>% 
  mutate(across(c(tasa_afiliacion, por_cobertura, por_cobertura_cont, por_cobertura_otro),
                ~ifelse(ano %in% 2011:2023, dplyr::lag(.), 0),
                .names = "lag_{.col}")) %>%
  ungroup()

# Exportar ----------------------------------------------------------------
saveRDS(data,"output/data/data_dt_ene_final.rds")
