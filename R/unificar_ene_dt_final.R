rm(list=ls())


# Unificar data ENE - DT (OOSS-NC-H) --------------------------------------


# Cargar paquetes ---------------------------------------------------------
pacman::p_load(tidyverse, sjmisc, writexl)

# Cargar data -------------------------------------------------------------
dt = readRDS("output/data/data_dt_proc_final.rds")
  
ene = readRDS("output/data/ene_final_CAENES_1d.rds") %>% 
  mutate(CAENES_1d = factor(CAENES_1d),
         ano = as.numeric(ano)) %>%
  filter(!is.na(CAENES_1d) & CAENES_1d != "Total")

olas = data.frame(CAENES_1d = factor(rep(levels(ene$CAENES_1d)[1:14], 14)),
                  ano = map(2010:2023, ~rep(.x, 14)) %>% unlist())

# Unificar ----------------------------------------------------------------
data = list(ene, dt) %>% 
  reduce(full_join, by = c("CAENES_1d", "ano")) %>% 
  merge(olas,
        ., 
        by = c("ano", "CAENES_1d"), all = T) %>% 
  arrange(ano, CAENES_1d) %>% 
  filter(ano %in% 2010:2023 & CAENES_1d != "T") %>% 
  mutate(across(total:por_cen,
                ~tidyr::replace_na(., 0)),
         tasa_afiliacion = ifelse(if_all(c(n_afil_tot, total), ~.!=0), round((n_afil_tot/total)*100,3), 0),
         por_cobertura = ifelse(if_all(c(cubiertos_tot, total), ~.!=0), round((cubiertos_tot/total)*100,3), 0),
         por_cobertura_cont = ifelse(if_all(c(cubiertos_cont, total), ~.!=0), round((cubiertos_cont/total)*100,3), 0),
         por_cobertura_otro = ifelse(if_all(c(cubiertos_otro, total), ~.!=0), round((cubiertos_otro/total)*100,3), 0),
         n_sind_mil = ifelse(if_all(c(n_sind, total), ~.!=0), round((n_sind/(total/1000))*100,3), 0)) %>% 
  arrange(CAENES_1d, ano) %>%
  #group_by(CAENES_1d, ano) %>% 
  mutate(across(c(tasa_afiliacion, por_cobertura, por_cobertura_cont, por_cobertura_otro),
                ~ifelse(ano %in% 2011:2023, dplyr::lag(.), 0),
                .names = "lag_{.col}")) %>%
  ungroup()

# Exportar ----------------------------------------------------------------
saveRDS(data,"output/data/data_dt_ene_final.rds")



# Explorar bugs -----------------------------------------------------------

#% mayores a 100

data %>% 
  summarise(across(c(tasa_afiliacion, por_cobertura, por_cobertura_cont),
                   ~sum(. > 100)))

data %>% 
  filter(if_any(c(tasa_afiliacion, por_cobertura, por_cobertura_cont),
                ~.>100)) %>% 
  select(CAENES_1d, ano, 
         total, n_afil_tot, tasa_afiliacion,
         cubiertos_tot, por_cobertura,
         cubiertos_cont, por_cobertura_cont,
         n_sind) %>% 
  write_xlsx("rev_porcentajes.xlsx")

ids=data %>% 
  filter(if_any(c(tasa_afiliacion, por_cobertura, por_cobertura_cont),
                ~.>100)) %>% 
  select(CAENES_1d) %>% unique() %>% pull()

j=data %>% 
  filter(CAENES_1d %in% ids) %>% 
  select(CAENES_1d, ano, 
         total, n_afil_tot, tasa_afiliacion,
         cubiertos_tot, por_cobertura,
         cubiertos_cont, por_cobertura_cont,
         n_sind)
  
  
  
  
  
  
  
  




























