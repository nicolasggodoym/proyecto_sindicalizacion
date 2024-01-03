rm(list = ls())


# Proc final data ENE, DT (Anuario) y OHL -------------------------------------------

# Cargar paquetes ---------------------------------------------------------
pacman::p_load(tidyverse, sjmisc)

# Cargar datos ------------------------------------------------------------
ene = readRDS("output/data/ene_final_CAENES_1d.rds") %>% 
  mutate(CAENES_1d = factor(CAENES_1d),
         ano = as.numeric(ano)) %>%
  filter(!is.na(CAENES_1d) & !CAENES_1d %in% c("Total", "T"))

ohl = readRDS("output/data/ohl.rds") %>% 
  group_by(CAENES_1d, ano) %>% 
  summarise(across(c(dptp, tc, duracion),
                   ~mean(., na.rm=T)),
            n_huelga = sum(n_huelga, na.rm=T)) %>% 
  filter(!is.na(CAENES_1d) & !CAENES_1d %in% c("T"))

dt = readRDS("input/data/dt/datos_anuario_armonizado.rds") %>% 
  filter(!is.na(CAENES_1d) & CAENES_1d != "T")


# Unificar ----------------------------------------------------------------

data = list(ene, ohl, dt) %>% 
  reduce(full_join, by = c("ano", "CAENES_1d")) %>% 
  filter(ano > 2010) %>% 
  mutate(across(total:n_sindicatos,
                ~ifelse(is.na(.), 0, .))) %>% 
  arrange(ano, CAENES_1d) %>% 
  mutate(tasa_afiliacion = ifelse(if_all(c(afiliados, total), ~.!=0), round((afiliados/total)*100,3), 0),
         n_sindicatos = ifelse(if_all(c(n_sindicatos, total), ~.!=0), round((n_sindicatos/(total/1000)),3), 0)) %>% 
  arrange(CAENES_1d, ano) %>%
  #group_by(CAENES_1d, ano) %>% 
  mutate(across(c(tasa_afiliacion, n_sindicatos, n_huelga, duracion, tc, dptp),
                ~ifelse(ano %in% 2011:2023, dplyr::lag(.), 0),
                .names = "lag_{.col}"),
         t = case_when(ano %in% 2011:2015 ~ "2011-2015", 
                       ano %in% 2011:2015 ~ "2016-2019", 
                       TRUE ~ "2020-2023")) %>%
  ungroup() %>% 
  select(ano, t, CAENES_1d, total:lag_dptp)

saveRDS(data, "output/data/ene_ohl_anuario.rds")
