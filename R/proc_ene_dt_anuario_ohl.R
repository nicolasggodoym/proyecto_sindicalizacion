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
                ~ifelse(is.na(.), 0, .)))
