rm(list=ls())

# Revisión CAE empresas DT ------------------------------------------------


# Cargar paquetes ---------------------------------------------------------
pacman::p_load(tidyverse,
               haven,
               writexl,
               sjmisc,
               data.table)

# Cargar datos ------------------------------------------------------------
data = read_dta("input/data/CAE_RUT_FINAL.dta")

# Revisar -----------------------------------------------------------------
proc = data %>% 
  mutate(across(c(orden, cae_2, id_cae2), ~haven::as_factor(.))) %>% 
  data.table

cae_2 = proc[,c("orden", "cae_2", "id_cae2")
    ][,.N, keyby = cae_2] %>% 
  mutate(id_cae2 = 1:n()) %>% 
  select(id_cae2, cae_2, N)

write_xlsx(cae_2, "output/data/CAE_DT.xlsx")

id_cae2 = proc[,c("orden", "cae_2", "id_cae2")
][,.N, keyby = id_cae2] 
