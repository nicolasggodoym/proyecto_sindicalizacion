rm(list=ls())
# Unificar ENE-DT-OHL -----------------------------------------------------

pacman::p_load(tidyverse,
               readxl,
               writexl,
               sjmisc)


# Cargar datos ------------------------------------------------------------

ene = read_xlsx("output/data/ene_final.xlsx")
#unempl = read_xlsx("output/data/ene_unempl.xlsx")
sheet = rep(paste0("Sheet", 1:9))

ene = readRDS("output/data/ene_final_CAENES_1d.rds") %>% 
  arrange(ano, CAENES_1d) %>% 
  mutate(self_empl = ifelse(CAENES_1d %in% "DE" & ano %in% "1998", 
                            0, self_empl)) %>% 
  #merge(., unempl, by = "ano", all = T) %>% 
  rowwise() %>% 
  mutate(across(c(female, self_empl, skills, contract_duration, 
                  firm_size, contract_type),
                ~ifelse(!is.na(.), round((./total)*100,3), NA))) %>% 
  ungroup() %>% 
  # arrange(CAENES_1d, ano) %>% 
  # mutate(across(c(female, self_empl, skills, contract_duration, 
  #                 firm_size, job_seniority, contract_type, unempl),
  #               ~lag(.),
  #               .names = "lag_{.col}")) %>% 
  arrange(ano, CAENES_1d) %>% 
  filter(ano != "1998")

#en analisis_dt_ohl.R
dtohl = read_xlsx("output/data/data_graficos_dtohl.xlsx")

data = merge(ene, dtohl,
             by = c("ano", "CAENES_1d"), all = T) #%>% 
  # group_by(ano, CAENES_1d) %>% 
  # mutate(across(c(afiliados, n_huelgas, lag_afiliados, lag_n_huelgas),
  #               ~ifelse(CAENES_1d %in% "Total", cumsum(.), .)),
  #        across(c(tasa_sind, tasa_sind_per, n_sind, n_sind_mil, dptp, tc,
  #                 lag_tasa_sind, lag_tasa_sind_per, lag_n_sind, lag_n_sind_mil, 
  #                 lag_dptp, lag_tc),
  #               ~ ifelse(CAENES_1d %in% "Total", mean(., na.rm=T), .)))

write_xlsx(data, "output/data/data_final.xlsx")

