rm(list=ls())
# Unificar ENE-DT-OHL -----------------------------------------------------

pacman::p_load(tidyverse,
               readxl,
               writexl,
               sjmisc)


# Cargar datos ------------------------------------------------------------

ene = read_xlsx("output/data/ene_final.xlsx")
unempl = read_xlsx("output/data/ene_unempl.xlsx")
sheet = rep(paste0("Sheet", 1:9))

ene = map(sheet,
          ~read_xlsx("output/data/ene_final.xlsx",
                     sheet = .x)) %>% 
  reduce(full_join, by = c("ano", "ciiu")) %>% 
  arrange(ano, ciiu) %>% 
  mutate(self_empl = ifelse(ciiu %in% "4. Suministro de electricidad, gas y agua" & ano %in% "1998", 
                            0, self_empl)) %>% 
  merge(., unempl, by = "ano", all = T) %>% 
  rowwise() %>% 
  mutate(across(c(female, self_empl, skills, contract_duration, 
                  firm_size, contract_type),
                ~ifelse(!is.na(.), round((./total)*100,3), NA))) %>% 
  ungroup() %>% 
  # arrange(ciiu, ano) %>% 
  # mutate(across(c(female, self_empl, skills, contract_duration, 
  #                 firm_size, job_seniority, contract_type, unempl),
  #               ~lag(.),
  #               .names = "lag_{.col}")) %>% 
  arrange(ano, ciiu) %>% 
  filter(ano != "1998")

dtohl = read_xlsx("output/data/data_graficos_dtohl.xlsx")

data = merge(ene, dtohl %>% select(ciiu = actividad_raw, everything(), -c(lag_afiliados, lag_ocupados_autoemp, ocupados_autoemp,
                                                                          lag_tasa_sind, lag_tasa_sind_per, lag_n_sind, lag_n_sind_mil)),
             by = c("ano", "ciiu"), all = T) #%>% 
  # group_by(ano, ciiu) %>% 
  # mutate(across(c(afiliados, n_huelgas, lag_afiliados, lag_n_huelgas),
  #               ~ifelse(ciiu %in% "Total", cumsum(.), .)),
  #        across(c(tasa_sind, tasa_sind_per, n_sind, n_sind_mil, dptp, tc,
  #                 lag_tasa_sind, lag_tasa_sind_per, lag_n_sind, lag_n_sind_mil, 
  #                 lag_dptp, lag_tc),
  #               ~ ifelse(ciiu %in% "Total", mean(., na.rm=T), .)))

write_xlsx(data, "output/data/data_final.xlsx")

