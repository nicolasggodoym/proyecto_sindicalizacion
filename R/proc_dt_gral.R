rm(list=ls())

# Rev datos DT - total ----------------------------------------------------

# Cargar paquetes ---------------------------------------------------------
pacman::p_load(tidyverse, sjmisc, readxl, data.table)

# Cargar data -------------------------------------------------------------

# NC y Huelgas ------------------------------------------------------------
files = list.files("input/data/dt", pattern = ".rds")[2:4]

nc_h = map(files, ~readRDS(paste0("input/data/dt/", .x)))

names(nc_h) = str_remove(files, pattern = ".rds$")

list2env(nc_h, globalenv())
rm(nc_h)

# OOSS --------------------------------------------------------------------

ooss = readRDS("input/data/dt/ooss.rds")

# Estimaciones ------------------------------------------------------------


# Número de trabajadores --------------------------------------------------

h_nt = huelgas_ntrab_dt %>% 
  group_by(rut_empresa, rsu, fecha) %>% 
  mutate(n = 1:n()) %>% 
  filter(n==1) %>% 
  ungroup() %>% 
  group_by(ID, ano) %>% 
  summarise(n_huelgas = n(),
            n_trab_huelga_tot = sum(trab_tot_h, na.rm = T),
            n_trab_huelga_fem = sum(trab_fem_h, na.rm = T),
            n_trab_huelga_mas = sum(trab_mas_h, na.rm = T),
            mean_trab_huelga_tot = mean(trab_tot_h, na.rm = T),
            mean_trab_huelga_fem = mean(trab_fem_h, na.rm = T),
            mean_trab_huelga_mas = mean(trab_mas_h, na.rm = T)) %>% 
  filter(!is.na(ID))

h_dur = huelgas_dur_dt %>% 
  group_by(rut_empresa, rsu, fecha) %>% 
  mutate(n = 1:n()) %>% 
  filter(n==1) %>% 
  ungroup() %>% 
  group_by(ID, ano) %>% 
  summarise(huelgas_dur = mean(dias_h, na.rm = T),
            n_dptp = sum(trab_tot_h*dias_h),
            mean_dptp = mean(trab_tot_h*dias_h)) %>% 
  filter(!is.na(ID))
  
nc = neg_col_dt %>% 
  group_by(rut_empresa, rsu, ano) %>% 
  mutate(n = 1:n()) %>% 
  filter(n==1) %>% 
  ungroup() %>% 
  group_by(ID, ano) %>% 
  summarise(cubiertos_tot = sum(trab_tot_nc, na.rm = T),
            cubiertos_fem = sum(trab_fem_nc, na.rm = T),
            cubiertos_mas = sum(trab_mas_nc, na.rm = T)) %>% 
  filter(!is.na(ID))

nc_contrato = neg_col_dt %>% 
  group_by(rut_empresa, rsu, ano) %>% 
  mutate(n = 1:n()) %>% 
  filter(n==1 & tipo_inst == "Contrato Colectivo") %>% 
  ungroup() %>% 
  group_by(ID, ano) %>% 
  summarise(cubiertos_cont = sum(trab_tot_nc, na.rm = T),
            cubiertos_cont_fem = sum(trab_fem_nc, na.rm = T),
            cubiertos_cont_mas = sum(trab_mas_nc, na.rm = T)) %>% 
  filter(!is.na(ID))

nc_otro = neg_col_dt %>% 
  group_by(rut_empresa, rsu, ano) %>% 
  mutate(n = 1:n()) %>% 
  filter(n==1 & tipo_inst != "Contrato Colectivo") %>% 
  ungroup() %>% 
  group_by(ID, ano) %>% 
  summarise(cubiertos_otro = sum(trab_tot_nc, na.rm = T),
            cubiertos_otro_fem = sum(trab_fem_nc, na.rm = T),
            cubiertos_otro_mas = sum(trab_mas_nc, na.rm = T)) %>% 
  filter(!is.na(ID))

sind = ooss %>% 
  group_by(rut_empresa, rsu, ano) %>% 
  mutate(n = 1:n()) %>% 
  filter(n==1) %>% 
  ungroup() %>% 
  group_by(ID, ano) %>% 
  summarise(n_sind = n(),
            n_afil_tot = sum(afil_tot, na.rm = T),
            n_afil_mas = sum(afil_mas, na.rm = T),
            n_afil_fem = sum(afil_fem, na.rm = T),
            mean_afil_tot = mean(afil_tot, na.rm = T),
            mean_afil_mas = mean(afil_mas, na.rm = T),
            mean_afil_fem = mean(afil_fem, na.rm = T),
            por_dir_fem = round(sum(n_dir_mas<n_dir_tot, na.rm = T)/n()*100,3),
            por_fed = round(sum(!is.na(rsu_fed))/n()*100, 3),
            por_conf = round(sum(!is.na(rsu_conf))/n()*100, 3),
            por_cen = round(sum(!is.na(rsu_cen))/n()*100, 3)) %>% 
  filter(!is.na(ID))


data = list(nc, nc_contrato, nc_otro, h_nt, h_dur, sind) %>% 
  reduce(full_join, by = c("ID", "ano"))

saveRDS(data, "output/data/data_dt_proc_final.rds")
