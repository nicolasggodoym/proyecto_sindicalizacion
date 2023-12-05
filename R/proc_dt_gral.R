rm(list=ls())

# Rev datos DT - total ----------------------------------------------------

# Cargar paquetes ---------------------------------------------------------
pacman::p_load(tidyverse, sjmisc, readxl, data.table)

# Cargar data -------------------------------------------------------------
# llave = read_xlsx("input/data/dt/CAE_DT_armonizado.xlsx") %>% 
#   select(ID, ID2) %>% 
#   unique

# NC y Huelgas ------------------------------------------------------------
files = list.files("input/data/dt", pattern = ".rds")[2:4]

nc_h = map(files, ~readRDS(paste0("input/data/dt/", .x)) #%>% merge(., llave, by = "ID", all.x = T)
           )

names(nc_h) = str_remove(files, pattern = ".rds$")

list2env(nc_h, globalenv())
rm(nc_h)



# OOSS --------------------------------------------------------------------

ooss = readRDS("input/data/dt/ooss.rds") #%>% merge(., llave,
        #by = "ID", all.x = T)

# Estimaciones ------------------------------------------------------------


# Número de trabajadores --------------------------------------------------

h_nt = huelgas_ntrab_dt %>% 
  group_by(rut_empresa, rsu, codigoactividad, ano, fecha, trab_mas_h, trab_fem_h, 
           trab_tot_h, tipo_org_h, codigoactividad_sii, ID2) %>% 
  mutate(n = 1:n()) %>% 
  filter(n==1) %>% 
  ungroup() %>% 
  group_by(ID2, ano) %>% 
  summarise(n_huelgas = n(),
            n_trab_huelga_tot = sum(trab_tot_h, na.rm = T),
            n_trab_huelga_fem = sum(trab_fem_h, na.rm = T),
            n_trab_huelga_mas = sum(trab_mas_h, na.rm = T),
            mean_trab_huelga_tot = mean(trab_tot_h, na.rm = T),
            mean_trab_huelga_fem = mean(trab_fem_h, na.rm = T),
            mean_trab_huelga_mas = mean(trab_mas_h, na.rm = T)) %>% 
  filter(!is.na(ID2))

h_dur = huelgas_dur_dt %>% 
  group_by(rut_empresa, rsu, codigoactividad, ano, fecha, dias_h, resultado_neg_h, trab_mas_h,
           trab_fem_h, trab_tot_h, codigoactividad_sii, ID2) %>% 
  mutate(n = 1:n()) %>% 
  filter(n==1) %>% 
  ungroup() %>% 
  group_by(ID2, ano) %>% 
  summarise(huelgas_dur = mean(dias_h, na.rm = T),
            n_dptp = sum(trab_tot_h*dias_h),
            mean_dptp = mean(trab_tot_h*dias_h)) %>% 
  filter(!is.na(ID2))
  
nc = neg_col_dt %>% 
  group_by(rut_empresa, rsu, codigoactividad, ano, trab_empresa_nc, tipo_inst, tipo_neg,
           trab_mas_nc, trab_fem_nc, trab_tot_nc, codigoactividad_sii, ID2) %>% 
  mutate(n = 1:n()) %>% 
  filter(n==1) %>% 
  ungroup() %>% 
  group_by(ID2, ano) %>% 
  filter(trab_tot_nc < quantile(trab_tot_nc, .99, na.rm=T)) %>% 
  summarise(cubiertos_tot = sum(trab_tot_nc, na.rm = T),
            cubiertos_fem = sum(trab_fem_nc, na.rm = T),
            cubiertos_mas = sum(trab_mas_nc, na.rm = T)) %>% 
  filter(!is.na(ID2))

nc_contrato = neg_col_dt %>% 
  group_by(rut_empresa, rsu, codigoactividad, ano, trab_empresa_nc, tipo_inst, tipo_neg,
           trab_mas_nc, trab_fem_nc, trab_tot_nc, codigoactividad_sii, ID2) %>% 
  mutate(n = 1:n()) %>% 
  filter(n==1 & tipo_inst == "Contrato Colectivo") %>% 
  ungroup() %>% 
  group_by(ID2, ano) %>% 
  filter(trab_tot_nc < quantile(trab_tot_nc, .99, na.rm=T)) %>% 
  summarise(cubiertos_cont = sum(trab_tot_nc, na.rm = T),
            cubiertos_cont_fem = sum(trab_fem_nc, na.rm = T),
            cubiertos_cont_mas = sum(trab_mas_nc, na.rm = T)) %>% 
  filter(!is.na(ID2))

nc_otro = neg_col_dt %>% 
  group_by(rut_empresa, rsu, codigoactividad, ano, trab_empresa_nc, tipo_inst, tipo_neg,
           trab_mas_nc, trab_fem_nc, trab_tot_nc, codigoactividad_sii, ID2) %>% 
  mutate(n = 1:n()) %>% 
  filter(n==1 & tipo_inst != "Contrato Colectivo") %>% 
  ungroup() %>% 
  group_by(ID2, ano) %>% 
  filter(trab_tot_nc < quantile(trab_tot_nc, .975, na.rm=T)) %>% 
  summarise(cubiertos_otro = sum(trab_tot_nc, na.rm = T),
            cubiertos_otro_fem = sum(trab_fem_nc, na.rm = T),
            cubiertos_otro_mas = sum(trab_mas_nc, na.rm = T)) %>% 
  filter(!is.na(ID2))

sind = ooss %>% 
  select(-c(cae_1d)) %>% 
  group_by(rsu, ano, rut_empresa) %>% 
  mutate(n = 1:n()) %>% 
  filter(n==1 & !tipo_org %in% c("Asociacion de funcionarios", "ASOCIACION DE FUNCIONARIOS")) %>% 
  ungroup() %>% 
  group_by(ID2, ano) %>% 
  filter(afil_tot < quantile(afil_tot, .975, na.rm=T)) %>% 
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
  ungroup() %>% 
  mutate(ID2 = factor(ID2)) %>% 
  filter(!is.na(ID2))


data = list(nc, nc_contrato, nc_otro, h_nt, h_dur, sind) %>% 
  reduce(full_join, by = c("ID2", "ano"))

saveRDS(data, "output/data/data_dt_proc_final.rds")
