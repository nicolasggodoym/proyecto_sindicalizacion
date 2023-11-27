rm(list = ls())

# Procesamiento data NC y Huelgas DT --------------------------------------

# Cargar paquetes ---------------------------------------------------------
pacman::p_load(tidyverse, readxl, sjmisc)

# Cargar datos ------------------------------------------------------------

llave = readRDS("input/data/dt/CAE_RUT_FINAL.rds") %>%
  select(rut_empresa = rut, dv, id_cae2) %>% 
  mutate(across(c(rut_empresa, dv), ~as.character(.)),
         id_cae2 = as.numeric(id_cae2), #rut_empresa = paste(rut_empresa, dv, sep = "-")
         ) %>% #con duplicados: 3.482.146
  group_by(rut_empresa) %>% 
  mutate(n = 1:n()) %>% 
  ungroup() %>% 
  filter(n==1) %>% #Sin duplicados: 1.617.697
  select(-dv, -n)

llave = merge(llave, read_xlsx("input/data/dt/CAE_DT_armonizado.xlsx") %>% 
                select(id_cae2, ID),
              by = "id_cae2", all.x = T)
  

#Cuadro 1: Listado de instrumentos colectivos y acuerdo de grupo negociador que 
#inician vigencia. Registrados desde 01 enero 1999 al 04 octubre 2023
#n=71.954
hoja1 = read_xlsx("input/data/dt/1590_NC_y_Huelgas_2005_2023.xlsx",
                 sheet = "Cuadro 1", range = "B4:R71958",
                na = "NULL") %>% 
  janitor::clean_names() %>% 
  select(rut_empresa, rsu, 
         cae_dt = codigo_cae,
         ano_inicio_nc = ano_inicio, 
         fecha_inicio_nc = fecha_inicio_vigencia,
         trab_empresa_nc = trab_empresa, region_nc = region,
         tipo_inst = tipo_instrumento_colectivo_y_acuerdo_grupo_negociador,
         tipo_neg = tipo_negociacion_colectiva,
         trab_mas_nc = trabajadores_hombres_involucrados,
         trab_fem_nc = trabajadores_mujeres_involucradas,
         trab_tot_nc = trabajadores_total_involucrados) %>% 
  mutate(rut_empresa = as.character(rut_empresa)) %>% #antes merge 71.954
  merge(., llave, by = "rut_empresa", all.x = T) #despues merge 72.158


hoja1 %>% 
  mutate(info = case_when(is.na(id_cae2) & !is.na(cae_dt) ~ "Sólo DT",
                          !is.na(id_cae2) & is.na(cae_dt) ~ "Sólo SII",
                          !is.na(id_cae2) & !is.na(cae_dt) ~ "Ambas",
                          TRUE ~ "Ninguna")) %>% 
  group_by(info) %>%
  count %>% 
  mutate(prop = round(n/nrow(hoja1)*100, 3))

hoja1 %>% 
  group_by(rut_empresa) %>% 
  summarise(sii = n_distinct(id_cae2),
           dt = n_distinct(cae_dt),
           armonizacion = n_distinct(ID)) %>% 
  summarise(across(c(sii, dt, armonizacion),
                   list(mean = ~mean(.),
                        q1 = ~quantile(., 0.25),
                        q2 = ~quantile(., 0.5),
                        q3 = ~quantile(., 0.75),
                        min = ~min(.),
                        max = ~max(.)))) %>% 
  pivot_longer(everything()) %>% 
  mutate(stat = str_extract(name, pattern = "(?<=_).*"),
         name = str_remove(str_extract(name, pattern = ".*(?<=_)"), "_")) %>% 
  pivot_wider(id_cols = "name", values_from = "value", names_from = "stat")
  
hoja1 %>% group_by(rut_empresa) %>% summarise(n=n()) %>% summarise(dup = sum(n>1),
                                                               uniq = sum(n==1))

sum(is.na(hoja1$rut_empresa))
  
saveRDS(hoja1, "input/data/dt/neg_col_dt.rds")

#Cuadro 2: Listado de huelgas aprobadas. Desde 01 enero 2005 al 04 octubre 2023
#n=15192
hoja2 = read_xlsx("input/data/dt/1590_NC_y_Huelgas_2005_2023.xlsx",
                  sheet = "Cuadro 2", range = "B4:P15196",
                  na = "NULL") %>% 
  janitor::clean_names() %>% 
  select(rut_empresa, rsu, 
         cae_dt = cae_codigo,
         ano_escrutinio_h = ano_escrutinio, fecha_escrutinio_h = fecha_escrutinio,
         trab_mas_h = trabajadores_hombres,
         trab_fem_h = trabajadores_mujeres,
         trab_tot_h = trabajadores,
         region_h = region,
         tipo_org_h = tipo_org_sindical_y_grupo) %>% #Antes merge 15.192
  merge(., llave, by = "rut_empresa", all.x = T) #Post merge 15.209

hoja2 %>% 
  mutate(info = case_when(is.na(id_cae2) & !is.na(cae_dt) ~ "Sólo DT",
                          !is.na(id_cae2) & is.na(cae_dt) ~ "Sólo SII",
                          !is.na(id_cae2) & !is.na(cae_dt) ~ "Ambas",
                          TRUE ~ "Ninguna")) %>% 
  group_by(info) %>%
  count %>% 
  mutate(prop = round(n/nrow(hoja2)*100, 3))

hoja2 %>% 
  group_by(rut_empresa) %>% 
  summarise(sii = n_distinct(id_cae2),
            dt = n_distinct(cae_dt),
            armonizacion = n_distinct(ID)) %>% 
  summarise(across(c(sii, dt, armonizacion),
                   list(mean = ~mean(.),
                        q1 = ~quantile(., 0.25),
                        q2 = ~quantile(., 0.5),
                        q3 = ~quantile(., 0.75),
                        min = ~min(.),
                        max = ~max(.)))) %>% 
  pivot_longer(everything()) %>% 
  mutate(stat = str_extract(name, pattern = "(?<=_).*"),
         name = str_remove(str_extract(name, pattern = ".*(?<=_)"), "_")) %>% 
  pivot_wider(id_cols = "name", values_from = "value", names_from = "stat")

hoja2 %>% group_by(rut_empresa) %>% summarise(n=n()) %>% summarise(dup = sum(n>1),
                                                               uniq = sum(n==1))

sum(is.na(hoja2$rut_empresa))

saveRDS(hoja2, "input/data/dt/huelgas_ntrab_dt.rds")

#Cuadro 3: Listado de huelgas terminadas. Desde 01 enero 2005 al 30 septiembre 2023
#n=2960
hoja3 = read_xlsx("input/data/dt/1590_NC_y_Huelgas_2005_2023.xlsx",
                  sheet = "Cuadro 3", range = "B4:T2964",
                  na = "NULL") %>% 
  janitor::clean_names() %>% 
  select(rut_empresa = empresa, rsu = rsu_raf, 
         cae_dt = cod_cae,
         ano_inicio_h = ano_inicio, 
         fecha_inicio_h = fecha_inicio, 
         ano_termino_h = ano_termino, 
         fecha_termino_h = fecha_termino,
         dias_h = dias_huelga, 
         termino_neg_h = termino_negociacion,
         trab_mas_h = trabajadores_hombres,
         trab_fem_h = trabajadores_mujeres,
         trab_tot_h = trabajadores,
         region_h = region) %>% 
  mutate(rut_empresa = as.character(rut_empresa)) %>% #Pre merge: .2960
  merge(., llave, by = "rut_empresa", all.x = T)# Post merge: 2.967
  

hoja3 %>% 
  mutate(info = case_when(is.na(id_cae2) & !is.na(cae_dt) ~ "Sólo DT",
                          !is.na(id_cae2) & is.na(cae_dt) ~ "Sólo SII",
                          !is.na(id_cae2) & !is.na(cae_dt) ~ "Ambas",
                          TRUE ~ "Ninguna")) %>% 
  group_by(info) %>%
  count %>% 
  mutate(prop = round(n/nrow(hoja3)*100, 3))

hoja3 %>% 
  group_by(rut_empresa) %>% 
  summarise(sii = n_distinct(id_cae2),
            dt = n_distinct(cae_dt),
            armonizacion = n_distinct(ID)) %>% 
  summarise(across(c(sii, dt, armonizacion),
                   list(mean = ~mean(.),
                        q1 = ~quantile(., 0.25),
                        q2 = ~quantile(., 0.5),
                        q3 = ~quantile(., 0.75),
                        min = ~min(.),
                        max = ~max(.)))) %>% 
  pivot_longer(everything()) %>% 
  mutate(stat = str_extract(name, pattern = "(?<=_).*"),
         name = str_remove(str_extract(name, pattern = ".*(?<=_)"), "_")) %>% 
  pivot_wider(id_cols = "name", values_from = "value", names_from = "stat")

hoja3 %>% group_by(rut_empresa) %>% summarise(n=n()) %>% summarise(dup = sum(n>1),
                                                               uniq = sum(n==1))

sum(is.na(hoja3$rut_empresa))
saveRDS(hoja3, "input/data/dt/huelgas_dur_dt.rds")

