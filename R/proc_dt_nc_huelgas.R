rm(list = ls())

# Procesamiento data NC y Huelgas DT --------------------------------------

# Cargar paquetes ---------------------------------------------------------
pacman::p_load(tidyverse, readxl, sjmisc)

# Cargar datos ------------------------------------------------------------

llave = readRDS("input/data/dt/CAE_RUT_FINAL.rds") %>% 
  select(rut_empresa = rut, dv, cae = id_cae2) %>% 
  mutate(across(c(rut_empresa, dv), ~as.character(.)),
         cae = as.numeric(cae))
  

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
  mutate(rut_empresa = as.character(rut_empresa)) %>% 
  merge(., llave, by = "rut_empresa", all.x = T) %>% 
  group_by(rut_empresa, rsu, fecha_inicio_nc) %>% 
  mutate(n = row_number()) %>% 
  ungroup() %>% 
  filter(n==1) %>% 
  select(-c(n, rsu))

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
         tipo_org_h = tipo_org_sindical_y_grupo) %>% 
  merge(., llave, by = "rut_empresa", all.x = T) %>% 
  group_by(rut_empresa, rsu, fecha_escrutinio_h) %>% 
  mutate(n = row_number()) %>% 
  ungroup() %>% 
  filter(n==1) %>% 
  select(-c(n, rsu))


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
  mutate(rut_empresa = as.character(rut_empresa)) %>% 
  merge(., llave, by = "rut_empresa", all.x = T) %>% 
  group_by(rut_empresa, rsu, fecha_inicio_h) %>% 
  mutate(n = row_number()) %>% 
  ungroup() %>% 
  filter(n==1) %>% 
  select(-c(n, rsu))


