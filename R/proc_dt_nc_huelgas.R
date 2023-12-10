rm(list = ls())

# Procesamiento data NC y Huelgas DT --------------------------------------

# Cargar paquetes ---------------------------------------------------------
pacman::p_load(tidyverse, readxl, sjmisc)

# Cargar datos ------------------------------------------------------------

llave = readRDS("input/data/dt/CAE_RUT_FINAL.rds") %>%
  select(rut_empresa = rut, dv, codigoactividad) %>% 
  mutate(across(c(rut_empresa, dv), ~as.character(.)),
         codigoactividad = ifelse(nchar(codigoactividad) == 5, 
                                  str_extract(codigoactividad, "\\d{1}"), 
                                  str_extract(codigoactividad, "\\d{2}"))#, rut_empresa = paste(rut_empresa, dv, sep = "-")
         ) %>% #con duplicados: 3.482.146
  group_by(rut_empresa) %>% 
  mutate(n = 1:n()) %>% 
  #ungroup() %>% 
  slice(which.min(n)) %>% #Sin duplicados: 1.617.697
  ungroup() %>% 
  select(-dv, -n) %>% 
  rename(codigoactividad_sii = codigoactividad)

llave = merge(llave, read_xlsx("input/data/dt/CAE_DT_armonizado.xlsx") %>% 
                select(codigoactividad_sii= codigoactividad, ID2sii = ID2) %>% unique,
              by = "codigoactividad_sii", all.x = T)
  

#Cuadro 1: Listado de instrumentos colectivos y acuerdo de grupo negociador que 
#inician vigencia. Registrados desde 01 enero 1999 al 04 octubre 2023
#n=71.954
hoja1 = read_xlsx("input/data/dt/1590_NC_y_Huelgas_2005_2023.xlsx",
                 sheet = "Cuadro 1", range = "B4:R71958",
                na = "NULL") %>% 
  janitor::clean_names() %>% 
  select(rut_empresa, rsu, 
         codigoactividad = codigo_cae,
         ano = ano_inicio, 
         trab_empresa_nc = trab_empresa, 
         tipo_inst = tipo_instrumento_colectivo_y_acuerdo_grupo_negociador,
         tipo_neg = tipo_negociacion_colectiva,
         trab_mas_nc = trabajadores_hombres_involucrados,
         trab_fem_nc = trabajadores_mujeres_involucradas,
         trab_tot_nc = trabajadores_total_involucrados) %>% 
  mutate(codigoactividad = as.numeric(ifelse(nchar(codigoactividad) == 5, 
                                    str_extract(codigoactividad, "\\d{1}"), 
                                    str_extract(codigoactividad, "\\d{2}"))),
         rut_empresa = as.character(rut_empresa)) %>% #antes merge 71.954
  merge(., llave, by = "rut_empresa", all.x = T) %>% #despues merge 72.158
  merge(., 
        read_xlsx("input/data/dt/CAE_DT_armonizado.xlsx") %>% 
          select(codigoactividad, ID2dt=ID2) %>% unique,
        by = "codigoactividad", all.x = T) %>% 
  mutate(
         # codigoactividad = case_when(codigoactividad %in% 1:4 ~ codigoactividad,
         #                    codigoactividad %in% 5:9 ~ 5, 
         #                    codigoactividad %in% 10:12 ~ 6,
         #                    codigoactividad %in% 13:14 ~ 7,
         #                    codigoactividad %in% 15:17 ~ 8,
         #                    codigoactividad %in% c(18, 21, 23) ~ 9,
         #                    codigoactividad %in% c(19, 20, 22) ~ 10,
         #                    codigoactividad %in% c(24:27, 33) ~ 11,
         #                    codigoactividad %in% c(28:30) ~ 12,
         #                    codigoactividad %in% c(31, 32) ~ 13,
         #                    codigoactividad %in% c(35:36) ~ 14,
         #                    codigoactividad %in% c(37:39) ~ 15,
         #                    codigoactividad %in% c(40,41) ~ 16,
         #                    codigoactividad %in% c(42) ~ 17,
         #                    codigoactividad %in% c(43) ~ 18,
         #                    codigoactividad %in% c(44:45) ~ 19,
         #                    codigoactividad %in% c(46:47) ~ 20,
         #                    codigoactividad %in% c(48) ~ 21,
         #                    codigoactividad %in% c(49) ~ 22,
         #                    codigoactividad %in% c(50:51) ~ 23,
         #                    codigoactividad %in% c(52:53) ~ 24,
         #                    codigoactividad %in% c(55:56) ~ codigoactividad - 30,
         #                    codigoactividad %in% c(57:60) ~ 27,
         #                    codigoactividad %in% c(61) ~ 28,
         #                    codigoactividad %in% c(62:63) ~ 29,
         #                    codigoactividad %in% c(64:66) ~ 30,
         #                    codigoactividad %in% c(68) ~ 31,
         #                    codigoactividad %in% c(69:75) ~ 32,
         #                    codigoactividad %in% c(76:82) ~ 33,
         #                    codigoactividad %in% c(84:88) ~ codigoactividad - 50,
         #                    codigoactividad %in% c(89:93) ~ 39,
         #                    codigoactividad %in% c(94:95) ~ codigoactividad - 54,
         #                    codigoactividad %in% c(96:98) ~ 42,
         #                    codigoactividad %in% c(99) ~ 43,
         #                    TRUE ~ NA_integer_),
         ID2 = factor(case_when(!is.na(ID2dt) ~ ID2dt,
                              !is.na(ID2sii) ~ ID2sii,
                              TRUE ~ NA_integer_)),
         across(c(tipo_inst, tipo_neg), ~ str_to_title(.))) #%>% 
  # merge(llave, read_xlsx("input/data/dt/CAE_DT_armonizado.xlsx") %>% 
  #         select(codigoactividad, ID2),
  #       by = "codigoactividad", all.x = T)


# hoja1 %>% 
#   mutate(info = case_when(is.na(codigoactividad) & !is.na(codigoactividad) ~ "Sólo DT",
#                           !is.na(codigoactividad) & is.na(codigoactividad) ~ "Sólo SII",
#                           !is.na(codigoactividad) & !is.na(codigoactividad) ~ "Ambas",
#                           TRUE ~ "Ninguna")) %>% 
#   group_by(info) %>%
#   count %>% 
#   mutate(prop = round(n/nrow(hoja1)*100, 3))
# 
# hoja1 %>% 
#   group_by(rut_empresa) %>% 
#   summarise(sii = n_distinct(codigoactividad),
#            dt = n_distinct(codigoactividad),
#            armonizacion = n_distinct(ID2)) %>% 
#   summarise(across(c(sii, dt, armonizacion),
#                    list(mean = ~mean(.),
#                         q1 = ~quantile(., 0.25),
#                         q2 = ~quantile(., 0.5),
#                         q3 = ~quantile(., 0.75),
#                         min = ~min(.),
#                         max = ~max(.)))) %>% 
#   pivot_longer(everything()) %>% 
#   mutate(stat = str_extract(name, pattern = "(?<=_).*"),
#          name = str_remove(str_extract(name, pattern = ".*(?<=_)"), "_")) %>% 
#   pivot_wider(id_cols = "name", values_from = "value", names_from = "stat")
#   
# hoja1 %>% group_by(rut_empresa) %>% summarise(n=n()) %>% summarise(dup = sum(n>1),
#                                                                uniq = sum(n==1))
# 
# sum(is.na(hoja1$rut_empresa))
  
saveRDS(hoja1, "input/data/dt/neg_col_dt.rds")

#Cuadro 2: Listado de huelgas aprobadas. Desde 01 enero 2005 al 04 octubre 2023
#n=15192
hoja2 = read_xlsx("input/data/dt/1590_NC_y_Huelgas_2005_2023.xlsx",
                  sheet = "Cuadro 2", range = "B4:P15196",
                  na = "NULL") %>% 
  janitor::clean_names() %>% 
  select(rut_empresa, rsu, 
         codigoactividad = cae_codigo,
         ano = ano_escrutinio, 
         fecha = fecha_escrutinio,
         trab_mas_h = trabajadores_hombres,
         trab_fem_h = trabajadores_mujeres,
         trab_tot_h = trabajadores,
         tipo_org_h = tipo_org_sindical_y_grupo) %>% #Antes merge 15.192
  mutate(codigoactividad = as.numeric(ifelse(nchar(codigoactividad) == 5, 
                                             str_extract(codigoactividad, "\\d{1}"), 
                                             str_extract(codigoactividad, "\\d{2}")))) %>% #antes merge 71.954
  merge(., llave, by = "rut_empresa", all.x = T) %>% #despues merge 72.158
  merge(., 
        read_xlsx("input/data/dt/CAE_DT_armonizado.xlsx") %>% 
          select(codigoactividad, ID2dt=ID2) %>% unique,
        by = "codigoactividad", all.x = T) %>% 
  mutate(
         # codigoactividad = case_when(codigoactividad %in% 5:9 ~ 5, 
         #                    codigoactividad %in% 10:12 ~ 6,
         #                    codigoactividad %in% 13:14 ~ 7,
         #                    codigoactividad %in% 15:17 ~ 8,
         #                    codigoactividad %in% c(18, 21, 23) ~ 9,
         #                    codigoactividad %in% c(19, 20, 22) ~ 10,
         #                    codigoactividad %in% c(24:27, 33) ~ 11,
         #                    codigoactividad %in% c(28:30) ~ 12,
         #                    codigoactividad %in% c(31, 32) ~ 13,
         #                    codigoactividad %in% c(35:36) ~ 14,
         #                    codigoactividad %in% c(37:39) ~ 15,
         #                    codigoactividad %in% c(40,41) ~ 16,
         #                    codigoactividad %in% c(42) ~ 17,
         #                    codigoactividad %in% c(43) ~ 18,
         #                    codigoactividad %in% c(44:45) ~ 19,
         #                    codigoactividad %in% c(46:47) ~ 20,
         #                    codigoactividad %in% c(48) ~ 21,
         #                    codigoactividad %in% c(49) ~ 22,
         #                    codigoactividad %in% c(50:51) ~ 23,
         #                    codigoactividad %in% c(52:53) ~ 24,
         #                    codigoactividad %in% c(55:56) ~ codigoactividad - 30,
         #                    codigoactividad %in% c(57:60) ~ 27,
         #                    codigoactividad %in% c(61) ~ 28,
         #                    codigoactividad %in% c(62:63) ~ 29,
         #                    codigoactividad %in% c(64:66) ~ 30,
         #                    codigoactividad %in% c(68) ~ 31,
         #                    codigoactividad %in% c(69:75) ~ 32,
         #                    codigoactividad %in% c(76:82) ~ 33,
         #                    codigoactividad %in% c(84:88) ~ codigoactividad - 50,
         #                    codigoactividad %in% c(89:93) ~ 39,
         #                    codigoactividad %in% c(94:95) ~ codigoactividad - 54,
         #                    codigoactividad %in% c(96:98) ~ 42,
         #                    codigoactividad %in% c(99) ~ 43,
         #                    TRUE ~ NA_integer_),
         ID2 = factor(case_when(!is.na(ID2dt) ~ ID2dt,
                                !is.na(ID2sii) ~ ID2sii,
                                TRUE ~ NA_integer_)),
         tipo_org_h = str_to_title(tipo_org_h))

# hoja2 %>% 
#   mutate(info = case_when(is.na(codigoactividad) & !is.na(codigoactividad) ~ "Sólo DT",
#                           !is.na(codigoactividad) & is.na(codigoactividad) ~ "Sólo SII",
#                           !is.na(codigoactividad) & !is.na(codigoactividad) ~ "Ambas",
#                           TRUE ~ "Ninguna")) %>% 
#   group_by(info) %>%
#   count %>% 
#   mutate(prop = round(n/nrow(hoja2)*100, 3))
# 
# hoja2 %>% 
#   group_by(rut_empresa) %>% 
#   summarise(sii = n_distinct(codigoactividad),
#             dt = n_distinct(codigoactividad),
#             armonizacion = n_distinct(ID2)) %>% 
#   summarise(across(c(sii, dt, armonizacion),
#                    list(mean = ~mean(.),
#                         q1 = ~quantile(., 0.25),
#                         q2 = ~quantile(., 0.5),
#                         q3 = ~quantile(., 0.75),
#                         min = ~min(.),
#                         max = ~max(.)))) %>% 
#   pivot_longer(everything()) %>% 
#   mutate(stat = str_extract(name, pattern = "(?<=_).*"),
#          name = str_remove(str_extract(name, pattern = ".*(?<=_)"), "_")) %>% 
#   pivot_wider(id_cols = "name", values_from = "value", names_from = "stat")
# 
# hoja2 %>% group_by(rut_empresa) %>% summarise(n=n()) %>% summarise(dup = sum(n>1),
#                                                                uniq = sum(n==1))
# 
# sum(is.na(hoja2$rut_empresa))

saveRDS(hoja2, "input/data/dt/huelgas_ntrab_dt.rds")

#Cuadro 3: Listado de huelgas terminadas. Desde 01 enero 2005 al 30 septiembre 2023
#n=2960
hoja3 = read_xlsx("input/data/dt/1590_NC_y_Huelgas_2005_2023.xlsx",
                  sheet = "Cuadro 3", range = "B4:T2964",
                  na = "NULL") %>% 
  janitor::clean_names() %>% 
  select(rut_empresa = empresa, rsu = rsu_raf, 
         codigoactividad = cod_cae,
         ano = ano_inicio, 
         fecha = fecha_inicio,
         dias_h = dias_huelga, 
         resultado_neg_h = termino_negociacion,
         trab_mas_h = trabajadores_hombres,
         trab_fem_h = trabajadores_mujeres,
         trab_tot_h = trabajadores) %>% 
  mutate(codigoactividad = as.numeric(ifelse(nchar(codigoactividad) == 5, 
                                             str_extract(codigoactividad, "\\d{1}"), 
                                             str_extract(codigoactividad, "\\d{2}"))),
         rut_empresa = as.character(rut_empresa)) %>% #antes merge 71.954
  merge(., llave, by = "rut_empresa", all.x = T) %>% #despues merge 72.158
  merge(., 
        read_xlsx("input/data/dt/CAE_DT_armonizado.xlsx") %>% 
          select(codigoactividad, ID2dt=ID2) %>% unique,
        by = "codigoactividad", all.x = T) %>% 
  mutate(
    # codigoactividad = case_when(codigoactividad %in% 5:9 ~ 5, 
         #                    codigoactividad %in% 10:12 ~ 6,
         #                    codigoactividad %in% 13:14 ~ 7,
         #                    codigoactividad %in% 15:17 ~ 8,
         #                    codigoactividad %in% c(18, 21, 23) ~ 9,
         #                    codigoactividad %in% c(19, 20, 22) ~ 10,
         #                    codigoactividad %in% c(24:27, 33) ~ 11,
         #                    codigoactividad %in% c(28:30) ~ 12,
         #                    codigoactividad %in% c(31, 32) ~ 13,
         #                    codigoactividad %in% c(35:36) ~ 14,
         #                    codigoactividad %in% c(37:39) ~ 15,
         #                    codigoactividad %in% c(40,41) ~ 16,
         #                    codigoactividad %in% c(42) ~ 17,
         #                    codigoactividad %in% c(43) ~ 18,
         #                    codigoactividad %in% c(44:45) ~ 19,
         #                    codigoactividad %in% c(46:47) ~ 20,
         #                    codigoactividad %in% c(48) ~ 21,
         #                    codigoactividad %in% c(49) ~ 22,
         #                    codigoactividad %in% c(50:51) ~ 23,
         #                    codigoactividad %in% c(52:53) ~ 24,
         #                    codigoactividad %in% c(55:56) ~ codigoactividad - 30,
         #                    codigoactividad %in% c(57:60) ~ 27,
         #                    codigoactividad %in% c(61) ~ 28,
         #                    codigoactividad %in% c(62:63) ~ 29,
         #                    codigoactividad %in% c(64:66) ~ 30,
         #                    codigoactividad %in% c(68) ~ 31,
         #                    codigoactividad %in% c(69:75) ~ 32,
         #                    codigoactividad %in% c(76:82) ~ 33,
         #                    codigoactividad %in% c(84:88) ~ codigoactividad - 50,
         #                    codigoactividad %in% c(89:93) ~ 39,
         #                    codigoactividad %in% c(94:95) ~ codigoactividad - 54,
         #                    codigoactividad %in% c(96:98) ~ 42,
         #                    codigoactividad %in% c(99) ~ 43,
         #                    TRUE ~ NA_integer_),
         ID2 = factor(case_when(!is.na(ID2dt) ~ ID2dt,
                                !is.na(ID2sii) ~ ID2sii,
                                TRUE ~ NA_integer_)))
  

# hoja3 %>% 
#   mutate(info = case_when(is.na(codigoactividad) & !is.na(codigoactividad) ~ "Sólo DT",
#                           !is.na(codigoactividad) & is.na(codigoactividad) ~ "Sólo SII",
#                           !is.na(codigoactividad) & !is.na(codigoactividad) ~ "Ambas",
#                           TRUE ~ "Ninguna")) %>% 
#   group_by(info) %>%
#   count %>% 
#   mutate(prop = round(n/nrow(hoja3)*100, 3))
# 
# hoja3 %>% 
#   group_by(rut_empresa) %>% 
#   summarise(sii = n_distinct(codigoactividad),
#             dt = n_distinct(codigoactividad),
#             armonizacion = n_distinct(ID2)) %>% 
#   summarise(across(c(sii, dt, armonizacion),
#                    list(mean = ~mean(.),
#                         q1 = ~quantile(., 0.25),
#                         q2 = ~quantile(., 0.5),
#                         q3 = ~quantile(., 0.75),
#                         min = ~min(.),
#                         max = ~max(.)))) %>% 
#   pivot_longer(everything()) %>% 
#   mutate(stat = str_extract(name, pattern = "(?<=_).*"),
#          name = str_remove(str_extract(name, pattern = ".*(?<=_)"), "_")) %>% 
#   pivot_wider(id_cols = "name", values_from = "value", names_from = "stat")
# 
# hoja3 %>% group_by(rut_empresa) %>% summarise(n=n()) %>% summarise(dup = sum(n>1),
#                                                                uniq = sum(n==1))

sum(is.na(hoja3$rut_empresa))
saveRDS(hoja3, "input/data/dt/huelgas_dur_dt.rds")

