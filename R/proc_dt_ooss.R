rm(list = ls())

# Procesamiento data OOSS --------------------------------------

# Cargar paquetes ---------------------------------------------------------
pacman::p_load(tidyverse, readxl, sjmisc)

# Cargar datos ------------------------------------------------------------

llave = readRDS("input/data/dt/CAE_RUT_FINAL.rds") %>%
  select(rut_empresa = rut, dv, codigoactividad) %>% 
  mutate(across(c(rut_empresa, dv), ~as.character(.)),
         codigoactividad = ifelse(nchar(codigoactividad) == 5, 
                                 str_extract(codigoactividad, "\\d{1}"), 
                                 str_extract(codigoactividad, "\\d{2}")),
         rut_empresa = paste(rut_empresa, dv, sep = "-")) %>% #con duplicados: 3.482.146
  group_by(rut_empresa) %>% 
  mutate(n = 1:n()) %>% 
  #ungroup() %>% 
  slice(which.min(n)) %>% #Sin duplicados: 1.617.697
  ungroup() %>% 
  select(-dv, -n)

llave = merge(llave, read_xlsx("input/data/dt/CAE_DT_armonizado.xlsx") %>% 
            select(codigoactividad, ID2),
          by = "codigoactividad", all.x = T)

# a %>% 
#   group_by(rut_empresa, codigoactividad) %>% 
#   mutate(n = n()) %>% 
#   ungroup() %>% 
#   summarise(uniq = sum(n==1),
#             uniq_p = sum(n==1)/nrow(.)*100,
#             dup = sum(n>1),
#             dup_p = sum(n>1)/nrow(.)*100)
# 
# a %>% 
#   group_by(rut_empresa, ID2) %>% 
#   mutate(n = n()) %>% 
#   ungroup() %>% 
#   summarise(uniq = sum(n==1),
#             uniq_p = sum(n==1)/nrow(.)*100,
#             dup = sum(n>1),
#             dup_p = sum(n>1)/nrow(.)*100)
# 
# a %>% 
#   #group_by(rut_empresa) %>% 
#   summarise(serv = sum(ID2 %in% 39:43, na.rm=T)/nrow(.))

files = list.files(path = "input/data/dt",
                   pattern = "1594")

#Microdatos

ooss_micro = map(files, 
           ~ read_xlsx(paste0("input/data/dt/", .x), 
                       na = c("SIN INFORMACIÓN", "NULL"),
                       guess_max = 23000) %>% 
             janitor::clean_names()) 

names(ooss_micro) = str_extract(files, pattern = "(OOSS_|COES_)\\d{4}")

ooss_micro = imap(ooss_micro,
                 ~ .x %>% 
                   mutate(anno = str_extract(.y, pattern = "\\d{4}$")))

ooss_micro$OOSS_2019 = ooss_micro$OOSS_2019 %>% #bind_rows() %>% 
  mutate(tipo_de_organizacion_sindical_de_base2 = ifelse(tipo_de_organizacion_sindical_de_base == "ASOCIACION DE FUNCIONARIOS",
                                                         "ASOCIACION DE FUNCIONARIOS", socios_hombres),
         socios_hombres2 = as.numeric(ifelse(tipo_de_organizacion_sindical_de_base == "ASOCIACION DE FUNCIONARIOS", 
                                             socios_hombres, tipo_de_organizacion_sindical_de_base))) %>% 
  select(ano:region_de_la_ooss_de_base, 
         tipo_de_organizacion_sindical_de_base = tipo_de_organizacion_sindical_de_base2,
         socios_hombres = socios_hombres2,
         socias_mujeres:anno)

ooss_micro$OOSS_2020 = ooss_micro$OOSS_2020 %>% #bind_rows() %>% 
  mutate(codigo_rae_sirela = as.numeric(codigo_rae_sirela))

# a=ooss_micro["OOSS_2019"] %>% bind_rows() %>% 
#   mutate(tipo_de_organizacion_sindical_de_base2 = ifelse(tipo_de_organizacion_sindical_de_base == "ASOCIACION DE FUNCIONARIOS", "ASOCIACION DE FUNCIONARIOS", socios_hombres),
#          socios_hombres2 = ifelse(tipo_de_organizacion_sindical_de_base == "ASOCIACION DE FUNCIONARIOS", socios_hombres, tipo_de_organizacion_sindical_de_base)) %>% 
#   select(ano:region_de_la_ooss_de_base, 
#          tipo_de_organizacion_sindical_de_base = tipo_de_organizacion_sindical_de_base2,
#          socios_hombres = socios_hombres2,
#          socias_mujeres:anno)

ooss_micro23 = ooss_micro[[8]] %>% 
  select(rut_empresa, dv, ano,
         rsu = rsu_organizacion_sindical_de_base,
         cae_1d = codigo_rae_sirela,
         trab_tot_emp = cantidad_de_trabajadores_en_la_empresa_afc,
         tamano_emp = tamano_empresa_afc,
         region = region_de_la_ooss_de_base,
         tipo_org = tipo_de_organizacion_sindical_de_base,
         afil_mas = socios_hombres,
         afil_fem = socias_mujeres,
         afil_tot = total_socios,
         n_dir_mas = n_de_dirigentes_hombres,
         n_dir_fem = n_de_dirigentas_mujeres,
         n_dir_tot = n_total_dirigentes,
         rsu_fed = rsu_federacion_rsu_fed,
         rsu_cen = rsu_central_sindical_rsu_cent,
         rsu_conf = rsu_confederacion_rsu_confed
         ) %>% 
  mutate(rut_empresa = paste(rut_empresa, dv, sep = "-")) %>% 
  dplyr::select(-dv) %>% 
  merge(., llave, by = "rut_empresa", all.x = T)

ooss_micro = bind_rows(ooss_micro[-8]) %>% 
  select(rut_empresa, dv, ano,
         rsu = rsu_organizacion_sindical_de_base,
         cae_1d = codigo_rae_sirela,
         trab_tot_emp = cantidad_de_trabajadores_en_la_empresa_afc,
         tamano_emp = tamano_empresa_afc,
         region = region_de_la_ooss_de_base,
         tipo_org = tipo_de_organizacion_sindical_de_base,
         afil_mas = socios_hombres,
         afil_fem = socias_mujeres,
         afil_tot = total_socios,
         n_dir_mas = n_de_dirigentes_hombres,
         n_dir_fem = n_de_dirigentas_mujeres,
         n_dir_tot = n_total_dirigentes)

ooss_micro = ooss_micro %>% #sin merge 168.908
  mutate(rut_empresa = paste(rut_empresa, dv, sep = "-")) %>% 
  select(-dv) %>% 
  merge(., llave, by = "rut_empresa", all.x = T)  #Con merge 169.230
  

#Pasa de 168.908 a 470.622

a = ooss_micro %>% filter(ano == 2019 & !is.na(ID2))

# ooss_micro_b %>% 
#   mutate(info = case_when(is.na(codigoactividad) & !is.na(cae_1d) ~ "Sólo DT",
#                           !is.na(codigoactividad) & is.na(cae_1d) ~ "Sólo SII",
#                           !is.na(codigoactividad) & !is.na(cae_1d) ~ "Ambas",
#                           TRUE ~ "Ninguna")) %>% 
#   group_by(info) %>%
#   count %>% 
#   mutate(prop = round(n/nrow(ooss_micro_b)*100, 3))
# 
# ooss_micro_b %>% 
#   group_by(rut_empresa) %>% 
#   summarise(sii = n_distinct(codigoactividad),
#             dt = n_distinct(cae_1d),
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

# a %>% group_by(rut_empresa) %>% summarise(n=n()) %>% summarise(dup = sum(n>1),
#                                                                uniq = sum(n==1))

#sum(is.na(a$rut_empresa))

saveRDS(ooss_micro, "input/data/dt/ooss_micro.rds")

#Federaciones

ooss_fed = map(files[-8], 
                 ~ read_xlsx(paste0("input/data/dt/", .x),
                             sheet = "Federaciones", 
                             na = "SIN INFORMACIÓN") %>% 
                 janitor::clean_names() %>% 
                 mutate(across(c(rsu_federacion_rsu_fed, rsu_raf), ~as.character(.))))

names(ooss_fed) = str_extract(files[-8], pattern = "OOSS_\\d{4}")

ooss_fed = imap(ooss_fed,
                  ~ .x %>% 
                    mutate(anno = str_extract(.y, pattern = "\\d{4}$")))

ooss_fed = bind_rows(ooss_fed) %>% 
  select(rsu = rsu_raf, ano = anno, 
         rsu_fed = rsu_federacion_rsu_fed,
         tipo_org_fed = tipo_de_federacion)

saveRDS(ooss_fed, "input/data/dt/ooss_fed.rds")

#Confederaciones

ooss_conf = map(files[-8], 
                 ~ read_xlsx(paste0("input/data/dt/", .x),
                             sheet = "Confederaciones", 
                             na = "SIN INFORMACIÓN") %>% 
                  janitor::clean_names() %>% 
                  mutate(across(c(rsu_confederacion_rsu_confed, rsu_raf), ~as.character(.))))

names(ooss_conf) = str_extract(files[-8], pattern = "OOSS_\\d{4}")

ooss_conf = imap(ooss_conf,
                  ~ .x %>% 
                    mutate(anno = str_extract(.y, pattern = "\\d{4}$")))

ooss_conf = bind_rows(ooss_conf) %>% 
  select(rsu = rsu_raf, ano = anno, 
         rsu_conf = rsu_confederacion_rsu_confed,
         tipo_org_conf = tipo_de_confederacion)

saveRDS(ooss_conf, "input/data/dt/ooss_conf.rds")


#Centrales

ooss_cen = map(files[-8], 
                 ~ read_xlsx(paste0("input/data/dt/", .x),
                             sheet = "Centrales", 
                             na = "SIN INFORMACIÓN") %>% 
                 janitor::clean_names() %>% 
                 mutate(across(c(rsu_central_sindical_rsu_cent, rsu_raf), ~as.character(.))))

names(ooss_cen) = str_extract(files[-8], pattern = "OOSS_\\d{4}")

ooss_cen = imap(ooss_cen,
                  ~ .x %>% 
                    mutate(anno = str_extract(.y, pattern = "\\d{4}$")))

ooss_cen = bind_rows(ooss_cen) %>% 
  select(rsu = rsu_raf, ano = anno, 
         rsu_cen = rsu_central_sindical_rsu_cent,
         tipo_org_cen = sigla)

saveRDS(ooss_cen, "input/data/dt/ooss_cen.rds")


# Unificar ----------------------------------------------------------------

ooss = ooss_micro %>% 
  merge(., 
        ooss_fed, by = c("rsu", "ano"),
        all.x = T) %>% 
  merge(., 
        ooss_conf, by = c("rsu", "ano"),
        all.x = T) %>% 
  merge(., 
        ooss_cen, by = c("rsu", "ano"),
        all.x = T) %>% 
  bind_rows(., ooss_micro23) %>% 
  mutate(across(starts_with("rsu_"), ~ifelse(. == "-", NA, .)),
         rut_empresa = ifelse(rut_empresa %in% c("0", "0-NA"), NA, rut_empresa),
         ) 

saveRDS(ooss, "input/data/dt/ooss.rds")
#169230

