rm(list = ls())

# Procesamiento data OOSS --------------------------------------

# Cargar paquetes ---------------------------------------------------------
pacman::p_load(tidyverse, readxl, sjmisc)

# Cargar datos ------------------------------------------------------------

llave = readRDS("input/data/dt/CAE_RUT_FINAL.rds") %>%
  select(rut_empresa = rut, dv, id_cae2) %>% 
  mutate(across(c(rut_empresa, dv), ~as.character(.)),
         id_cae2 = as.numeric(id_cae2),
         rut_empresa = paste(rut_empresa, dv, sep = "-")) %>% #con duplicados: 3.482.146
  group_by(rut_empresa) %>% 
  mutate(n = 1:n()) %>% 
  ungroup() %>% 
  filter(n==1) %>% #Sin duplicados: 1.617.697
  select(-dv, -n)

llave = merge(llave, read_xlsx("input/data/dt/CAE_DT_armonizado.xlsx") %>% 
            select(id_cae2, ID),
          by = "id_cae2", all.x = T)

# a %>% 
#   group_by(rut_empresa, id_cae2) %>% 
#   mutate(n = n()) %>% 
#   ungroup() %>% 
#   summarise(uniq = sum(n==1),
#             uniq_p = sum(n==1)/nrow(.)*100,
#             dup = sum(n>1),
#             dup_p = sum(n>1)/nrow(.)*100)
# 
# a %>% 
#   group_by(rut_empresa, ID) %>% 
#   mutate(n = n()) %>% 
#   ungroup() %>% 
#   summarise(uniq = sum(n==1),
#             uniq_p = sum(n==1)/nrow(.)*100,
#             dup = sum(n>1),
#             dup_p = sum(n>1)/nrow(.)*100)
# 
# a %>% 
#   #group_by(rut_empresa) %>% 
#   summarise(serv = sum(ID %in% 39:43, na.rm=T)/nrow(.))

files = list.files(path = "input/data/dt",
                   pattern = "1594")

#Microdatos

ooss_micro = map(files, 
           ~ read_xlsx(paste0("input/data/dt/", .x), 
                       na = c("SIN INFORMACIÓN", "NULL")) %>% 
             janitor::clean_names()) 

names(ooss_micro) = str_extract(files, pattern = "(OOSS_|COES_)\\d{4}")

ooss_micro = imap(ooss_micro,
                 ~ .x %>% 
                   mutate(anno = str_extract(.y, pattern = "\\d{4}$")))


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



# ooss_micro_b %>% 
#   mutate(info = case_when(is.na(id_cae2) & !is.na(cae_1d) ~ "Sólo DT",
#                           !is.na(id_cae2) & is.na(cae_1d) ~ "Sólo SII",
#                           !is.na(id_cae2) & !is.na(cae_1d) ~ "Ambas",
#                           TRUE ~ "Ninguna")) %>% 
#   group_by(info) %>%
#   count %>% 
#   mutate(prop = round(n/nrow(ooss_micro_b)*100, 3))
# 
# ooss_micro_b %>% 
#   group_by(rut_empresa) %>% 
#   summarise(sii = n_distinct(id_cae2),
#             dt = n_distinct(cae_1d),
#             armonizacion = n_distinct(ID)) %>% 
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
         rut_empresa = ifelse(rut_empresa %in% c("0", "0-NA"), NA, rut_empresa)) 

saveRDS(ooss, "input/data/dt/ooss.rds")
#169230

