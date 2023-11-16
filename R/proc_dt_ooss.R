rm(list = ls())

# Procesamiento data OOSS --------------------------------------

# Cargar paquetes ---------------------------------------------------------
pacman::p_load(tidyverse, readxl, sjmisc)

# Cargar datos ------------------------------------------------------------

llave = readRDS("input/data/dt/CAE_RUT_FINAL.rds") %>% 
  select(rut_empresa = rut, dv, cae = id_cae2) %>% 
  mutate(across(c(rut_empresa, dv), ~as.character(.)),
         cae = as.numeric(cae))

files = list.files(path = "input/data/dt",
                   pattern = "1594")

#Microdatos

ooss_micro = map(files, 
           ~ read_xlsx(paste0("input/data/dt/", .x), 
                       na = "SIN INFORMACIÓN") %>% 
             janitor::clean_names()) 

names(ooss_micro) = str_extract(files, pattern = "(OOSS_|COES_)\\d{4}")

ooss_micro = imap(ooss_micro[-8],
                 ~ .x %>% 
                   mutate(anno = str_extract(.y, pattern = "\\d{4}$")))

ooss_micro = bind_rows(ooss_micro)

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

ooss_fed = bind_rows(ooss_fed)

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

ooss_conf = bind_rows(ooss_conf)

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

ooss_cen = bind_rows(ooss_cen)

saveRDS(ooss_cen, "input/data/dt/ooss_cen.rds")

