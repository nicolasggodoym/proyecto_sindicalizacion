rm(list=ls())
# Procesamiento datos ENE 2006  -------------------------------------------


# Cargar paquetes ---------------------------------------------------------
pacman::p_load(tidyverse,
               sjmisc,
               haven)

# Cargar datos ------------------------------------------------------------
files = list.files("input/data/ene", pattern = "-11.RData") %>% 
  str_remove("-11.RData")

data.list = imap(files, ~get(load(paste0("input/data/ene/", .x, "-11.RData"))))
data.list = setNames(data.list, files %>% str_remove("ene-"))

# a = data.list$`2014`
# b = data.list$`2009`
# c = data.list$`1995`
# d = data.list$`1986`

# names(data.list)
# 
# names = data.list %>% 
#   map(~colnames(.x))



data.list = data.list %>% 
  map_if(., ~ncol(.x) == 40, 
                           ~ .x %>% 
                      select(ano = ano_trimestre,
                             estrato, 
                             cong = seccion, 
                             id = hogar, 
                             fe = fact, 
                             ciiu = p07,
                             cise = p08) %>% 
           mutate(ciiu = case_when(ciiu == 1 ~ "1. Agricultura, ganadería, silvicultura y pesca",
                                   ciiu == 2 ~ "2. Minería",
                                   ciiu == 3 ~ "3. Industrias manufactureras",
                                   ciiu == 4 ~ "4. Suministro de electricidad, gas y agua",
                                   ciiu == 5 ~ "5. Construcción",
                                   ciiu == 6 ~ "6. Comercio, hoteles y restaurantes",
                                   ciiu == 7 ~ "7. Transporte y comunicaciones",
                                   ciiu == 8 ~ "8. Servicios financieros, inmobiliarios y empresariales",
                                   ciiu == 9 ~ "9. Servicios sociales, domésticos, profesionales y otros",
                                   TRUE ~ NA_character_),
                  cise = ifelse(cise %in% c(2, 4, 6, 7, 8),
                                "CP/Priv", "No")),
         .progress = T) %>% 
  map_if(., ~ncol(.x) == 43, 
         ~ .x %>% 
           select(ano = ano_trimestre, 
                  estrato, 
                  cong = seccion, 
                  id = hogar, 
                  fe = fact, 
                  ciiu = p07a,
                  cise = p08) %>% 
           mutate(ciiu = case_when(ciiu == 1 ~ "1. Agricultura, ganadería, silvicultura y pesca",
                                   ciiu == 2 ~ "2. Minería",
                                   ciiu == 3 ~ "3. Industrias manufactureras",
                                   ciiu == 4 ~ "4. Suministro de electricidad, gas y agua",
                                   ciiu == 5 ~ "5. Construcción",
                                   ciiu == 6 ~ "6. Comercio, hoteles y restaurantes",
                                   ciiu == 7 ~ "7. Transporte y comunicaciones",
                                   ciiu == 8 ~ "8. Servicios financieros, inmobiliarios y empresariales",
                                   ciiu == 9 ~ "9. Servicios sociales, domésticos, profesionales y otros",
                                   TRUE ~ NA_character_),
                  cise = ifelse(cise %in% c(2, 4, 6, 7, 8),
                                "CP/Priv", "No")),
         .progress = T) %>% 
  map_if(., ~ncol(.x) == 47, 
         ~ .x %>% select(ano = ano_trimestre, 
                         estrato, 
                         cong = seccion, 
                         id = hogar, 
                         fe = fact, 
                         ciiu = p08,
                         cise = p07) %>% 
           mutate(ciiu = case_when(ciiu == 1 ~ "1. Agricultura, ganadería, silvicultura y pesca",
                                   ciiu == 2 ~ "2. Minería",
                                   ciiu == 3 ~ "3. Industrias manufactureras",
                                   ciiu == 4 ~ "4. Suministro de electricidad, gas y agua",
                                   ciiu == 5 ~ "5. Construcción",
                                   ciiu == 6 ~ "6. Comercio, hoteles y restaurantes",
                                   ciiu == 7 ~ "7. Transporte y comunicaciones",
                                   ciiu == 8 ~ "8. Servicios financieros, inmobiliarios y empresariales",
                                   ciiu == 9 ~ "9. Servicios sociales, domésticos, profesionales y otros",
                                   TRUE ~ NA_character_),
                  cise = ifelse(cise %in% c(2, 3, 5, 6),
                                "CP/Priv", "No")),
         .progress = T) %>% 
  map_if(., ~ncol(.x) %in% c(140:143), 
         ~ .x %>% 
           mutate_at(vars(b12,
                          b13_ciiu_rev3, 
                       b14_ciiu_rev3, 
                       categoria_ocupacion),
                     ~haven::as_factor(.)) %>% 
           select(ano = ano_trimestre, 
                  estrato, 
                  cong = id_directorio, 
                  id = hogar, 
                  fe = fact_cal, 
                  b12,
                  ciiu13 = b13_ciiu_rev3,
                  ciiu14 = b14_ciiu_rev3,
                  cise = categoria_ocupacion) %>% 
           mutate(ciiu = case_when((b12 %in% "…directamente con la empresa en donde trabaja?" & ciiu14 %in% c("Agricultura, ganadería, caza y silvicultura", "Pesca"))|
                                     (!b12 %in% "…directamente con la empresa en donde trabaja?" & ciiu13 %in% c("Agricultura, ganadería, caza y silvicultura", "Pesca")) ~ "1. Agricultura, ganadería, silvicultura y pesca",
                                   (b12 %in% "…directamente con la empresa en donde trabaja?" & ciiu14 %in% c("Explotación de minas y canteras"))|
                                     (!b12 %in% "…directamente con la empresa en donde trabaja?" & ciiu13 %in% c("Explotación de minas y canteras")) ~ "2. Minería",
                                   (b12 %in% "…directamente con la empresa en donde trabaja?" & ciiu14 %in% c("Industrias manufactureras"))|
                                     (!b12 %in% "…directamente con la empresa en donde trabaja?" & ciiu13 %in% c("Industrias manufactureras")) ~ "3. Industrias manufactureras",
                                   (b12 %in% "…directamente con la empresa en donde trabaja?" & ciiu14 %in% c("Suministro de electricidad, gas y agua"))|
                                     (!b12 %in% "…directamente con la empresa en donde trabaja?" & ciiu13 %in% c("Suministro de electricidad, gas y agua")) ~ "4. Suministro de electricidad, gas y agua",
                                   (b12 %in% "…directamente con la empresa en donde trabaja?" & ciiu14 %in% c("Construcción"))|
                                     (!b12 %in% "…directamente con la empresa en donde trabaja?" & ciiu13 %in% c("Construcción")) ~ "5. Construcción",
                                   (b12 %in% "…directamente con la empresa en donde trabaja?" & ciiu14 %in% c("Comercio al por mayor y al por menor", "Hoteles y restaurantes"))|
                                     (!b12 %in% "…directamente con la empresa en donde trabaja?" & ciiu13 %in% c("Comercio al por mayor y al por menor", "Hoteles y restaurantes")) ~ "6. Comercio, hoteles y restaurantes",
                                   (b12 %in% "…directamente con la empresa en donde trabaja?" & ciiu14 %in% c("Transporte, almacenamiento y comunicaciones"))|
                                     (!b12 %in% "…directamente con la empresa en donde trabaja?" & ciiu13 %in% c("Transporte, almacenamiento y comunicaciones")) ~ "7. Transporte y comunicaciones",
                                   (b12 %in% "…directamente con la empresa en donde trabaja?" & ciiu14 %in% c("Intermediación financiera", "Actividades inmobiliarias, empresariales y de alquiler"))|
                                     (!b12 %in% "…directamente con la empresa en donde trabaja?" & ciiu13 %in% c("Intermediación financiera", "Actividades inmobiliarias, empresariales y de alquiler")) ~ "8. Servicios financieros, inmobiliarios y empresariales",
                                   (b12 %in% "…directamente con la empresa en donde trabaja?" & !is.na(ciiu14))|
                                     (!b12 %in% "…directamente con la empresa en donde trabaja?" & !is.na(ciiu13)) ~ "9. Servicios sociales, domésticos, profesionales y otros",
                                   TRUE ~ NA_character_),
                  cise = ifelse(cise %in% c("Cuenta propia", "Asalariado sector privado", "Personal de servicio doméstico puertas afuera", "Personal de servicio doméstico puertas adentro"),
                                "CP/Priv", "No")) %>%
           select(ano, estrato, cong, id, fe, ciiu, cise),
         .progress = T) %>% 
  map_if(., ~ncol(.x) > 143, 
         ~ .x %>% 
           mutate_at(vars(b12,
                          b13_rev4cl_caenes, 
                       b14_rev4cl_caenes, 
                       categoria_ocupacion),
                     ~haven::as_factor(.)) %>% 
           select(ano = ano_trimestre, 
                  estrato, 
                  cong = id_directorio, 
                  id = hogar, 
                  fe = fact_cal, 
                  b12,
                  ciiu13 = b13_rev4cl_caenes,
                  ciiu14 = b14_rev4cl_caenes,
                  cise = categoria_ocupacion) %>% 
           mutate(ciiu = case_when((b12 %in% "…directamente con la empresa en donde trabaja?" & ciiu14 %in% c("Agricultura, ganadería, silvicultura y pesca"))|
                                     (!b12 %in% "…directamente con la empresa en donde trabaja?" & ciiu13 %in% c("Agricultura, ganadería, silvicultura y pesca")) ~ "1. Agricultura, ganadería, silvicultura y pesca",
                                   (b12 %in% "…directamente con la empresa en donde trabaja?" & ciiu14 %in% c("Explotación de minas y canteras"))|
                                     (!b12 %in% "…directamente con la empresa en donde trabaja?" & ciiu13 %in% c("Explotación de minas y canteras")) ~ "2. Minería",
                                   (b12 %in% "…directamente con la empresa en donde trabaja?" & ciiu14 %in% c("Industrias manufactureras"))|
                                     (!b12 %in% "…directamente con la empresa en donde trabaja?" & ciiu13 %in% c("Industrias manufactureras")) ~ "3. Industrias manufactureras",
                                   (b12 %in% "…directamente con la empresa en donde trabaja?" & ciiu14 %in% c("Suministro de electricidad, gas, vapor y aire acondicionado", "Suministro de agua"))|
                                     (!b12 %in% "…directamente con la empresa en donde trabaja?" & ciiu13 %in% c("Suministro de electricidad, gas, vapor y aire acondicionado", "Suministro de agua")) ~ "4. Suministro de electricidad, gas y agua",
                                   (b12 %in% "…directamente con la empresa en donde trabaja?" & ciiu14 %in% c("Construcción"))|
                                     (!b12 %in% "…directamente con la empresa en donde trabaja?" & ciiu13 %in% c("Construcción")) ~ "5. Construcción",
                                   (b12 %in% "…directamente con la empresa en donde trabaja?" & ciiu14 %in% c("Comercio al por mayor y al por menor", "Actividades de alojamiento y de servicio de comidas"))|
                                     (!b12 %in% "…directamente con la empresa en donde trabaja?" & ciiu13 %in% c("Comercio al por mayor y al por menor", "Actividades de alojamiento y de servicio de comidas")) ~ "6. Comercio, hoteles y restaurantes",
                                   (b12 %in% "…directamente con la empresa en donde trabaja?" & ciiu14 %in% c("Transporte y almacenamiento", "Información y comunicaciones"))|
                                     (!b12 %in% "…directamente con la empresa en donde trabaja?" & ciiu13 %in% c("Transporte y almacenamiento", "Información y comunicaciones")) ~ "7. Transporte y comunicaciones",
                                   (b12 %in% "…directamente con la empresa en donde trabaja?" & ciiu14 %in% c("Actividades financieras y de seguros", "Actividades inmobiliarias", "Actividades profesionales, científicas y técnicas", "Actividades de servicios administrativos y de apoyo"))|
                                     (!b12 %in% "…directamente con la empresa en donde trabaja?" & ciiu13 %in% c("Actividades financieras y de seguros", "Actividades inmobiliarias", "Actividades profesionales, científicas y técnicas", "Actividades de servicios administrativos y de apoyo")) ~ "8. Servicios financieros, inmobiliarios y empresariales",
                                   (b12 %in% "…directamente con la empresa en donde trabaja?" & !is.na(ciiu14))|
                                     (!b12 %in% "…directamente con la empresa en donde trabaja?" & !is.na(ciiu13)) ~ "9. Servicios sociales, domésticos, profesionales y otros",
                                   TRUE ~ NA_character_),
                  cise = ifelse(cise %in% c("Cuenta propia", "Asalariado sector privado", "Personal de servicio doméstico puertas afuera", "Personal de servicio doméstico puertas adentro"),
                                "CP/Priv", "No")) %>%
           select(ano, estrato, cong, id, fe, ciiu, cise),
         .progress = T)

# map_if(data.list,~ncol(.x)==9,
#    ~frq(.x %>% select(b12,ciiu13, ciiu14, cise)))


# Exportar ----------------------------------------------------------------

save(data.list, file="output/data/ene_86_19.RData")

