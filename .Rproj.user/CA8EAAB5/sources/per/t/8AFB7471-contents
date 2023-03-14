
# Preparación de datos ----------------------------------------------------

# Cargar datos ------------------------------------------------------------
ohl <- read.csv2("input/data/OHL1979_2020.csv", na.strings = "")

# Selección y filtrado ----------------------------------------------------
ohl_p <- ohl %>% 
  filter(yr %in% c(2010:2019) & inst %in% c(1,2)) %>% 
  select(sector2 = ciuur2,
         sector3 = ciuur3,
         sector4 = ciuur4,
         ano = yr,
         duracion,
         ddpp,
         rangoemp,
         #org,
         tc,
         #reg1,
         leg,
         dptp = dhtp) %>% 

# Recodificación ----------------------------------------------------------

  mutate(sector2 = case_when(sector2 == 1 ~ "Agricultura, ganaderia, silvicultura y pesca",
                             sector2 == 2 ~ "Explotacion de minas y canteras",
                             sector2 == 3 ~ "Industrias manufactureras",
                             sector2 == 4 ~ "Suministro de electricidad, agua y gas",
                             sector2 == 5 ~ "Construccion",
                             sector2 == 6 ~ "Comercio",
                             sector2 == 7 ~ "Transporte, almacenamiento y comunicaciones",
                             sector2 == 8 ~ "Intermediacion financiera",
                             sector2 == 9 ~ "Administracion publica y defensa",
                             sector2 == 10 ~ "Enseñanza",
                             sector2 %in% c(11, 12) ~ "Servicios sociales y de salud",
                             TRUE ~ NA_character_),
         sector3 = case_when(sector3 %in% c(1,2) ~ "Agricultura, ganaderia, silvicultura y pesca",
                             sector3 == 3 ~ "Explotacion de minas y canteras",
                             sector3 == 4 ~ "Industrias manufactureras",
                             sector3 == 5 ~ "Suministro de electricidad, agua y gas",
                             sector3 == 6 ~ "Construccion",
                             sector3 == 7 ~ "Comercio",
                             sector3 == 8 ~ "Hoteles y restaurantes",
                             sector3 == 9 ~ "Transporte, almacenamiento y comunicaciones",
                             sector3 == 10 ~ "Intermediacion financiera",
                             sector3 == 11 ~ "Actividades inmobiliarias, empresariales y alquiler",
                             sector3 == 12 ~ "Administracion publica y defensa",
                             sector3 == 13 ~ "Enseñanza",
                             sector3 == 14 ~ "Servicios sociales y de salud",
                             sector3 == 15 ~  "Otras actividades de servicios",
                             sector3 == 16 ~ "Hogares privados con servicio domestico",
                             sector3 == 17 ~ "Organizaciones y organos extraterritoriales",
                             TRUE ~ NA_character_),
         sector4 = case_when(sector4 == 0 ~ "Agricultura, ganaderia, silvicultura y pesca",
                             sector4 == 1 ~ "Explotacion de minas y canteras",
                             sector4 == 2 ~ "Industrias manufactureras",
                             sector4 %in% c(3, 4) ~ "Suministro de electricidad, agua y gas",
                             sector4 == 5 ~ "Construccion",
                             sector4 == 6 ~ "Comercio",
                             sector4 %in% c(7, 9) ~ "Transporte, almacenamiento y comunicaciones",
                             sector4 == 8 ~ "Hoteles y restaurantes",
                             sector4 == 10 ~ "Intermediacion financiera",
                             sector4 %in% c(11, 12, 13) ~ "Actividades inmobiliarias, empresariales y alquiler",
                             sector4 == 14 ~ "Administracion publica y defensa",
                             sector4 == 15 ~ "Enseñanza",
                             sector4 == 16 ~ "Servicios sociales y de salud",
                             sector4 %in% c(17, 18) ~ "Otras actividades de servicios",
                             sector4 == 19 ~ "Hogares privados con servicio domestico",
                             sector4 == 20 ~ "Organizaciones y organos extraterritoriales",
                             TRUE ~ NA_character_),
         sector = ifelse(!is.na(sector3), sector3,
                         ifelse(!is.na(sector4), sector4,
                                ifelse(!is.na(sector2), sector2, NA))),
         dptp = as.numeric(gsub(",", ".", .$dptp)),
         leg = ifelse(leg == 1, 1, 0),
         extra_leg = ifelse(leg == 0, 1, 0)
         ) %>% 
  select(sector, 
         ano,
         duracion,
         ddpp,
         rangoemp,
         #org,
         tc,
         #reg1,
         leg,
         extra_leg,
         dptp) %>% 
 group_by(sector, ano) %>%
 summarise(duracion = round(mean(duracion, na.rm = T), 3),
           ddpp = round(mean(ddpp, na.rm = T), 3),
           dptp = round(mean(dptp, na.rm = T), 3),
           tc = round(mean(tc, na.rm = T), 3),
           leg = sum(leg, na.rm = T),
           extra_leg = sum(extra_leg, na.rm = T)) %>%
 ungroup() %>% 
  rowwise() %>% 
  mutate(n_huelgas = leg + extra_leg) %>% 
  ungroup() %>% 
  filter(!is.na(sector)) %>% 
  mutate(sector = car::recode(.$sector, recodes = c("'Agricultura, ganaderia, silvicultura y pesca' = '1. Agricultura, ganaderia, silvicultura y pesca';
                            'Explotacion de minas y canteras' = '2. Explotacion de minas y canteras';
                            'Industrias manufactureras' = '3. Industrias manufactureras';
                            'Suministro de electricidad, agua y gas' = '4. Suministro de electricidad, agua y gas';
                            'Construccion' = '5. Construccion';
                            'Comercio' = '6. Comercio';
                            'Hoteles y restaurantes' = '7. Hoteles y restaurantes';
                            'Transporte, almacenamiento y comunicaciones' = '8. Transporte, almacenamiento y comunicaciones';
                            'Intermediacion financiera' = '9. Intermediacion financiera';
                            'Actividades inmobiliarias, empresariales y alquiler' = '10. Actividades inmobiliarias, empresariales y alquiler';
                            'Administracion publica y defensa' = '11. Administracion publica y defensa';
                            'Enseñanza' = '12. Enseñanza';
                            'Servicios sociales y de salud' = '13. Servicios sociales y de salud';
                            'Hogares privados con servicio domestico' = '14. Hogares privados con servicio domestico';
                            'Organizaciones y organos extraterritoriales' = '15. Organizaciones y organos extraterritoriales';
                            'Otras actividades de servicios' = '16. Otras actividades de servicios'")))

# Crear total

total <- ohl_p %>% 
  group_by(ano) %>% 
  summarise(duracion = round(mean(duracion, na.rm = T), 3),
            ddpp = round(mean(ddpp, na.rm = T), 3),
            dptp = round(mean(dptp, na.rm = T), 3),
            tc = round(mean(tc, na.rm = T), 3),
            leg = sum(leg, na.rm = T),
            extra_leg = sum(extra_leg, na.rm = T),
            n_huelgas = sum(n_huelgas, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(sector = "Total") %>% 
  select(sector, everything())

# Unir total

ohl_p <- bind_rows(ohl_p, total)

# Exportar datos ----------------------------------------------------------

writexl::write_xlsx(ohl_p, "output/data/ohl.xlsx")
