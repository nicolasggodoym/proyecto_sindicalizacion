rm(list=ls())
# Preparación de datos ----------------------------------------------------
pacman::p_load(tidyverse, sjmisc)
# Cargar datos ------------------------------------------------------------
ohl <- read.csv2("input/data/OHL1979_2020.csv", na.strings = "")
llave = read_xlsx("input/data/anuario_dt.xlsx", 
                          sheet = "rev3-rev4") %>% 
  mutate(across(c(actividad3, actividad4),
                ~str_extract(., "\\d{1,2}"))) %>% 
  rename(sector3=actividad3, sector4=actividad4)

# Selección y filtrado ----------------------------------------------------
ohl_p <- ohl %>% 
  filter(yr %in% c(1998:2019) & inst %in% c(1,2)) %>% 
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
  merge(.,
        llave %>% select(sector3, CAENES_1d3=CAENES_1d) %>% unique,
        by = "sector3", all.x = T) %>% 
  merge(.,
        llave %>% select(sector4, CAENES_1d4=CAENES_1d) %>% unique,
        by = "sector4", all.x = T) %>% 

# Recodificación ----------------------------------------------------------

  mutate(CAENES_1d = ifelse(!is.na(CAENES_1d3), CAENES_1d3, CAENES_1d4),
         actividad_raw = case_when(sector2 == 1 | sector3 %in% c(1,2) | sector4 == 0 ~ "1. Agricultura, ganadería, silvicultura y pesca",
                                   sector2 == 2 | sector3 == 3 | sector4 == 1 ~ "2. Minería",
                                   sector2 == 3 | sector3 == 4 | sector4 == 2 ~ "3. Industrias manufactureras",
                                   sector2 == 4 | sector3 == 5 | sector4 %in% c(3,4) ~ "4. Suministro de electricidad, gas y agua",
                                   sector2 == 5 | sector3 == 6 | sector4 == 5 ~ "5. Construcción",
                                   sector2 == 6 | sector3 %in% c(7,8) | sector4 %in% c(6,8) ~ "6. Comercio, hoteles y restaurantes",
                                   sector2 == 7 | sector3 == 9 | sector4 %in% c(7,9) ~ "7. Transporte y comunicaciones",
                                   sector2 == 8 | sector3 %in% c(10,11) | sector4 %in% c(10:13) ~ "8. Servicios financieros, inmobiliarios y empresariales",
                                   sector2 %in% c(9:12) | sector3 %in% c(12:17) | sector4 %in% c(14:20) ~ "9. Servicios sociales, domésticos, profesionales y otros",
                                   TRUE ~ NA_character_) %>% 
           factor(),
         dptp = as.numeric(gsub(",", ".", .$dptp)),
         leg = ifelse(leg == 1, 1, 0),
         extra_leg = ifelse(leg == 0, 1, 0)) %>% 
  rowwise() %>% 
  mutate(
         n_huelga = sum(leg, extra_leg, na.rm = T)
         ) %>% 
  ungroup() %>% 
  select(ano,
         actividad_raw,
         CAENES_1d,
         duracion,
         ddpp,
         rangoemp,
         tc,
         n_huelga,
         leg,
         extra_leg,
         dptp) 

a=ohl_p %>% 
  group_by(ano, actividad_raw) %>% 
  summarise(across(c(dptp, tc, duracion),
                   ~mean(., na.rm=T)),
            n_huelga = sum(n_huelga, na.rm=T))

# a %>% 
#   select(ano, actividad_raw, duracion) %>% 
#   na.omit %>% 
#   write_xlsx("output/data/duracion_huelgas.xlsx")
  

# Exportar datos ----------------------------------------------------------

saveRDS(ohl_p, "output/data/ohl.rds")
