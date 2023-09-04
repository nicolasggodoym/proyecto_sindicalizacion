rm(list=ls())


# Estimación totales ENE 1986-2019 ----------------------------------------
# Por rama

options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")
# Cargar paquetes ---------------------------------------------------------

pacman::p_load(tidyverse,
               survey,
               srvyr,
               writexl)



# Cargar data -------------------------------------------------------------

load("output/data/ene_86_19.RData")

olas = as.character(seq(1999, 2019))

data.list = data.list[olas]

# Transformar df en objetos encuesta --------------------------------------

enc = map(data.list,
          ~.x %>% as_survey_design(ids = cong,
                            strata = estrato,
                            weights = fe,
                            nest = T))


# Estimar -----------------------------------------------------------------


# Totales por rama --------------------------------------------------------

a = enc %>% 
  map(.,
    ~.x %>% 
      group_by(ciiu, cp_priv) %>% 
      summarise(total = survey_total(na.rm = T)) %>% 
      filter(cp_priv == "CP/Priv" & !is.na(ciiu)) %>% 
      select(ciiu, total)) %>% 
  imap(., 
       ~.x %>% mutate(ano = .y))

totales = bind_rows(a)

total_ano = totales %>% 
  group_by(ano) %>% 
  summarise(ciiu = "Total",
            total = sum(total)) %>% 
  ungroup()

totales = rbind(totales, total_ano) 

totales = totales[order(totales$ano, totales$ciiu),]

write_xlsx(totales, "output/data/totales_ene_99-19.xlsx")

# % de mujeres por rama y año ---------------------------------------------
#sexo

a = enc %>% 
  map(.,
      ~.x %>% 
        group_by(ciiu, sexo) %>% 
        summarise(female = survey_total(na.rm = T)) %>% 
        filter(sexo == "Mujer" & !is.na(ciiu)) %>% 
        select(ciiu, female)) %>% 
  imap(., 
       ~.x %>% mutate(ano = .y))

totales = bind_rows(a)

total_ano = totales %>% 
  group_by(ano) %>% 
  summarise(ciiu = "Total",
            female = sum(female)) %>% 
  ungroup()

totales = rbind(totales, total_ano) 

totales = totales[order(totales$ano, totales$ciiu),]

write_xlsx(totales, "output/data/female_ene_99-19.xlsx")

# Edad promedio por rama y año ---------------------------------------------
#edad

# % de autoempleados por rama y año ---------------------------------------------
#self_empl

# % de expertiz por rama y año ---------------------------------------------
#skills


# % de contratos indefinidos por rama y año ---------------------------------------------
#contract_duration

# % de personas con 50 emp o más por rama y año ---------------------------------------------
#tamano

# Promedio de años de antiguedad por rama y año ---------------------------------------------
#job_seniority

# % personas con jornada completa por rama y año ---------------------------------------------
#contract_type

# % de desempleados por rama y año ---------------------------------------------
#unempl



