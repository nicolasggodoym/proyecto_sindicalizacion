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

enc = enc %>% 
  map(.,
    ~.x %>% 
      group_by(ciiu, cise) %>% 
      summarise(total = survey_total(na.rm = T)) %>% 
      filter(cise == "CP/Priv" & !is.na(ciiu)) %>% 
      select(ciiu, total)) %>% 
  imap(., 
       ~.x %>% mutate(ano = .y))

totales = bind_rows(enc)

total_ano = totales %>% 
  group_by(ano) %>% 
  summarise(ciiu = "Total",
            total = sum(total)) %>% 
  ungroup()

totales = rbind(totales, total_ano) 

totales = totales[order(totales$ano, totales$ciiu),]


# Exportar ----------------------------------------------------------------

write_xlsx(totales, "output/data/totales_ene_99-19.xlsx")


