rm(list = ls())


# Estimación totales CASEN ------------------------------------------------



# Cargar paquetes ---------------------------------------------------------

pacman::p_load(tidyverse,
               survey,
               srvyr)


# Cargar data -------------------------------------------------------------

data = readRDS("output/data/casen_proc.rds")

#Objeto encuesta

enc = map(data,
          ~.x %>% 
            filter(!is.na(exp)) %>% 
            as_survey_design(ids = id,
                            strata = strat,
                            weights = exp))

years = 1998:2009



# contract_duration -------------------------------------------------------
a = map(enc,
        ~.x %>% 
          group_by(rama, salaried_selfempl, contract_duration) %>% 
          summarise(contract_duration = survey_prop(na.rm=T)))



# company_size ------------------------------------------------------------


# job_seniority -----------------------------------------------------------









