rm(list = ls())


# Estimación totales CASEN ------------------------------------------------

# Cargar paquetes ---------------------------------------------------------

pacman::p_load(tidyverse,
               survey,
               srvyr,
               writexl)


# Cargar data -------------------------------------------------------------

data = readRDS("output/data/casen_proc.rds")

#Objeto encuesta

enc = map(data,
          ~.x %>% 
            filter(!is.na(exp)) %>% 
            as_survey_design(ids = id,
                            strata = strat,
                            weights = exp))

names(enc) = c("1998", "2000", "2003", "2006", "2009")

years = 1998:2009



# contract_duration -------------------------------------------------------
# a = imap(enc,
#         ~.x %>%
#           group_by(rama, salaried_selfempl, contract_duration) %>%
#           summarise(per = survey_prop(na.rm=T)) %>%
#           mutate(ano = .y))
# 
# saveRDS(a, "output/data/casen_contract_duration.rds")
a = readRDS("output/data/casen_contract_duration.rds")

contract_duration = bind_rows(a) %>% 
  ungroup() %>% 
  filter(if_all(c(salaried_selfempl, contract_duration), ~.== "1")) %>% 
  select(ano, rama, contract_duration=per) %>% 
  na.omit() %>% 
  mutate(contract_duration = round(contract_duration, 3)*100)
  


# company_size ------------------------------------------------------------
# a = imap(enc,
#          ~.x %>%
#            group_by(rama, salaried_selfempl, company_size) %>%
#            summarise(per = survey_prop(na.rm=T)) %>%
#            mutate(ano = .y))
# 
# saveRDS(a, "output/data/casen_company_size.rds")


a = readRDS("output/data/casen_company_size.rds")

company_size = bind_rows(a) %>% 
  ungroup() %>% 
  filter(if_all(c(salaried_selfempl, company_size), ~.== "1")) %>% 
  select(ano, rama, company_size=per) %>% 
  na.omit() %>% 
  mutate(company_size = round(company_size, 3)*100)

# job_seniority -----------------------------------------------------------
# a = imap(enc[c(4,5)],
#          ~.x %>%
#            group_by(rama, salaried_selfempl) %>%
#            summarise(mean = survey_mean(job_seniority, na.rm=T)) %>%
#            mutate(ano = .y))
# 
# saveRDS(a, "output/data/casen_job_seniority.rds")
a = readRDS("output/data/casen_job_seniority.rds")

job_seniority = bind_rows(a) %>% 
  ungroup() %>% 
  filter(salaried_selfempl == "1") %>% 
  select(ano, rama, job_seniority=mean) %>% na.omit

data = merge(contract_duration, company_size,
             by = c("ano", "rama"), all = T) %>% 
  merge(., job_seniority,
        by = c("ano", "rama"), all = T)

write_xlsx(data, "output/data/casen_98_09.xlsx")





