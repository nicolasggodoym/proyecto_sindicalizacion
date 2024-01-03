rm(list=ls())


# Estimación totales ENE 1986-2023 ----------------------------------------
# Por rama

options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")
# Cargar paquetes ---------------------------------------------------------

pacman::p_load(tidyverse,
               survey,
               srvyr,
               readxl,
               writexl,
               beepr)



# Cargar data -------------------------------------------------------------

load("output/data/ene_86_23.RData")

olas = as.character(seq(2010, 2023))

data.list = data.list[olas]

data.list = map(data.list, 
                 ~merge(.x, read_xlsx("input/data/dt/CAE_DT_armonizado.xlsx") %>% 
        select(ID2, CAENES_1d) %>% 
          unique, 
        by = "ID2", all.x = T))

# Transformar df en objetos encuesta --------------------------------------

enc = map(data.list,
          ~.x %>% as_survey_design(ids = cong,
                            strata = estrato,
                            weights = fe,
                            nest = T))
rm(data.list, olas)

# Estimar -----------------------------------------------------------------


# Totales por rama --------------------------------------------------------

a = enc %>% 
  map(.,
    ~.x %>% 
      group_by(CAENES_1d, cp_priv) %>% 
      summarise(total = survey_total(na.rm = T)) %>% 
      ungroup() %>% 
      filter(cp_priv == "CP/Priv" & !is.na(CAENES_1d)) %>% 
      select(CAENES_1d, total)) %>% 
  imap(., 
       ~.x %>% mutate(ano = .y))
beep(1)

totales = bind_rows(a)

total_ano = totales %>% 
  group_by(ano) %>% 
  summarise(CAENES_1d = "Total",
            total = sum(total)) %>% 
  ungroup()

totales = rbind(totales, total_ano) 
rm(total_ano)
totales = totales[order(totales$ano, totales$CAENES_1d),]

write_xlsx(totales, "output/data/totales_ene_99-23.xlsx")

# % de mujeres por rama y año ---------------------------------------------
#sexo

a = enc %>% 
  map(.,
      ~.x %>% 
        group_by(CAENES_1d, cp_priv, sexo) %>% 
        summarise(female = survey_total(na.rm = T)) %>% 
        ungroup() %>% 
        filter(sexo == "Mujer" & cp_priv == "CP/Priv" & !is.na(CAENES_1d)) %>% 
        select(CAENES_1d, female)) %>% 
  imap(., 
       ~.x %>% mutate(ano = .y))
beep(1)

female = bind_rows(a)

female_ano = female %>% 
  group_by(ano) %>% 
  summarise(CAENES_1d = "Total",
            female = sum(female)) %>% 
  ungroup()

female = rbind(female, female_ano) 
rm(female_ano)
female = female[order(female$ano, female$CAENES_1d),]

write_xlsx(totales, "output/data/female_ene_99-23.xlsx")

# Edad promedio por rama y año ---------------------------------------------
#edad

a = enc %>% 
  map(.,
      ~.x %>% 
        group_by(CAENES_1d, cp_priv) %>% 
        summarise(age = survey_mean(edad, na.rm = T)) %>% 
        ungroup() %>% 
        filter(cp_priv == "CP/Priv" & !is.na(CAENES_1d)) %>% 
        select(CAENES_1d, age)) %>% 
  imap(., 
       ~.x %>% mutate(ano = .y))
beep(1)

age = bind_rows(a)

age_ano = age %>% 
  group_by(ano) %>% 
  summarise(CAENES_1d = "Total",
            age = mean(age)) %>% 
  ungroup()

age = rbind(age, age_ano) 
rm(age_ano)
age = age[order(age$ano, age$CAENES_1d),]

# % de autoempleados por rama y año ---------------------------------------------
#self_empl

a = enc %>% 
  map(.,
      ~.x %>% 
        group_by(CAENES_1d, self_empl) %>% 
        summarise(a = survey_total(na.rm = T)) %>% 
        ungroup() %>% 
        filter(self_empl == "CP" & !is.na(CAENES_1d)) %>% 
        select(CAENES_1d, self_empl = a)) %>% 
  imap(., 
       ~.x %>% mutate(ano = .y))
beep(1)

self_empl = bind_rows(a)

self_empl_ano = self_empl %>% 
  group_by(ano) %>% 
  summarise(CAENES_1d = "Total",
            self_empl = sum(self_empl)) %>% 
  ungroup()

self_empl = rbind(self_empl, self_empl_ano) 
rm(self_empl_ano)
self_empl = self_empl[order(self_empl$ano, self_empl$CAENES_1d),]

# % de expertiz por rama y año ---------------------------------------------
#skills

a = enc %>% 
  map(.,
      ~.x %>% 
        group_by(CAENES_1d, cp_priv, skills) %>% 
        summarise(a = survey_total(na.rm = T)) %>% 
        ungroup() %>% 
        filter(skills == "Experto" & cp_priv == "CP/Priv" & !is.na(CAENES_1d)) %>% 
        select(CAENES_1d, skills = a)) %>% 
  imap(., 
       ~.x %>% mutate(ano = .y))
beep(1)

skills = bind_rows(a)

skills_ano = skills %>% 
  group_by(ano) %>% 
  summarise(CAENES_1d = "Total",
            skills = sum(skills)) %>% 
  ungroup()

skills = rbind(skills, skills_ano) 
rm(skills_ano)
skills = skills[order(skills$ano, skills$CAENES_1d),]

# % de contratos indefinidos por rama y año ---------------------------------------------
#contract_duration

a = enc %>% 
  map_if(., 
      ~mean(.x$variables$ano) %in% c(2010:2023),
      ~.x %>% 
        group_by(CAENES_1d, cp_priv, contract_duration) %>% 
        summarise(a = survey_total(na.rm = T)) %>% 
        ungroup() %>% 
        filter(contract_duration == "Indefinido" & cp_priv == "CP/Priv" & !is.na(CAENES_1d)) %>% 
        select(CAENES_1d, contract_duration = a)) %>% 
  imap(., 
       ~.x %>% mutate(ano = .y))

olas = as.character(seq(2010, 2023))

a = a[olas]

beep(1)

contract_duration = bind_rows(a)

contract_duration_ano = contract_duration %>% 
  group_by(ano) %>% 
  summarise(CAENES_1d = "Total",
            contract_duration = sum(contract_duration)) %>% 
  ungroup()

contract_duration = rbind(contract_duration, contract_duration_ano) 
rm(contract_duration_ano)
contract_duration = contract_duration[order(contract_duration$ano, contract_duration$CAENES_1d),]

# % de personas con 50 emp o más por rama y año ---------------------------------------------
#tamano

a = enc %>% 
  map_if(., 
         ~mean(.x$variables$ano) %in% c(2010:2023),
      ~.x %>% 
        group_by(CAENES_1d, cp_priv, tamano) %>% 
        summarise(a = survey_total(na.rm = T)) %>% 
        ungroup() %>% 
        filter(tamano == "Más de 50" & cp_priv == "CP/Priv" & !is.na(CAENES_1d)) %>% 
        select(CAENES_1d, firm_size = a)) %>% 
  imap(., 
       ~.x %>% mutate(ano = .y))
beep(1)

olas = as.character(seq(2010, 2023))

a = a[olas]

tamano = bind_rows(a)

tamano_ano = tamano %>% 
  group_by(ano) %>% 
  summarise(CAENES_1d = "Total",
            firm_size = sum(firm_size)) %>% 
  ungroup()

tamano = rbind(tamano, tamano_ano) 
rm(tamano_ano)
tamano = tamano[order(tamano$ano, tamano$CAENES_1d),]

# Promedio de años de antiguedad por rama y año ---------------------------------------------
#job_seniority

a = enc %>% 
  map_if(., 
         ~mean(.x$variables$ano) %in% c(2010:2023),
      ~.x %>% 
        group_by(CAENES_1d, cp_priv) %>% 
        summarise(a = survey_mean(job_seniority, na.rm = T)) %>% 
        ungroup() %>% 
        filter(cp_priv == "CP/Priv" & !is.na(CAENES_1d)) %>% 
        select(CAENES_1d, job_seniority = a)) %>% 
  imap(., 
       ~.x %>% mutate(ano = .y))
beep(1)

olas = as.character(seq(2010, 2023))

a = a[olas]

job_seniority = bind_rows(a)

job_seniority_ano = job_seniority %>% 
  group_by(ano) %>% 
  summarise(CAENES_1d = "Total",
            job_seniority = mean(job_seniority)) %>% 
  ungroup()

job_seniority = rbind(job_seniority, job_seniority_ano) 
rm(job_seniority_ano)
job_seniority = job_seniority[order(job_seniority$ano, job_seniority$CAENES_1d),]

# % personas con jornada completa por rama y año ---------------------------------------------
#contract_type

a = enc %>% 
  map_if(., 
         ~mean(.x$variables$ano) %in% c(2010:2023),
      ~.x %>% 
        group_by(CAENES_1d, cp_priv, contract_type) %>% 
        summarise(a = survey_total(na.rm = T)) %>% 
        ungroup() %>% 
        filter(contract_type == "Completa" & cp_priv == "CP/Priv" & !is.na(CAENES_1d)) %>% 
        select(CAENES_1d, contract_type = a)) %>% 
  imap(., 
       ~.x %>% mutate(ano = .y))
beep(1)

olas = as.character(seq(2010, 2023))

a = a[olas]

contract_type = bind_rows(a)

contract_type_ano = contract_type %>% 
  group_by(ano) %>% 
  summarise(CAENES_1d = "Total",
            contract_type = sum(contract_type)) %>% 
  ungroup()

contract_type = rbind(contract_type, contract_type_ano) 
rm(contract_type_ano)
contract_type = contract_type[order(contract_type$ano, contract_type$CAENES_1d),]

# % de desempleados por rama y año ---------------------------------------------
#unempl
# 
# a = enc %>% 
#   map_if(., 
#          ~mean(.x$variables$ano) %in% c(2010:2023),
#       ~.x %>% 
#         group_by(unempl) %>% 
#         summarise(a = survey_total(na.rm = T)) %>% 
#         ungroup() #%>% 
#         #filter(unempl == "Desempleado") %>% 
#         #select(unempl = a)
#       ) %>% 
#   imap(., 
#        ~.x %>% mutate(ano = .y))
# beep(1)
# 
# olas = as.character(seq(2010, 2023))
# 
# a = a[olas]
# 
# unempl = bind_rows(a)
# 
# unempl_ano = unempl %>% 
#   group_by(ano) %>% 
#   summarise(#CAENES_1d = "Total",
#             unempl = sum(unempl)) %>% 
#   ungroup()
# 
# unempl = rbind(unempl, unempl) 
# rm(unempl_ano)
# unempl = unempl[order(unempl$ano),]
# unempl = unique(unempl)

# Ocupados ----------------------------------------------------------------
# 
# a = enc %>% 
#   map(~.x %>% 
#         mutate(unempl = ifelse(cise %in% c(1:7) & edad >= 15, "PA", "NO"))) %>% 
#   map_if(., 
#          ~mean(.x$variables$ano) %in% c(2010:2023),
#          ~.x %>% 
#            group_by(unempl) %>% 
#            summarise(a = survey_total(na.rm = T)) %>% 
#            ungroup() #%>% 
#            #filter(unempl == "PA") %>% 
#            #select(unempl = a),
#            ) %>% 
#   imap(., 
#        ~.x %>% mutate(ano = .y))
# beep(1)
# 
# olas = as.character(seq(2010, 2023))
# 
# a = a[olas]
# 
# unempl = bind_rows(a) %>% 
#   select(ano, unempl, a) %>% 
#   pivot_wider(id_cols = ano,
#               names_from = "unempl",
#               values_from = "a") %>% 
#   na.omit() %>% 
#   rowwise() %>% 
#   mutate(unempl = round((NO/sum(NO, PA, is.na(u)))*100,2)) %>% 
#   ungroup() %>% 
#   select(ano, unempl)
# 
# unempl_ano = unempl %>% 
#   group_by(ano) %>% 
#   summarise(#CAENES_1d = "Total",
#     unempl = sum(unempl)) %>% 
#   ungroup()
# 
# unempl = rbind(unempl, unempl) 
# rm(unempl_ano)
# unempl = unempl[order(unempl$ano),]
# unempl = unique(unempl)
# 
# write_xlsx(unempl, "output/data/ene_unempl.xlsx")
# Unificar ----------------------------------------------------------------

# lista = list(totales,
#              female,
#              age,
#              self_empl,
#              skills,
#              contract_duration,
#              tamano,
#              job_seniority,
#              contract_type#, unempl, cise
#              )

lista = list(totales,
             female,
             age,
             self_empl,
             skills,
             contract_duration,
             tamano,
             job_seniority,
             contract_type#, unempl, cise
             ) %>% 
  Reduce(function(x,y) merge (x,y, all = T), .) %>% 
  arrange(ano, CAENES_1d) %>% 
  rowwise() %>% 
  mutate(across(c(female, self_empl, skills, contract_duration, 
                  firm_size, contract_type),
                ~ifelse(!is.na(.), round((./total)*100,3), NA))#, #unempl = ifelse(CAENES_1d == "Total", round((unempl/total)*100,3), NA)
         ) %>% 
  ungroup() %>% 
  # arrange(CAENES_1d, ano) %>% 
  # mutate(across(c(female, self_empl, skills, contract_duration, 
  #                 firm_size, job_seniority, contract_type, unempl),
  #               ~lag(.),
  #               .names = "lag_{.col}")) %>% 
  arrange(ano, CAENES_1d) #%>%  filter(ano != "1998")


# Exportar ----------------------------------------------------------------

write_xlsx(lista, "output/data/ene_final_CAENES_1d.xlsx")
saveRDS(lista, "output/data/ene_final_CAENES_1d.rds")
