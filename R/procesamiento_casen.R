
# Procesamiento de datos CASEN 1990-2017 --------------------------------------

rm(list=ls())
# Cargar paquetes ---------------------------------------------------------
pacman::p_load(tidyverse,
               car,
               haven,
               sjmisc,
               readxl)

# Cargar datos ------------------------------------------------------------

c00 = read_dta("input/data/casen2000.dta")
c03 = read_dta("input/data/casen2003.dta")
c13 = read_dta("input/data/casen2013.dta")
c15 = read_dta("input/data/casen2015.dta")
c17 = read_dta("input/data/casen2017.dta")
# c00 = readRDS("output/data/c00.rds")
# c03 = readRDS("output/data/c03.rds")
# c13 = readRDS("output/data/c13.rds")
# c15 = readRDS("output/data/c15.rds")
# c17 = readRDS("output/data/c17.rds")# c17 = readRDS("output/data/c17.rds")


# Explorar datos ----------------------------------------------------------

# Sindicalización


# find_var(c00, "p18")
# find_var(c03, "r18")
# find_var(c13, "r9")#Participación en organizaciones soc civil
# find_var(c15, "o24") #Participación en sindicatos/asociaciones de funcionarios/gremial/colegio profesional
# find_var(c15, "r6") #Participación en organizaciones soc civil
# find_var(c17, "o24") #Participación en sindicatos/asociaciones de funcionarios/gremial/colegio profesional
# find_var(c17, "r6") #Participación en organizaciones soc civil

# Activos

# find_var(c00, "activ")
# find_var(c03, "activ")
# find_var(c13, "activ")
# find_var(c15, "activ")
# find_var(c17, "activ")

# CISE

# find_var(c00, "o10")
# find_var(c03, "o9")
# find_var(c13, "o15")
# find_var(c15, "o15")
# find_var(c17, "o15")

# CIUO

# find_var(c00, "oficio")
# find_var(c03, "oficio")
# find_var(c13, "oficio1")
# find_var(c15, "oficio1")
# find_var(c17, "oficio1")

# CIIU

# find_var(c00, "rama")
# find_var(c03, "rama")
# find_var(c13, "rama1")
# find_var(c15, "rama1")
# find_var(c17, "rama1")

# Tamaño empresa

# find_var(c00, "o13")
# find_var(c03, "o14")
# find_var(c13, "o25")
# find_var(c15, "o23")
# find_var(c17, "o23")

# Presencia de contrato oral o escrito

# find_var(c00, "o11")
# find_var(c03, "o11")
# find_var(c13, "o17")
# find_var(c15, "o17")
# find_var(c17, "o17")

# Duración contrato

# find_var(c15, "o16") 
# find_var(c17, "o16") 

# Jornada 

# find_var(c00, "o12")
# find_var(c03, "o13a")
# find_var(c13, "o18")
# find_var(c15, "o18") 
# find_var(c17, "o18")

# Subcontrato


# find_var(c13, "o20")
# find_var(c15, "o20")
# find_var(c17, "o20")

# Ingresos laborales informante

# find_var(c00, "yopraj")
# find_var(c03, "yopraj")
# find_var(c13, "yoprcor")
# find_var(c15, "yoprcor")
# find_var(c17, "y1")

# Educacion

# find_var(c00, "educ")
# find_var(c03, "educ")
# find_var(c13, "educ")
# find_var(c15, "educ")
# find_var(c17, "educ")

# SEXO

# find_var(c00, "sexo")
# find_var(c03, "sexo")
# find_var(c13, "sexo")
# find_var(c15, "sexo")
# find_var(c17, "sexo")

# Estado civil

# find_var(c00, "ecivil")
# find_var(c03, "ecivil")
# find_var(c13, "ecivil")
# find_var(c15, "ecivil")
# find_var(c17, "ecivil")

# Edad

# find_var(c00, "edad")
# find_var(c03, "edad")
# find_var(c13, "edad")
# find_var(c15, "edad")
# find_var(c17, "edad")

# Identificador

# find_var(c00, "folio")
# find_var(c03, "f")
# find_var(c13, "folio")
# find_var(c15, "folio")
# find_var(c17, "folio")

# Ponderador

# find_var(c00, "expr")
# find_var(c00, "estrato")
# find_var(c03, "expr")
# find_var(c03, "estrato")
# find_var(c13, "expr")
# find_var(c13, "estrato")
# find_var(c15, "expr")
# find_var(c15, "varstrat")
# find_var(c15, "varunit")
# find_var(c17, "expr")
# find_var(c17, "varstrat")
# find_var(c17, "varunit")


# Procesamiento -----------------------------------------------------------


# 2000 --------------------------------------------------------------------

c00 = c00 %>% 
  select(id = folio,
         exp = expr,
         strat = estrato,
         edad,
         gender = sexo,
         married = ecivil,
         educ,
         unionized =  p18,
         income = yopraj,
         contract_type = o12,
         contract = o11,
         company_size = o13,
         rama,
         ciuo = oficio,
         cise = o10,
         pco1) %>%
  mutate_at(vars(everything(), -company_size), ~as.numeric(.)) %>% 
  mutate(gender = car::recode(.$gender, c("1 = 'Male';
                                      2 = 'Female'"), as.factor = T),
         married = car::recode(.$married, c("c(1,2) = '1';
                                          c(3,4,5,6,7) = '0'"), as.factor = T),
         educ = car::recode(.$educ, c("c(1,2,3,4,6) = '1. Incomplete secondary or less';
                                      c(5,7) = '2. Secondary complete';
                                      c(8,9,10) = '3. Technical degree (completed) or incomplete tertiary';
                                      11 = '4. Completed tertiary or more';
                                      99 = NA"), 
                            as.factor = T),
         unionized =  factor(case_when(unionized %in% c(10, 18) ~ "1", 
                                       !unionized %in% c(10, 18, 99) &  !is.na(unionized) ~ "0",
                                       TRUE ~ NA_character_)),
         contract = car::recode(.$contract, c("c(1,2,3) = 1;
                                              c(4,5) = 0"),
                                as.factor = T),
         contract_duration = car::recode(.$contract, c("1 = 1;
                                                   c(2,3,4,5) = 0"), 
                                     as.factor = T),
         contract_type = case_when(contract_type >= 40 ~ "1",
                                   contract_type < 40 ~ "2",
                                   TRUE ~ NA_character_), 
         company_size = car::recode(.$company_size, c("c('A', 'B', 'C') = '1. Micro';
                                          'D' = '2. Pequeña';
                                          'E' = '3. Mediana';
                                          'F' = '4. Grande';
                                          c('X', '') = NA"), 
                              as.factor = T),
         income_log = log(income),
         rama = factor(case_when(rama == 1 ~ "1. Agricultura, ganadería, silvicultura y pesca",
                                 rama == 2 ~ "2. Minería",
                                 rama == 3 ~ "3. Industrias manufactureras",
                                 rama == 4 ~ "4. Suministro de electricidad, gas y agua",
                                 rama == 5 ~ "5. Construcción",
                                 rama == 6 ~ "6. Comercio, hoteles y restaurantes",
                                 rama == 7 ~ "7. Transporte y comunicaciones",
                                 rama == 8 ~ "8. Servicios financieros, inmobiliarios y empresariales",
                                 rama == 9 ~ "9. Servicios sociales, domésticos, profesionales y otros",
                                 TRUE ~ NA_character_)),
         ciuo = factor(case_when(ciuo == 2 ~ "1. Altos cargos públicos",
                          ciuo == 3 ~ "2. Profesionales",
                          ciuo == 4 ~ "3. Técnicos y profesionales niv. medio",
                          ciuo == 5 ~ "4. Empleados de oficina",
                          ciuo == 6 ~ "5. Trab. servicios y comercio",
                          ciuo == 7 ~ "6. Agricultores y calificados",
                          ciuo == 8 ~ "7. Oficiales, operarios y artesanos",
                          ciuo == 9 ~ "8. Operadores y montadores",
                          ciuo == 10 ~ "9. Trab. no calificados",
                          TRUE ~ NA_character_)),
         private_sector = car::recode(.$cise, c("c(1,2,5,6,7) = 1;
                                                c(3,4) = 0;
                                                c(8,9) = NA"), 
                                      as.factor = T),
         salaried_selfempl = car::recode(.$cise, c("c(2,3,4,5,6,7) = 1;
                                                   c(1) = 0;
                                                   c(8,9) = NA"), 
                                         as.factor = T),
         year = 2000)

# 2003 --------------------------------------------------------------------

c03 = c03 %>% 
   select(id = f,
         exp = expr,
         strat = estrato,
         edad,
         gender = sexo,
         married = ecivil,
         educ,
         unionized =  r18,
         income = yopraj,
         contract_type = o13a,
         contract = o11,
         company_size = o14,
         rama, 
         ciuo = oficio,
         cise = o9,
         pco1) %>% 
  mutate_at(vars(everything(), -company_size), ~as.numeric(.)) %>% 
  mutate(gender = car::recode(.$gender, c("1 = 'Male';
                                      2 = 'Female'"), as.factor = T),
         married = car::recode(.$married, c("c(1,2) = '1';
                                          c(3,4,5,6,7) = '0'"), as.factor = T),
         educ = car::recode(.$educ, c("c(1,2,3,4,6) = '1. Incomplete secondary or less';
                                      c(5,7) = '2. Secondary complete';
                                      c(8,9,10) = '3. Technical degree (completed) or incomplete tertiary';
                                      11 = '4. Completed tertiary or more';
                                      99 = NA"), 
                            as.factor = T),
         unionized =  factor(case_when(unionized %in% c(11, 20) ~ "1", 
                                       !unionized %in% c(11, 20, 99) &  !is.na(unionized) ~ "0",
                                       TRUE ~ NA_character_)),
         contract = car::recode(.$contract, c("c(1,2) = 1;
                                              3 = 0;
                                              9 = NA"),
                                as.factor = T),
         contract_duration = car::recode(.$contract, c("1 = 1;
                                                   c(2,3,4,5) = 0;
                                                       9 = NA"), 
                                         as.factor = T),
         contract_type = factor(case_when(contract_type == 1 ~ "1",
                                   contract_type == 2 ~ "2",
                                   TRUE ~ NA_character_)), 
         company_size = car::recode(.$company_size, c("c('A', 'B', 'C') = '1. Micro';
                                          'D' = '2. Pequeña';
                                          'E' = '3. Mediana';
                                          'F' = '4. Grande';
                                          c('X', '') = NA"), 
                                    as.factor = T),
         income_log = log(income),
         rama = factor(case_when(rama == 1 ~ "1. Agricultura, ganadería, silvicultura y pesca",
                                 rama == 2 ~ "2. Minería",
                                 rama == 3 ~ "3. Industrias manufactureras",
                                 rama == 4 ~ "4. Suministro de electricidad, gas y agua",
                                 rama == 5 ~ "5. Construcción",
                                 rama == 6 ~ "6. Comercio, hoteles y restaurantes",
                                 rama == 7 ~ "7. Transporte y comunicaciones",
                                 rama == 8 ~ "8. Servicios financieros, inmobiliarios y empresariales",
                                 rama == 9 ~ "9. Servicios sociales, domésticos, profesionales y otros",
                                 TRUE ~ NA_character_)),
         ciuo = factor(case_when(ciuo == 2 ~ "1. Altos cargos públicos",
                                 ciuo == 3 ~ "2. Profesionales",
                                 ciuo == 4 ~ "3. Técnicos y profesionales niv. medio",
                                 ciuo == 5 ~ "4. Empleados de oficina",
                                 ciuo == 6 ~ "5. Trab. servicios y comercio",
                                 ciuo == 7 ~ "6. Agricultores y calificados",
                                 ciuo == 8 ~ "7. Oficiales, operarios y artesanos",
                                 ciuo == 9 ~ "8. Operadores y montadores",
                                 ciuo == 10 ~ "9. Trab. no calificados",
                                 TRUE ~ NA_character_)),
         private_sector = car::recode(.$cise, c("c(1,2,5,6,7) = 1;
                                                c(3,4) = 0;
                                                c(8,9) = NA"), 
                                      as.factor = T),
         salaried_selfempl = car::recode(.$cise, c("c(2,3,4,5,6,7) = 1;
                                                   c(1) = 0;
                                                   c(8,9) = NA"), 
                                         as.factor = T),
         year = 2003)

# 2013 --------------------------------------------------------------------

c13 = c13 %>% 
   select(id = folio,
         exp = expr,
         strat = varstrat,
         unit = varunit,
         edad,
         gender = sexo,
         married = ecivil,
         educ,
         unionized =  r9,
         income = yoprcor,
         subcontracted = o20,
         contract_type = o18,
         contract = o17,
         company_size = o25,
         rama = rama1, 
         ciuo = oficio1,
         cise = o15,
         pco1) %>% 
  mutate_at(vars(everything(), -company_size), ~as.numeric(.)) %>% 
  mutate(gender = car::recode(.$gender, c("1 = 'Male';
                                      2 = 'Female'"), as.factor = T),
         married = car::recode(.$married, c("c(1,2) = '1';
                                          c(3,4,5,6,7) = '0'"), as.factor = T),
         educ = car::recode(.$educ, c("c(0,1,2,3,4) = '1. Incomplete secondary or less';
                                      c(5,6) = '2. Secondary complete';
                                      c(7,8,9) = '3. Technical degree (completed) or incomplete tertiary';
                                      c(10,11,12) = '4. Completed tertiary or more';
                                      99 = NA"), 
                            as.factor = T),
         unionized =  factor(case_when(unionized == 12 ~ "1", 
                                       !unionized %in% c(12, 99) &  !is.na(unionized) ~ "0",
                                       TRUE ~ NA_character_)),
         contract = car::recode(.$contract, c("c(1,2) = 1;
                                              3 = 0;
                                              9 = NA"),
                                as.factor = T),
         contract_duration = car::recode(.$contract, c("1 = 1;
                                                   2 = 0"), 
                                         as.factor = T),
         contract_type = factor(case_when(contract_type %in% c(1,3) ~ "1",
                                          contract_type == 2 ~ "2",
                                          TRUE ~ NA_character_)),
         subcontracted = factor(case_when(subcontracted == 1 ~ "0",
                                          subcontracted %in% c(2,3) ~ "1",
                                           TRUE ~ NA_character_)), 
         company_size = car::recode(.$company_size, c("c('A', 'B', 'C') = '1. Micro';
                                          'D' = '2. Pequeña';
                                          'E' = '3. Mediana';
                                          'F' = '4. Grande';
                                           c('X', '') = NA"), 
                                    as.factor = T),
         income_log = log(income),
         rama = factor(case_when(rama %in% c(1,2) ~ "1. Agricultura, ganadería, silvicultura y pesca",
                                 rama == 3 ~ "2. Minería",
                                 rama == 4 ~ "3. Industrias manufactureras",
                                 rama == 5 ~ "4. Suministro de electricidad, gas y agua",
                                 rama == 6 ~ "5. Construcción",
                                 rama %in% c(7,8) ~ "6. Comercio, hoteles y restaurantes",
                                 rama == 9 ~ "7. Transporte y comunicaciones",
                                 rama %in% c(10,11) ~ "8. Servicios financieros, inmobiliarios y empresariales",
                                 rama %in% c(12,13,14,15,16,17) ~ "9. Servicios sociales, domésticos, profesionales y otros",
                                 TRUE ~ NA_character_)),
         ciuo = factor(case_when(ciuo == 2 ~ "1. Altos cargos públicos",
                                 ciuo == 3 ~ "2. Profesionales",
                                 ciuo == 4 ~ "3. Técnicos y profesionales niv. medio",
                                 ciuo == 5 ~ "4. Empleados de oficina",
                                 ciuo == 6 ~ "5. Trab. servicios y comercio",
                                 ciuo == 7 ~ "6. Agricultores y calificados",
                                 ciuo == 8 ~ "7. Oficiales, operarios y artesanos",
                                 ciuo == 9 ~ "8. Operadores y montadores",
                                 ciuo == 10 ~ "9. Trab. no calificados",
                                 TRUE ~ NA_character_)),
         private_sector = car::recode(.$cise, c("c(1,2,5,6,7) = 1;
                                                c(3,4) = 0;
                                                c(8,9) = NA"), 
                                      as.factor = T),
         salaried_selfempl = car::recode(.$cise, c("c(2,3,4,5,6,7) = 1;
                                                   c(1) = 0;
                                                   c(8,9) = NA"), 
                                         as.factor = T),
         subcontracted = ifelse(subcontracted == 1, 1, 0),
         year = 2013)

# 2015 --------------------------------------------------------------------

c15 = c15 %>% 
  select(id = folio,
         exp = expr,
         strat = varstrat,
         unit = varunit,
         edad,
         gender = sexo,
         married = ecivil,
         educ,
         unionized =  r6,
         p_sind = o24a,
         p_func = o24b,
         p_grem = o24c,
         p_col = o24d,
         income = yoprcor,
         subcontracted = o20,
         contract_type = o18,
         contract = o17,
         contract_duration = o16,
         company_size = o23,
         rama = rama1, 
         ciuo = oficio1,
         cise = o15,
         pco1) %>% 
  mutate_at(vars(everything(), -company_size), ~as.numeric(.)) %>% 
  mutate(gender = car::recode(.$gender, c("1 = 'Male';
                                      2 = 'Female'"), as.factor = T),
         married = car::recode(.$married, c("c(1,2,3) = '1';
                                          c(4,5,6,7,8,9) = '0'"), as.factor = T),
         educ = car::recode(.$educ, c("c(0,1,2,3,4) = '1. Incomplete secondary or less';
                                      c(5,6) = '2. Secondary complete';
                                      c(7,8,9) = '3. Technical degree (completed) or incomplete tertiary';
                                      c(10,11,12) = '4. Completed tertiary or more';
                                      99 = NA"), 
                            as.factor = T),
         unionized =  factor(case_when(unionized == 12 ~ "1", 
                                       !unionized %in% c(12, 99) &  !is.na(unionized) ~ "0",
                                       TRUE ~ NA_character_)),
         unionized2 = factor(case_when(if_any(starts_with("p_"), ~ . == 1) ~ "1",
                                       if_any(starts_with("p_"), ~ . == 2) ~ "0",
                                       TRUE ~ NA_character_)),
         contract = car::recode(.$contract, c("c(1,2) = '1';
                                              c(3,4) = '0';
                                              9 = NA"),
                                as.factor = T),
         contract_duration = car::recode(.$contract_duration, c("1 = '1';
                                                                2 = '0';
                                                                9 = NA"), 
                                         as.factor = T),
         contract_type = case_when(contract_type %in% c(1,3) ~ "1",
                                   contract_type == 2 ~ "2",
                                   TRUE ~ NA_character_), 
         company_size = car::recode(.$company_size, c("c('A', 'B', 'C') = '1. Micro';
                                          'D' = '2. Pequeña';
                                          'E' = '3. Mediana';
                                          'F' = '4. Grande';
                                          c('X', '') = NA"), 
                                    as.factor = T),
         income_log = log(income),
         rama = factor(case_when(rama %in% c(1,2) ~ "1. Agricultura, ganadería, silvicultura y pesca",
                                 rama == 3 ~ "2. Minería",
                                 rama == 4 ~ "3. Industrias manufactureras",
                                 rama == 5 ~ "4. Suministro de electricidad, gas y agua",
                                 rama == 6 ~ "5. Construcción",
                                 rama %in% c(7,8) ~ "6. Comercio, hoteles y restaurantes",
                                 rama == 9 ~ "7. Transporte y comunicaciones",
                                 rama %in% c(10,11) ~ "8. Servicios financieros, inmobiliarios y empresariales",
                                 rama %in% c(12,13,14,15,16,17) ~ "9. Servicios sociales, domésticos, profesionales y otros",
                                 TRUE ~ NA_character_)),
         ciuo = factor(case_when(ciuo == 2 ~ "1. Altos cargos públicos",
                                 ciuo == 3 ~ "2. Profesionales",
                                 ciuo == 4 ~ "3. Técnicos y profesionales niv. medio",
                                 ciuo == 5 ~ "4. Empleados de oficina",
                                 ciuo == 6 ~ "5. Trab. servicios y comercio",
                                 ciuo == 7 ~ "6. Agricultores y calificados",
                                 ciuo == 8 ~ "7. Oficiales, operarios y artesanos",
                                 ciuo == 9 ~ "8. Operadores y montadores",
                                 ciuo == 10 ~ "9. Trab. no calificados",
                                 TRUE ~ NA_character_)),
         private_sector = car::recode(.$cise, c("c(1,2,5,6,7) = 1;
                                                c(3,4) = 0;
                                                c(8,9) = NA"), 
                                      as.factor = T),
         salaried_selfempl = car::recode(.$cise, c("c(2,3,4,5,6,7) = 1;
                                                   c(1) = 0;
                                                   c(8,9) = NA"), 
                                         as.factor = T),
         year = 2015) %>% 
  select(-c(starts_with("p_")))

# 2017 --------------------------------------------------------------------

c17 = c17 %>% 
  select(id = folio,
         exp = expr,
         strat = varstrat,
         unit = varunit,
         edad,
         gender = sexo,
         married = ecivil,
         educ,
         unionized =  r6,
         p_sind = o24a,
         p_func = o24b,
         p_grem = o24c,
         p_col = o24d,
         income = y1,
         subcontracted = o20,
         contract_type = o18,
         contract = o17,
         contract_duration = o16,
         company_size = o23,
         rama = rama1, 
         ciuo = oficio1,
         cise = o15,
         pco1) %>% 
  mutate_at(vars(everything(), -company_size), ~as.numeric(.)) %>% 
  mutate(gender = car::recode(.$gender, c("1 = 'Male';
                                      2 = 'Female'"), as.factor = T),
         married = car::recode(.$married, c("c(1,2,3) = '1';
                                          c(4,5,6,7,8,9) = '0'"), as.factor = T),
         educ = car::recode(.$educ, c("c(0,1,2,3,4) = '1. Incomplete secondary or less';
                                      c(5,6) = '2. Secondary complete';
                                      c(7,8,9) = '3. Technical degree (completed) or incomplete tertiary';
                                      c(10,11,12) = '4. Completed tertiary or more';
                                      99 = NA")),
         unionized =  factor(case_when(unionized == 12 ~ "1", 
                                       !unionized %in% c(12, 99) &  !is.na(unionized) ~ "0",
                                       TRUE ~ NA_character_)),
         unionized2 = factor(case_when(if_any(starts_with("p_"), ~ . == 1) ~ "1",
                                       if_any(starts_with("p_"), ~ . == 2) ~ "0",
                                       TRUE ~ NA_character_)),
         contract = car::recode(.$contract, c("c(1,2) = '1';
                                              3 = '0';
                                              c(4,9) = NA"),
                                as.factor = T),
         contract_duration = car::recode(.$contract_duration, c("1 = '1';
                                                                2 = '0';
                                                                9 = NA"), 
                                         as.factor = T),
         contract_type = case_when(contract_type %in% c(1,3) ~ "1",
                                   contract_type == 2 ~ "2",
                                   TRUE ~ NA_character_), 
         company_size = car::recode(.$company_size, c("c('A', 'B', 'C') = '1. Micro';
                                          'D' = '2. Pequeña';
                                          'E' = '3. Mediana';
                                          'F' = '4. Grande';
                                          c('X', '') = NA"), 
                                    as.factor = T),
         income_log = log(income),
         rama = factor(case_when(rama %in% c(1,2) ~ "1. Agricultura, ganadería, silvicultura y pesca",
                                 rama == 3 ~ "2. Minería",
                                 rama == 4 ~ "3. Industrias manufactureras",
                                 rama == 5 ~ "4. Suministro de electricidad, gas y agua",
                                 rama == 6 ~ "5. Construcción",
                                 rama %in% c(7,8) ~ "6. Comercio, hoteles y restaurantes",
                                 rama == 9 ~ "7. Transporte y comunicaciones",
                                 rama %in% c(10,11) ~ "8. Servicios financieros, inmobiliarios y empresariales",
                                 rama %in% c(12,13,14,15,16,17) ~ "9. Servicios sociales, domésticos, profesionales y otros",
                                 TRUE ~ NA_character_)),
         ciuo = factor(case_when(ciuo == 2 ~ "1. Altos cargos públicos",
                                 ciuo == 3 ~ "2. Profesionales",
                                 ciuo == 4 ~ "3. Técnicos y profesionales niv. medio",
                                 ciuo == 5 ~ "4. Empleados de oficina",
                                 ciuo == 6 ~ "5. Trab. servicios y comercio",
                                 ciuo == 7 ~ "6. Agricultores y calificados",
                                 ciuo == 8 ~ "7. Oficiales, operarios y artesanos",
                                 ciuo == 9 ~ "8. Operadores y montadores",
                                 ciuo == 10 ~ "9. Trab. no calificados",
                                 TRUE ~ NA_character_)),
         private_sector = car::recode(.$cise, c("c(1,2,5,6,7) = 1;
                                                c(3,4) = 0;
                                                c(8,9) = NA"), 
                                      as.factor = T),
         salaried_selfempl = car::recode(.$cise, c("c(2,3,4,5,6,7) = 1;
                                                   c(1) = 0;
                                                   c(8,9) = NA"), 
                                         as.factor = T),
         year = 2017) %>% 
  select(-c(starts_with("p_")))


saveRDS(c00 ,"output/data/c00.rds")
saveRDS(c03 ,"output/data/c03.rds")
saveRDS(c13 ,"output/data/c13.rds")
saveRDS(c15 ,"output/data/c15.rds")
saveRDS(c17 ,"output/data/c17.rds")

data = bind_rows(c00,c03,c13,c15,c17)

rm(c00,c03,c13,c15,c17)

saveRDS(data, "output/data/data.rds")
