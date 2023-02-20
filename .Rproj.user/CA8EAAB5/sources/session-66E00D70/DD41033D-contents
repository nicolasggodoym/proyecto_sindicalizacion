
# Procesamiento de datos CASEN 1990-2017 --------------------------------------

rm(list=ls())
# Cargar paquetes ---------------------------------------------------------
pacman::p_load(tidyverse,
               haven,
               sjmisc)

# Cargar datos ------------------------------------------------------------

c00 = read_dta("input/data/casen2000.dta")
c03 = read_dta("input/data/casen2003.dta")
c13 = read_dta("input/data/casen2013.dta")
c15 = read_dta("input/data/casen2015.dta")
c17 = read_dta("input/data/casen2017.dta")

# Explorar datos ----------------------------------------------------------

# Sindicalización


find_var(c00, "p18")
find_var(c03, "r18")
find_var(c13, "r9")#Participación en organizaciones soc civil
find_var(c15, "o24") #Participación en sindicatos/asociaciones de funcionarios/gremial/colegio profesional
find_var(c15, "r6") #Participación en organizaciones soc civil
find_var(c17, "o24") #Participación en sindicatos/asociaciones de funcionarios/gremial/colegio profesional
find_var(c17, "r6") #Participación en organizaciones soc civil

# Activos

find_var(c00, "activ")
find_var(c03, "activ")
find_var(c13, "activ")
find_var(c15, "activ")
find_var(c17, "activ")

# CISE

find_var(c00, "o10")
find_var(c03, "o9")
find_var(c13, "o15")
find_var(c15, "o15")
find_var(c17, "o15")

# CIUO

find_var(c00, "oficio")
find_var(c03, "oficio")
find_var(c13, "oficio1")
find_var(c15, "oficio1")
find_var(c17, "oficio1")

# CIIU

find_var(c00, "rama")
find_var(c03, "rama")
find_var(c13, "rama1")
find_var(c15, "rama1")
find_var(c17, "rama1")

# Tamaño empresa

find_var(c00, "o13")
find_var(c03, "o14")
find_var(c13, "o25")
find_var(c15, "o23")
find_var(c17, "o23")

# Presencia de contrato oral o escrito

find_var(c00, "o11")
find_var(c03, "o11")
find_var(c13, "o17")
find_var(c15, "o17")
find_var(c17, "o17")

# Duración contrato

#find_var(c00, "contrato")
#find_var(c03, "contrato")
#find_var(c13, "contrato")
find_var(c15, "o16") 
find_var(c17, "o16") 

# Jornada 

find_var(c00, "o12")
find_var(c03, "o13a")
find_var(c13, "o18")
find_var(c15, "o18") 
find_var(c17, "o18")

# Subcontrato

#find_var(c00, "contrato")
#find_var(c03, "contrato")
find_var(c13, "o20")
find_var(c15, "o20")
find_var(c17, "o20")

# Ingresos laborales informante

find_var(c00, "yopraj")
find_var(c03, "yopraj")
find_var(c13, "yoprcor")
find_var(c15, "yoprcor")
find_var(c17, "y1")

# Educacion

find_var(c00, "educ")
find_var(c03, "educ")
find_var(c13, "educ")
find_var(c15, "educ")
find_var(c17, "educ")

# SEXO

find_var(c00, "sexo")
find_var(c03, "sexo")
find_var(c13, "sexo")
find_var(c15, "sexo")
find_var(c17, "sexo")

# Estado civil

find_var(c00, "ecivil")
find_var(c03, "ecivil")
find_var(c13, "ecivil")
find_var(c15, "ecivil")
find_var(c17, "ecivil")

# Edad

find_var(c00, "edad")
find_var(c03, "edad")
find_var(c13, "edad")
find_var(c15, "edad")
find_var(c17, "edad")

# Identificador

find_var(c00, "folio")
find_var(c03, "f")
find_var(c13, "folio")
find_var(c15, "folio")
find_var(c17, "folio")

# Ponderador

find_var(c00, "expr")
find_var(c00, "estrato")
find_var(c03, "expr")
find_var(c03, "estrato")
find_var(c13, "expr")
find_var(c13, "estrato")
find_var(c15, "expr")
find_var(c15, "varstrat")
find_var(c15, "varunit")
find_var(c17, "expr")
find_var(c17, "varstrat")
find_var(c17, "varunit")


# Procesamiento -----------------------------------------------------------


# 2000 --------------------------------------------------------------------

c00 = c00 %>% 
  filter(activ == 1) %>% 
  select(id = folio,
         exp = expr,
         strat = estrato,
         edad,
         sexo,
         ecivil,
         educ,
         part = p18,
         ing = yopraj,
         jornada = o12,
         contrato = o11,
         tamano = o13,
         rama, 
         ciuo = oficio,
         cise = o10)

# 2003 --------------------------------------------------------------------

c03 = c03 %>% 
  filter(activ == 1) %>% 
  select(id = f,
         exp = expr,
         strat = estrato,
         edad,
         sexo,
         ecivil,
         educ,
         part = r18,
         ing = yopraj,
         jornada = o13a,
         contrato = o11,
         tamano = o14,
         rama, 
         ciuo = oficio,
         cise = o9)

# 2013 --------------------------------------------------------------------

c13 = c13 %>% 
  filter(activ == 1) %>% 
  select(id = folio,
         exp = expr,
         strat = varstrat,
         unit = varunit,
         edad,
         sexo,
         ecivil,
         educ,
         part = r9,
         ing = yoprcor,
         subcontrato = o20,
         jornada = o18,
         contrato = o17,
         tamano = o25,
         rama = rama1, 
         ciuo = oficio1,
         cise = o15)

# 2015 --------------------------------------------------------------------

c15 = c15 %>% 
  filter(activ == 1) %>% 
  select(id = folio,
         exp = expr,
         strat = varstrat,
         unit = varunit,
         edad,
         sexo,
         ecivil,
         educ,
         part = r6,
         p_sind = o24a,
         p_func = o24b,
         p_grem = o24c,
         p_col = o24d,
         ing = yoprcor,
         subcontrato = o20,
         jornada = o18,
         contrato = o17,
         duracion = o16,
         tamano = o23,
         rama = rama1, 
         ciuo = oficio1,
         cise = o15)

# 2017 --------------------------------------------------------------------

c17 = c17 %>% 
  filter(activ == 1) %>% 
  select(id = folio,
         exp = expr,
         strat = varstrat,
         unit = varunit,
         edad,
         sexo,
         ecivil,
         educ,
         part = r6,
         p_sind = o24a,
         p_func = o24b,
         p_grem = o24c,
         p_col = o24d,
         ing = y1,
         subcontrato = o20,
         jornada = o18,
         contrato = o17,
         duracion = o16,
         tamano = o23,
         rama = rama1, 
         ciuo = oficio1,
         cise = o15)


## Filtrar y seleccionar variables -----------------------------------------


## Procesar variables ------------------------------------------------------



