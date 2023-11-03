rm(list=ls())

# Procesamiento data DT ---------------------------------------------------


# Cargar paquetes ---------------------------------------------------------

pacman::p_load(tidyverse,
               haven,
               sjmisc)


# Cargar datos ------------------------------------------------------------

data = read_dta("input/data/CAE_RUT_FINAL.dta")

