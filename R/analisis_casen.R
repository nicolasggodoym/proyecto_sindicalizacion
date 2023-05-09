rm(list=ls())
# Análisis descriptivo CASEN general --------------------------------------


# Cargar paquetes ---------------------------------------------------------
pacman::p_load(tidyverse,
               sjmisc,
               sjPlot,
               ggplot2,
               survey,
               srvyr)

# Cargar datos ------------------------------------------------------------
data = readRDS("output/data/data.rds")

# Crear gráficos ----------------------------------------------------------
a = data %>% 
  filter(salaried_selfempl == 1) %>% 
  select(id, exp, strat, year, unionized) %>% 
  na.omit() %>% 
  as_survey_design(#ids = unit,
                   strata = strat,
                   weights = exp) 

a %>% 
  group_by(year, unionized) %>% 
  summarise(per = survey_prop(na.rm = T)) %>% 
  filter(unionized == "1") %>% 
  mutate(per = per*100) %>% 
  ggplot(aes(year, y = per)) +
  geom_line() +
  geom_point() +
  geom_label(aes(label = round(per,3))) +
  scale_x_continuous(breaks = c(2000, 2003, 2013, 2015, 2017)) +
  scale_y_continuous(limits = c(0, 3), n.breaks = 8) +
  labs(title = "Unionized (2003-2017)",
       x = "Año", y = "%")

ggsave("output/img/unionized.jpeg", dpi = 500)

a = data %>% 
  filter(salaried_selfempl == 1) %>% 
  select(id, exp, strat, unit, year, unionized2) %>% 
  na.omit() %>% 
  as_survey_design(ids = unit,
                   strata = strat,
                   weights = exp) 

a %>% 
  group_by(year, unionized2) %>% 
  summarise(per = survey_prop(na.rm = T)) %>% 
  filter(unionized2 == "1") %>% 
  mutate(per = per*100) %>% 
  ggplot(aes(year, y = per)) +
  geom_line() +
  geom_point() +
  geom_label(aes(label = round(per,2))) +
  scale_x_continuous(limits = c(2015, 2017), n.breaks = 3) +
  scale_y_continuous(limits = c(0, 15), n.breaks = 8) +
  labs(title = "Unionized2 (2015-2017)",
       x = "Año", y = "%")
  
ggsave("output/img/unionized2.jpeg", dpi = 500)

