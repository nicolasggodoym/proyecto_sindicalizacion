rm(list=ls())

# Gráficos de dispersión ENE-RRAADT ID2 --------------------------------


# Cargar paquetes ---------------------------------------------------------
pacman::p_load(tidyverse, readxl, RColorBrewer, patchwork, ggplot2, ggpubr, ggrepel)

# Cargar data -------------------------------------------------------------
llave = read_xlsx("input/data/dt/CAE_DT_armonizado.xlsx") %>% 
  select(ID2, CAENES_1d) %>% unique
data = readRDS("output/data/data_dt_ene_finalID2.rds") %>% 
  mutate(t = ifelse(ano %in% 2010:2015, "2010-2015", "2016-2019"),
         n_sind_mil = n_sind_mil/100) %>% 
  group_by(t, ID2) %>% 
  summarise(across(c(tasa_afiliacion, n_huelgas, n_sind_mil, por_cobertura_cont),
                   ~mean(.))) %>% 
  ungroup() %>% 
  merge(.,
        llave,
        by = "ID2",
        all.x = T) %>%
  select(CAENES_1d, ID2, everything())


# Gráficos ----------------------------------------------------------------


# Afiliación sindical x Número de huelgas ---------------------------------

# data %>% 
#   filter(t == "2010-2015") %>% 
#   ggplot(aes(x = tasa_afiliacion, y = n_huelgas, label = ID2)) +
#   geom_point() +
#   geom_smooth(method="lm", se = F)+
#   labs(title = "Relación entre afiliación sindical y número de huelgas",
#        subtitle = "2010-2015",
#        x = "Tasa de afiliación sindical",
#        y = "Número de huelgas",
#        color = "Act. económica") +
#   geom_text_repel(vjust = 1.5) +
#   stat_cor(method="pearson",
#            label.x.npc = 0.78, label.y.npc = 0.95) +
#   theme_bw() +
#   theme(legend.position="bottom")

# ggsave("output/img/graficos_ene_dtrraa/densidad_nhuelgas_1115ID2.png", last_plot(),
#        width = 10, height = 8)

# data %>% 
#   filter(t == "2016-2019") %>% 
#   ggplot(aes(x = tasa_afiliacion, y = n_huelgas, label = ID2)) +
#   geom_point() +
#   geom_smooth(method="lm", se = F)+
#   labs(title = "Relación entre afiliación sindical y número de huelgas",
#        subtitle = "2016-2019",
#        x = "Tasa de afiliación sindical",
#        y = "Número de huelgas",
#        color = "Act. económica") +
#   geom_text_repel(vjust = 1.5) +
#   stat_cor(method="pearson",
#            label.x.npc = 0.78, label.y.npc = 0.95) +
#   theme_bw() +
#   theme(legend.position="bottom")
# 
# ggsave("output/img/graficos_ene_dtrraa/densidad_nhuelgas_1619ID2.png", last_plot(),
#        width = 10, height = 8)

# data %>% 
#   ggplot(aes(x = tasa_afiliacion, y = n_huelgas, label = ID2)) +
#   geom_point(aes(color = t)) +
#   geom_smooth(method="lm", se = F)+
#   labs(title = "Relación entre afiliación sindical y número de huelgas",
#        subtitle = "2011-2019",
#        x = "Tasa de afiliación sindical",
#        y = "Número de huelgas",
#        color = "Act. económica") +
#   geom_text_repel(vjust = 1.5) +
#   stat_cor(method="pearson",
#            label.x.npc = 0.78, label.y.npc = 0.95) +
#   theme_bw() +
#   theme(legend.position="bottom")
# 
# ggsave("output/img/graficos_ene_dtrraa/densidad_nhuelgas_1119ID2.png", last_plot(),
#        width = 10, height = 8)

# N sindicatos x N de huelgas ---------------------------------------------

# data %>% 
#   filter(t == "2010-2015") %>% 
#   ggplot(aes(x = n_sind_mil, y = n_huelgas, label = ID2)) +
#   geom_point() +
#   geom_smooth(method="lm", se = F)+
#   labs(title = "Relación entre fragmentación sindical y número de huelgas",
#        subtitle = "2010-2015",
#        x = "N. de sindicatos c/mil trabajadores",
#        y = "Número de huelgas",
#        color = "Act. económica") +
#   geom_text_repel(vjust = 1.5) +
#   stat_cor(method="pearson",
#            label.x.npc = 0.78, label.y.npc = 0.90) +
#   theme_bw() +
#   theme(legend.position="bottom")
# 
# ggsave("output/img/graficos_ene_dtrraa/fragmentacion_nhuelgas_1115ID2.png", last_plot(),
#        width = 10, height = 8)

# data %>% 
#   filter(t == "2016-2019") %>% 
#   ggplot(aes(x = n_sind_mil, y = n_huelgas, label = ID2)) +
#   geom_point() +
#   geom_smooth(method="lm", se = F)+
#   labs(title = "Relación entre fragmentación sindical y número de huelgas",
#        subtitle = "2016-2019",
#        x = "N. de sindicatos c/mil trabajadores",
#        y = "Número de huelgas",
#        color = "Act. económica") +
#   geom_text_repel(vjust = 1.5) +
#   stat_cor(method="pearson",
#            label.x.npc = 0.78, label.y.npc = 0.95) +
#   theme_bw() +
#   theme(legend.position="bottom")
# 
# ggsave("output/img/graficos_ene_dtrraa/fragmentacion_nhuelgas_1519ID2.png", last_plot(),
#        width = 10, height = 8)

# data %>% 
#   ggplot(aes(x = n_sind_mil, y = n_huelgas, label = ID2)) +
#   geom_point(aes(color = t)) +
#   geom_smooth(method="lm", se = F)+
#   labs(title = "Relación entre fragmentación sindical y número de huelgas",
#        subtitle = "2011-2019",
#        x = "N. de sindicatos c/mil trabajadores",
#        y = "Número de huelgas",
#        color = "Act. económica") +
#   geom_text_repel(vjust = 1.5) +
#   stat_cor(method="pearson",
#            label.x.npc = 0.78, label.y.npc = 0.87) +
#   theme_bw() +
#   theme(legend.position="bottom")
# 
# ggsave("output/img/graficos_ene_dtrraa/fragmentacion_nhuelgas_1119ID2.png", last_plot(),
#        width = 10, height = 8)



# Cobertura neg. colectiva x N huelgas ------------------------------------


data %>%
  filter(t == "2010-2015") %>%
  ggplot(aes(x = por_cobertura_cont, y = n_huelgas, label = ID2)) +
  geom_point(aes(color = CAENES_1d)) +
  geom_smooth(method="lm", se = F) +
  labs(title = "Relación entre tasa de cobertura de contratos colectivos y número de huelgas",
       subtitle = "2010-2015",
       x = "N. de sindicatos c/mil trabajadores",
       y = "Número de huelgas",
       color = "Act. económica") +
  geom_text_repel(vjust = 1.5) +
  stat_cor(method="pearson",
           label.x.npc = 0.78, label.y.npc = 0.90) +
  theme_bw() +
  theme(legend.position="bottom")

ggsave("output/img/graficos_ene_dtrraa/cobertura_nhuelgas_1015ID2.png", last_plot(),
       width = 10, height = 8)

data %>% 
  filter(t == "2016-2019") %>% 
  ggplot(aes(x = por_cobertura_cont, y = n_huelgas, label = ID2)) +
  geom_point(aes(color = CAENES_1d)) +
  geom_smooth(method="lm", se = F)+
  labs(title = "Relación entre tasa de cobertura de contratos colectivos y número de huelgas",
       subtitle = "2016-2019",
       x = "Tasa de cobertura de negociación colectiva",
       y = "Número de huelgas",
       color = "Act. económica") +
  geom_text_repel(vjust = 1.5) +
  stat_cor(method="pearson",
           label.x.npc = 0.78, label.y.npc = 0.95) +
  theme_bw() +
  theme(legend.position="bottom")

ggsave("output/img/graficos_ene_dtrraa/cobertura_nhuelgas_1519ID2.png", last_plot(),
       width = 10, height = 8)

data %>%
  ggplot(aes(x = por_cobertura_cont, y = n_huelgas, label = ID2)) +
  geom_point(aes(color = CAENES_1d, shape = t)) +
  geom_smooth(method="lm", se = F)+
  labs(title = "Relación entre tasa de cobertura de contratos colectivos y número de huelgas",
       subtitle = "2010-2019",
       x = "N. de sindicatos c/mil trabajadores",
       y = "Número de huelgas",
       color = "Act. económica",
       shape = "Período") +
  geom_text_repel(vjust = 1.5) +
  stat_cor(method="pearson",
           label.x.npc = 0.78, label.y.npc = 0.87) +
  theme_bw() +
  theme(legend.position="bottom")

ggsave("output/img/graficos_ene_dtrraa/cobertura_nhuelgas_1119ID2.png", last_plot(),
       width = 10, height = 8)
