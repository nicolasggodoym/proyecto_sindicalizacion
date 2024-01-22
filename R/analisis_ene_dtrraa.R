rm(list=ls())

# Gráficos de dispersión ENE-RRAADT CAENES 1d --------------------------------


# Cargar paquetes ---------------------------------------------------------
pacman::p_load(tidyverse, RColorBrewer, patchwork, ggplot2, ggpubr)

# Cargar data -------------------------------------------------------------

data = merge(readRDS("output/data/data_dt_ene_final.rds"),
              read_xlsx("input/data/dt/CAE_DT_armonizado.xlsx") %>% 
                select(ID2, CAENES_1d) %>% unique,
        by = "ID2",
        all.x = T) %>% 
  mutate(t = case_when(ano %in% 2010:2015 ~ "2010-2015", 
                       ano %in% 2016:2019 ~ "2016-2019", 
                       ano %in% 2020:2023 ~ "2020-2023",
                       TRUE ~ NA_character_)) %>% 
  group_by(t, CAENES_1d) %>% 
  summarise(across(c(tasa_afiliacion, n_huelgas, n_sind_mil, por_cobertura_cont),
                   ~mean(.))) %>% 
  ungroup() %>% 
  select(CAENES_1d, everything()) %>% 
  filter(if_all(c(CAENES_1d, t), ~!is.na(.)))


# Gráficos ----------------------------------------------------------------


# Afiliación sindical x Número de huelgas ---------------------------------

# data %>% 
#   filter(t == "2010-2015") %>% 
#   ggplot(aes(x = tasa_afiliacion, y = n_huelgas, label = CAENES_1d)) +
#   geom_point() +
#   geom_smooth(method="lm", se = F)+
#   labs(title = "Relación entre afiliación sindical y número de huelgas",
#        subtitle = "2010-2015",
#        x = "Tasa de afiliación sindical",
#        y = "Número de huelgas",
#        color = "Act. económica") +
#   geom_text(vjust = 1.5) +
#   stat_cor(method="pearson",
#            label.x.npc = 0.78, label.y.npc = 0.95) +
#   theme_bw() +
#   theme(legend.position="bottom")

# ggsave("output/img/graficos_ene_dtrraa/densidad_nhuelgas_1115.png", last_plot(),
#        width = 7, height = 5)

data %>% 
  filter(t == "2016-2019") %>% 
  ggplot(aes(x = tasa_afiliacion, y = n_huelgas, label = CAENES_1d)) +
  geom_point() +
  geom_smooth(method="lm", se = F)+
  labs(title = "Relación entre afiliación sindical y número de huelgas",
       subtitle = "2016-2019",
       x = "Tasa de afiliación sindical",
       y = "Número de huelgas",
       color = "Act. económica") +
  geom_text(vjust = 1.5) +
  stat_cor(method="pearson",
           label.x.npc = 0.78, label.y.npc = 0.95, aes(label = ..r.label..)) +
  theme_bw() +
  theme(legend.position="bottom")

ggsave("output/img/graficos_ene_dtrraa/densidad_nhuelgas_1619.png", last_plot(),
       width = 7, height = 5)

# data %>% 
#   ggplot(aes(x = tasa_afiliacion, y = n_huelgas, label = CAENES_1d)) +
#   geom_point(aes(color = t)) +
#   geom_smooth(method="lm", se = F)+
#   labs(title = "Relación entre afiliación sindical y número de huelgas",
#        subtitle = "2011-2019",
#        x = "Tasa de afiliación sindical",
#        y = "Número de huelgas",
#        color = "Act. económica") +
#   geom_text(vjust = 1.5) +
#   stat_cor(method="pearson",
#            label.x.npc = 0.78, label.y.npc = 0.95) +
#   theme_bw() +
#   theme(legend.position="bottom")
# 
# ggsave("output/img/graficos_ene_dtrraa/densidad_nhuelgas_1119.png", last_plot(),
#        width = 7, height = 5)

data %>% 
  filter(t == "2020-2023") %>% 
  ggplot(aes(x = tasa_afiliacion, y = n_huelgas, label = CAENES_1d)) +
  geom_point() +
  geom_smooth(method="lm", se = F)+
  labs(title = "Relación entre afiliación sindical y número de huelgas",
       subtitle = "2020-2023",
       x = "Tasa de afiliación sindical",
       y = "Número de huelgas",
       color = "Act. económica") +
  geom_text(vjust = 1.5) +
  stat_cor(method="pearson",
           label.x.npc = 0.78, label.y.npc = 0.95, aes(label = ..r.label..)) +
  theme_bw() +
  theme(legend.position="bottom")

ggsave("output/img/graficos_ene_dtrraa/densidad_nhuelgas_2023.png", last_plot(),
       width = 7, height = 5)


data %>%
  filter(!t == "2010-2015") %>% 
  ggplot(aes(x = tasa_afiliacion, y = n_huelgas, label = CAENES_1d)) +
  geom_point(aes(color = t)) +
  geom_smooth(method="lm", se = F)+
  labs(title = "Relación entre afiliación sindical y número de huelgas",
       subtitle = "2016-2023",
       x = "Tasa de afiliación sindical",
       y = "Número de huelgas",
       color = "Período") +
  geom_text_repel(vjust = 1.5) +
  stat_cor(method="pearson",
           label.x.npc = 0.78, label.y.npc = 0.87, aes(label = ..r.label..)) +
  theme_bw() +
  theme(legend.position="bottom")


ggsave("output/img/graficos_ene_dtrraa/cobertura_nhuelgas_1623.png", last_plot(),
       width = 9, height = 7)


# N sindicatos x N de huelgas ---------------------------------------------

# data %>% 
#   filter(t == "2010-2015") %>% 
#   ggplot(aes(x = n_sind_mil, y = n_huelgas, label = CAENES_1d)) +
#   geom_point() +
#   geom_smooth(method="lm", se = F)+
#   labs(title = "Relación entre fragmentación sindical y número de huelgas",
#        subtitle = "2010-2015",
#        x = "N. de sindicatos c/mil trabajadores",
#        y = "Número de huelgas",
#        color = "Act. económica") +
#   geom_text(vjust = 1.5) +
#   stat_cor(method="pearson",
#            label.x.npc = 0.78, label.y.npc = 0.90) +
#   theme_bw() +
#   theme(legend.position="bottom")
# 
# ggsave("output/img/graficos_ene_dtrraa/fragmentacion_nhuelgas_1115.png", last_plot(),
#        width = 7, height = 5)

data %>% 
  filter(t == "2016-2019") %>% 
  ggplot(aes(x = n_sind_mil, y = n_huelgas, label = CAENES_1d)) +
  geom_point() +
  geom_smooth(method="lm", se = F)+
  labs(title = "Relación entre fragmentación sindical y número de huelgas",
       subtitle = "2016-2019",
       x = "N. de sindicatos c/mil trabajadores",
       y = "Número de huelgas",
       color = "Act. económica") +
  geom_text(vjust = 1.5) +
  stat_cor(method="pearson",
           label.x.npc = 0.78, label.y.npc = 0.95, aes(label = ..r.label..)) +
  theme_bw() +
  theme(legend.position="bottom")

ggsave("output/img/graficos_ene_dtrraa/fragmentacion_nhuelgas_1619.png", last_plot(),
       width = 7, height = 5)

# data %>% 
#   ggplot(aes(x = n_sind_mil, y = n_huelgas, label = CAENES_1d)) +
#   geom_point(aes(color = t)) +
#   geom_smooth(method="lm", se = F)+
#   labs(title = "Relación entre fragmentación sindical y número de huelgas",
#        subtitle = "2011-2019",
#        x = "N. de sindicatos c/mil trabajadores",
#        y = "Número de huelgas",
#        color = "Act. económica") +
#   geom_text(vjust = 1.5) +
#   stat_cor(method="pearson",
#            label.x.npc = 0.78, label.y.npc = 0.87) +
#   theme_bw() +
#   theme(legend.position="bottom")
# 
# ggsave("output/img/graficos_ene_dtrraa/fragmentacion_nhuelgas_1119.png", last_plot(),
#        width = 7, height = 5)

data %>% 
  filter(t == "2020-2023") %>% 
  ggplot(aes(x = n_sind_mil, y = n_huelgas, label = CAENES_1d)) +
  geom_point() +
  geom_smooth(method="lm", se = F)+
  labs(title = "Relación entre fragmentación sindical y número de huelgas",
       subtitle = "2020-2023",
       x = "N. de sindicatos c/mil trabajadores",
       y = "Número de huelgas",
       color = "Act. económica") +
  geom_text(vjust = 1.5) +
  stat_cor(method="pearson",
           label.x.npc = 0.78, label.y.npc = 0.95, aes(label = ..r.label..)) +
  theme_bw() +
  theme(legend.position="bottom")

ggsave("output/img/graficos_ene_dtrraa/fragmentacion_nhuelgas_2023.png", last_plot(),
       width = 7, height = 5)


data %>%
  filter(!t == "2010-2015") %>% 
  ggplot(aes(x = n_sind_mil, y = n_huelgas, label = CAENES_1d)) +
  geom_point(aes(color = t)) +
  geom_smooth(method="lm", se = F)+
  labs(title = "Relación entre fragmentación sindical y número de huelgas",
       subtitle = "2016-2023",
       x = "N. de sindicatos c/mil trabajadoresN. de sindicatos c/mil trabajadores",
       y = "Número de huelgas",
       color = "Período") +
  geom_text_repel(vjust = 1.5) +
  stat_cor(method="pearson",
           label.x.npc = 0.78, label.y.npc = 0.87, aes(label = ..r.label..)) +
  theme_bw() +
  theme(legend.position="bottom")


ggsave("output/img/graficos_ene_dtrraa/fragmentacion_nhuelgas_1623.png", last_plot(),
       width = 9, height = 7)


# Cobertura neg. colectiva x N huelgas ------------------------------------


# data %>% 
#   filter(t == "2010-2015") %>% 
#   ggplot(aes(x = por_cobertura_cont, y = n_huelgas, label = CAENES_1d)) +
#   geom_point() +
#   geom_smooth(method="lm", se = F)+
#   labs(title = "Relación entre tasa de cobertura de contratos colectivos y número de huelgas",
#        subtitle = "2010-2015",
#        x = "N. de sindicatos c/mil trabajadores",
#        y = "Número de huelgas",
#        color = "Act. económica") +
#   geom_text(vjust = 1.5) +
#   stat_cor(method="pearson",
#            label.x.npc = 0.78, label.y.npc = 0.90) +
#   theme_bw() +
#   theme(legend.position="bottom")
# 
# ggsave("output/img/graficos_ene_dtrraa/cobertura_nhuelgas_1115.png", last_plot(),
#        width = 7, height = 5)

data %>% 
  filter(t == "2016-2019") %>% 
  ggplot(aes(x = por_cobertura_cont, y = n_huelgas, label = CAENES_1d)) +
  geom_point() +
  geom_smooth(method="lm", se = F)+
  labs(title = "Relación entre tasa de cobertura de contratos colectivos y número de huelgas",
       subtitle = "2016-2019",
       x = "Tasa de cobertura de negociación colectiva",
       y = "Número de huelgas",
       color = "Act. económica") +
  geom_text(vjust = 1.5) +
  stat_cor(method="pearson",
           label.x.npc = 0.78, label.y.npc = 0.95, aes(label = ..r.label..)) +
  theme_bw() +
  theme(legend.position="bottom")

ggsave("output/img/graficos_ene_dtrraa/cobertura_nhuelgas_1619.png", last_plot(),
       width = 7, height = 5)

# data %>% 
#   ggplot(aes(x = por_cobertura_cont, y = n_huelgas, label = CAENES_1d)) +
#   geom_point(aes(color = t)) +
#   geom_smooth(method="lm", se = F)+
#   labs(title = "Relación entre tasa de cobertura de contratos colectivos y número de huelgas",
#        subtitle = "2011-2019",
#        x = "N. de sindicatos c/mil trabajadores",
#        y = "Número de huelgas",
#        color = "Act. económica") +
#   geom_text(vjust = 1.5) +
#   stat_cor(method="pearson",
#            label.x.npc = 0.78, label.y.npc = 0.87) +
#   theme_bw() +
#   theme(legend.position="bottom")
# 
# ggsave("output/img/graficos_ene_dtrraa/cobertura_nhuelgas_1119.png", last_plot(),
#        width = 7, height = 5)

data %>% 
  filter(t == "2020-2023") %>% 
  ggplot(aes(x = por_cobertura_cont, y = n_huelgas, label = CAENES_1d)) +
  geom_point() +
  geom_smooth(method="lm", se = F)+
  labs(title = "Relación entre tasa de cobertura de contratos colectivos y número de huelgas",
       subtitle = "202-2023",
       x = "Tasa de cobertura de negociación colectiva",
       y = "Número de huelgas",
       color = "Act. económica") +
  geom_text(vjust = 1.5) +
  stat_cor(method="pearson",
           label.x.npc = 0.78, label.y.npc = 0.95, aes(label = ..r.label..)) +
  theme_bw() +
  theme(legend.position="bottom")

ggsave("output/img/graficos_ene_dtrraa/cobertura_nhuelgas_2023.png", last_plot(),
       width = 7, height = 5)


data %>%
  filter(!t == "2010-2015") %>% 
  ggplot(aes(x = por_cobertura_cont, y = n_huelgas, label = CAENES_1d)) +
  geom_point(aes(color = t)) +
  geom_smooth(method="lm", se = F)+
  labs(title = "Relación entre tasa de cobertura de contratos colectivos y número de huelgas",
       subtitle = "2016-2023",
       x = "Tasa de cobertura de negociación colectiva",
       y = "Número de huelgas",
       color = "Período") +
  geom_text_repel(vjust = 1.5) +
  stat_cor(method="pearson",
           label.x.npc = 0.78, label.y.npc = 0.87, aes(label = ..r.label..)) +
  theme_bw() +
  theme(legend.position="bottom")


ggsave("output/img/graficos_ene_dtrraa/cobertura_nhuelgas_1623.png", last_plot(),
       width = 9, height = 7)


