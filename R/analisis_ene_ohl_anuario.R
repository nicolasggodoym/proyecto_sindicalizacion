rm(list=ls())

# Gráficos de dispersión ENE-OHL-AnuarioDT --------------------------------


# Cargar paquetes ---------------------------------------------------------
pacman::p_load(tidyverse, RColorBrewer, patchwork, ggplot2, ggpubr)

# Cargar data -------------------------------------------------------------

data = readRDS("output/data/ene_ohl_anuario.rds") %>% 
  group_by(t, CAENES_1d) %>% 
  summarise(across(c(tasa_afiliacion, n_huelga, n_sindicatos),
                   ~mean(.))) %>% 
  ungroup()


# Gráficos ----------------------------------------------------------------


# Afiliación sindical x Número de huelgas ---------------------------------

data %>% 
  filter(t == "2011-2015") %>% 
  ggplot(aes(x = tasa_afiliacion, y = n_huelga, label = CAENES_1d)) +
  geom_point() +
  geom_smooth(method="lm", se = F)+
  labs(title = "Relación entre afiliación sindical y número de huelgas",
       subtitle = "2011-2015",
       x = "Tasa de afiliación sindical",
       y = "Número de huelgas",
       color = "Act. económica") +
  geom_text(vjust = 1.5) +
  stat_cor(method="pearson",
           label.x.npc = 0.78, label.y.npc = 0.95, aes(label = ..r.label..)) +
  theme_bw() +
  theme(legend.position="bottom")

ggsave("output/img/graficos_ene_ohl_anuariodt/densidad_nhuelgas_1115.png", last_plot(),
       width = 7, height = 5)

data %>% 
  filter(t == "2016-2019") %>% 
  ggplot(aes(x = tasa_afiliacion, y = n_huelga, label = CAENES_1d)) +
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

ggsave("output/img/graficos_ene_ohl_anuariodt/densidad_nhuelgas_1619.png", last_plot(),
       width = 7, height = 5)


data %>% 
  filter(!t %in% "2020-2023") %>% 
  ggplot(aes(x = tasa_afiliacion, y = n_huelga, label = CAENES_1d)) +
  geom_point(aes(color = t)) +
  geom_smooth(method="lm", se = F)+
  labs(title = "Relación entre afiliación sindical y número de huelgas",
       subtitle = "2011-2019",
       x = "Tasa de afiliación sindical",
       y = "Número de huelgas",
       color = "Act. económica") +
  geom_text(vjust = 1.5) +
  stat_cor(method="pearson",
           label.x.npc = 0.78, label.y.npc = 0.95, aes(label = ..r.label..)) +
  theme_bw() +
  theme(legend.position="bottom")
  
ggsave("output/img/graficos_ene_ohl_anuariodt/densidad_nhuelgas_1119.png", last_plot(),
       width = 9, height = 7)

# N sindicatos x N de huelgas ---------------------------------------------

data %>% 
  filter(t == "2011-2015") %>% 
  ggplot(aes(x = n_sindicatos, y = n_huelga, label = CAENES_1d)) +
  geom_point() +
  geom_smooth(method="lm", se = F)+
  labs(title = "Relación entre fragmentación sindical y número de huelgas",
       subtitle = "2011-2015",
       x = "N. de sindicatos c/mil trabajadores",
       y = "Número de huelgas",
       color = "Act. económica") +
  geom_text(vjust = 1.5) +
  stat_cor(method="pearson",
           label.x.npc = 0.78, label.y.npc = 0.90, aes(label = ..r.label..)) +
  theme_bw() +
  theme(legend.position="bottom")

ggsave("output/img/graficos_ene_ohl_anuariodt/fragmentacion_nhuelgas_1115.png", last_plot(),
       width = 7, height = 5)

data %>% 
  filter(t == "2016-2019") %>% 
  ggplot(aes(x = n_sindicatos, y = n_huelga, label = CAENES_1d)) +
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

ggsave("output/img/graficos_ene_ohl_anuariodt/fragmentacion_nhuelgas_1519.png", last_plot(),
       width = 7, height = 5)

data %>% 
  filter(!t %in% "2020-2023") %>% 
  ggplot(aes(x = n_sindicatos, y = n_huelga, label = CAENES_1d)) +
  geom_point(aes(color = t)) +
  geom_smooth(method="lm", se = F)+
  labs(title = "Relación entre fragmentación sindical y número de huelgas",
       subtitle = "2011-2019",
       x = "N. de sindicatos c/mil trabajadores",
       y = "Número de huelgas",
       color = "Act. económica") +
  geom_text(vjust = 1.5) +
  stat_cor(method="pearson",
           label.x.npc = 0.78, label.y.npc = 0.87, aes(label = ..r.label..)) +
  theme_bw() +
  theme(legend.position="bottom")

ggsave("output/img/graficos_ene_ohl_anuariodt/fragmentacion_nhuelgas_1119.png", last_plot(),
       width = 7, height = 5)
