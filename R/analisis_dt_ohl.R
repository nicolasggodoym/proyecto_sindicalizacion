rm(list = ls())

# Gráficos análisis datos DT-OHL ------------------------------------------

# Cargar paquetes ---------------------------------------------------------
pacman::p_load(tidyverse,
               ggplot2,
               sjmisc, 
               readxl,
               ggrepel,
               patchwork)
# Cargar datos ------------------------------------------------------------
dt = read_xlsx("input/data/anuario_dt.xlsx",
               sheet = "resumen",
               range = "B1:J341") %>% 
  mutate(actividad_raw = factor(actividad_raw))

ohl = readRDS("output/data/ohl.rds")

# Gráficos ----------------------------------------------------------------
#Son todos por rama

# Promedio de huelgas ----------------------------------
lista = list()

point = c(9:19)

for (i in 1:9) {
  a = ohl %>% 
    filter(ano %in% c(1999:2019) & !is.na(actividad_raw)) %>% 
    count(ano, actividad_raw) %>% 
    filter(actividad_raw == levels(ohl$actividad_raw)[i]) %>% 
    ggplot(aes(x = ano, y = n)) +
    geom_line() +
    geom_point(shape = point[i], size = 2, color = "black") +
    labs(subtitle = str_wrap(levels(ohl$actividad_raw)[i], width = 32),
         caption = "",
         x = "Año",
         y = "") +
    #geom_text_repel(aes(label = paste(n, "%")), colour = "black") +
    scale_x_continuous(limits = c(1999, 2019), n.breaks = 20) +
    scale_y_continuous(limits = c(0,100), n.breaks = 5) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90)) 
  lista[[i]] = a
}

wrap_plots(lista,
           ncol = 3, 
           nrow = 3)

ggsave("output/img/n_huelgas_act.jpg")

# Promedio de Días-personas de trabajo perdido en huelgas -----------------
lista = list()

point = c(9:19)

for (i in 1:9) {
  a = ohl %>% 
    filter(ano %in% c(1999:2019)) %>% 
    group_by(ano, actividad_raw) %>% 
    summarise(n = round(mean(dptp, na.rm = T), 2)) %>%
    filter(actividad_raw == levels(ohl$actividad_raw)[i]) %>% 
    ggplot(aes(x = ano, y = n)) +
    geom_line() +
    geom_point(shape = point[i], size = 2, color = "black") +
    labs(subtitle = str_wrap(levels(ohl$actividad_raw)[i], width = 32),
         caption = "",
         x = "Año",
         y = "") +
    #geom_text_repel(aes(label = paste(n, "%")), colour = "black") +
    scale_x_continuous(limits = c(1999, 2019), n.breaks = 20) +
    scale_y_continuous(limits = c(0,15000), n.breaks = 5) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90)) 
  lista[[i]] = a
}

wrap_plots(lista,
           ncol = 3, 
           nrow = 3)

ggsave("output/img/dptp_act(1).jpg")

# Promedio de trabajadores involucrados en huelgas ------------------------
lista = list()

point = c(9:19)

for (i in 1:9) {
  a = ohl %>% 
    filter(ano %in% c(1999:2019)) %>% 
    group_by(ano, actividad_raw) %>% 
    summarise(n = round(mean(tc, na.rm = T), 2)) %>%
    filter(actividad_raw == levels(ohl$actividad_raw)[i]) %>% 
    ggplot(aes(x = ano, y = n)) +
    geom_line() +
    geom_point(shape = point[i], size = 2, color = "black") +
    labs(subtitle = str_wrap(levels(ohl$actividad_raw)[i], width = 32),
         caption = "",
         x = "Año",
         y = "") +
    #geom_text_repel(aes(label = paste(n, "%")), colour = "black") +
    scale_x_continuous(limits = c(1999, 2019), n.breaks = 20) +
    scale_y_continuous(limits = c(0,3000), n.breaks = 5) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90)) 
  lista[[i]] = a
}

wrap_plots(lista,
           ncol = 3, 
           nrow = 3)

ggsave("output/img/trab_act.jpg")

# Tasa de afiliación sindical  ------------------------------------

lista = list()

point = c(9:19)

for (i in 1:9) {
  a = dt %>% 
    filter(ano %in% c(1999:2019)) %>% 
    group_by(ano, actividad_raw) %>% 
    summarise(n = round(mean(tasa_sind_per, na.rm = T), 2)) %>%
    filter(actividad_raw == levels(dt$actividad_raw)[i]) %>% 
    ggplot(aes(x = ano, y = n)) +
    geom_line() +
    geom_point(shape = point[i], size = 2, color = "black") +
    labs(subtitle = str_wrap(levels(dt$actividad_raw)[i], width = 32),
         caption = "",
         x = "Año",
         y = "") +
    #geom_text_repel(aes(label = paste(n, "%")), colour = "black") +
    scale_x_continuous(limits = c(1999, 2019), n.breaks = 20) +
    scale_y_continuous(limits = c(0,100), n.breaks = 5) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90)) 
  lista[[i]] = a
}

wrap_plots(lista,
           ncol = 3, 
           nrow = 3)

ggsave("output/img/tasa_sind_act.jpg")

# Tasa de fragmentación sindical por 1000 trabajadores --------------------

lista = list()

point = c(9:19)

for (i in 1:9) {
  a = dt %>% 
    filter(ano %in% c(1999:2019)) %>% 
    group_by(ano, actividad_raw) %>% 
    summarise(n = round(mean(n_sind_mil, na.rm = T), 2)) %>%
    filter(actividad_raw == levels(dt$actividad_raw)[i]) %>% 
    ggplot(aes(x = ano, y = n)) +
    geom_line() +
    geom_point(shape = point[i], size = 2, color = "black") +
    labs(subtitle = str_wrap(levels(dt$actividad_raw)[i], width = 32),
         caption = "",
         x = "Año",
         y = "") +
    #geom_text_repel(aes(label = paste(n, "%")), colour = "black") +
    scale_x_continuous(limits = c(1999, 2019), n.breaks = 20) +
    scale_y_continuous(limits = c(0,5), n.breaks = 5) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90)) 
  lista[[i]] = a
}

wrap_plots(lista,
           ncol = 3, 
           nrow = 3)

ggsave("output/img/n_sind_act.jpg")



