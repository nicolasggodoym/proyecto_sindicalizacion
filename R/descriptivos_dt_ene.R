rm(list = ls())

# Descriptivos datos finales ENE DT ---------------------------------------

# Cargar paquetes ---------------------------------------------------------
pacman::p_load(tidyverse, readxl, writexl, sjmisc, RColorBrewer, sjPlot)

# Cargar datos ------------------------------------------------------------

llave = read_xlsx("input/data/dt/CAE_DT_armonizado.xlsx") %>% 
  select(ID, CAENES_1d) %>% unique
data = readRDS("output/data/data_dt_ene_final.rds")

data = merge(data, 
             llave,
             by = "ID",
             all.x = T) %>% 
  group_by(ano, ID) %>% 
  mutate(n=1:n()) %>% 
  filter(n==1) %>% 
  ungroup() %>% 
  select(CAENES_1d, ID, everything(), -n) %>% 
  mutate(across(c(CAENES_1d, ID), ~factor(.)))


# Tabla descriptivos ------------------------------------------------------

a=data %>% 
  group_by(ID) %>% 
  summarise(across(total:lag_por_cobertura_otro,
                   .fns = list(mean = mean,
                               n0 = ~sum(. == 0),
                               min = min,
                               q1 = ~quantile(., .25),
                               q2 = ~quantile(., .5),
                               q3 = ~quantile(., .75),
                               max = max))) %>% 
  merge(., llave, by = "ID", all.x = T) %>% 
  select(CAENES_1d, ID, everything()) %>% 
  arrange(CAENES_1d, ID)

write_xlsx(a, "output/img/descriptivos_DT_ENE/descriptivos_ID_seccion.xlsx")

write_xlsx(data %>% 
             group_by(ID) %>% 
             summarise(across(c(n_huelgas,
                                n_trab_huelga_tot,
                                huelgas_dur,
                                tasa_afiliacion,
                                por_cobertura,
                                por_cobertura_cont,
                                n_dptp,
                                mean_dptp),
                              .fns = list(mean = mean,
                                          n0 = ~sum(. == 0),
                                          min = min,
                                          q1 = ~quantile(., .25),
                                          q2 = ~quantile(., .5),
                                          q3 = ~quantile(., .75),
                                          max = max))) %>% 
             merge(., llave, by = "ID", all.x = T) %>% 
             select(CAENES_1d, ID, everything()) %>% 
             arrange(CAENES_1d, ID),
           "output/img/descriptivos_DT_ENE/descriptivos_resumido_ID_seccion.xlsx")

list=split(data, data$CAENES_1d)

#Número de huelgas
imap(list,
     ~saveRDS(.x %>% 
      ggplot(aes(x = ano, y = n_huelgas, color = ID)) + 
       geom_line() +
        labs(title = "Número de huelgas por año",
             subtitle = .y,
             x = "Año", y = "") +
        theme_bw(),
     paste0("output/img/descriptivos_DT_ENE/n_huelgas", .y, ".rds"))
       )

#Número de trabajadores en huelgas
imap(list,
     ~saveRDS(.x %>% 
                ggplot(aes(x = ano, y = n_trab_huelga_tot, color = ID)) + 
                geom_line() +
                labs(title = "Número de trabajadores en huelga por año",
                     subtitle = .y,
                     x = "Año", y = "") +
                theme_bw(),
              paste0("output/img/descriptivos_DT_ENE/n_trab_huelga_tot", .y, ".rds"))
)

#Duración promedio huelgas
imap(list,
     ~saveRDS(.x %>% 
                ggplot(aes(x = ano, y = huelgas_dur, color = ID)) + 
                geom_line() +
                labs(title = "Duración promedio huelgas por año",
                     subtitle = .y,
                     x = "Año", y = "") +
                theme_bw(),
              paste0("output/img/descriptivos_DT_ENE/huelgas_dur", .y, ".rds"))
)

#Tasa de afiliación sindical
imap(list,
     ~saveRDS(.x %>% 
                ggplot(aes(x = ano, y = tasa_afiliacion, color = ID)) + 
                geom_line() +
                labs(title = "Tasa de afiliación sindical por año",
                     subtitle = .y,
                     x = "Año", y = "") +
                theme_bw(),
              paste0("output/img/descriptivos_DT_ENE/tasa_afiliacion", .y, ".rds"))
)

#Cobertura neg. colectiva (total)
imap(list,
     ~saveRDS(.x %>% 
                ggplot(aes(x = ano, y = por_cobertura, color = ID)) + 
                geom_line() +
                labs(title = "% Cobertura neg. colectiva (total) por año",
                     subtitle = .y,
                     x = "Año", y = "") +
                theme_bw(),
              paste0("output/img/descriptivos_DT_ENE/por_cobertura", .y, ".rds"))
)

#Cobertura neg. colectiva (contratos)
imap(list,
     ~saveRDS(.x %>% 
                ggplot(aes(x = ano, y = por_cobertura_cont, color = ID)) + 
                geom_line() +
                labs(title = "% Cobertura neg. colectiva (contratos) por año",
                     subtitle = .y,
                     x = "Año", y = "") +
                theme_bw(),
              paste0("output/img/descriptivos_DT_ENE/por_cobertura_cont", .y, ".rds"))
)

# Días-persona de trabajo perdidos
imap(list,
     ~saveRDS(.x %>% 
                ggplot(aes(x = ano, y = n_dptp, color = ID)) + 
                geom_line() +
                labs(title = "Días-persona de trabajo perdidos (n) por año",
                     subtitle = .y,
                     x = "Año", y = "") +
                theme_bw(),
              paste0("output/img/descriptivos_DT_ENE/n_dptp", .y, ".rds"))
)

imap(list,
     ~saveRDS(.x %>% 
                ggplot(aes(x = ano, y = mean_dptp, color = ID)) + 
                geom_line() +
                labs(title = "Días-persona de trabajo perdidos (media) por año",
                     subtitle = .y,
                     x = "Año", y = "") +
                theme_bw(),
              paste0("output/img/descriptivos_DT_ENE/mean_dptp", .y, ".rds"))
)
































