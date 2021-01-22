### Configuración inicial ----
pacman::p_load(tidyverse, scales,ggtext,
               zoo) # rolling averages

## Setup
Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Mac
Sys.setlocale("LC_ALL", "Spanish_Mexico.1252") # Windows
options(scipen=999) 

## Eliminar objetos
rm(list = ls())

data <- read_csv("1_data/datos_abiertos_covid19/210121COVID19MEXICO.csv")


### Adecuación de datos----

## Base que suma los casos diarios por entidad:
# El truco para que el geom_area salga como en el Financial Times y no como comúnmente sería es que hasta abajo tiene que haber una observación que es equivalente a: el número máximo de fallecimientos en un día (>800) menos la suma de los fallecimientos en un día particular, entre dos. Hay que ordenar esta observación (que sería como una región) para que esté hasta abajo del geom_area y ponerle color transparente.

a = data %>% 
  filter(CLASIFICACION_FINAL %in% c(1:3),
         FECHA_DEF != is.na(FECHA_DEF)) %>% 
  rename(entidad_nac = ENTIDAD_RES) %>% 
  mutate(entidad_nac = case_when(entidad_nac == "01" ~ "AGUASCALIENTES",
                                 entidad_nac == "02" ~ "BAJA CALIFORNIA",
                                 entidad_nac == "03" ~ "BAJA CALIFORNIA SUR",
                                 entidad_nac == "04" ~ "CAMPECHE",
                                 entidad_nac == "05" ~ "COAHUILA",
                                 entidad_nac == "06" ~ "COLIMA",
                                 entidad_nac == "07" ~ "CHIAPAS",
                                 entidad_nac == "08" ~ "CHIHUAHUA",
                                 entidad_nac == "09" ~ "CIUDAD DE MÉXICO",
                                 entidad_nac == "10" ~ "DURANGO",
                                 entidad_nac == "11" ~ "GUANAJUATO",
                                 entidad_nac == "12" ~ "GUERRERO",
                                 entidad_nac == "13" ~ "HIDALGO",
                                 entidad_nac == "14" ~ "JALISCO",
                                 entidad_nac == "15" ~ "MÉXICO",
                                 entidad_nac == "16" ~ "MICHOACÁN",
                                 entidad_nac == "17" ~ "MORELOS",
                                 entidad_nac == "18" ~ "NAYARIT",
                                 entidad_nac == "19" ~ "NUEVO LEÓN",
                                 entidad_nac == "20" ~ "OAXACA",
                                 entidad_nac == "21" ~ "PUEBLA",
                                 entidad_nac == "22" ~ "QUERÉTARO",
                                 entidad_nac == "23" ~ "QUINTANA ROO",
                                 entidad_nac == "24" ~ "SAN LUIS POTOSÍ",
                                 entidad_nac == "25" ~ "SINALOA",
                                 entidad_nac == "26" ~ "SONORA",
                                 entidad_nac == "27" ~ "TABASCO",
                                 entidad_nac == "28" ~ "TAMAULIPAS",
                                 entidad_nac == "29" ~ "TLAXCALA",
                                 entidad_nac == "30" ~ "VERACRUZ",
                                 entidad_nac == "31" ~ "YUCATÁN",
                                 entidad_nac == "32" ~ "ZACATECAS")) %>% 
  mutate(entidad_nac = str_to_title(entidad_nac)) %>% 
  mutate(entidad_nac = str_replace_all(entidad_nac, c("Ciudad De México" = "CDMX",
                                                      "México" = "Edomex"))) %>% 
  count(entidad_nac, FECHA_DEF) %>% 
  arrange(FECHA_DEF, entidad_nac) %>% 
  pivot_wider(values_from = n,
              names_from = FECHA_DEF) %>%
  
  mutate_if(is.integer, ~replace(., is.na(.), 0)) %>%  pivot_longer(-entidad_nac,
                                                                    names_to = "FECHA_DEF",
                                                                    values_to = "n") %>% 
  mutate(FECHA_DEF = lubridate::ymd(FECHA_DEF)) %>% 
  filter(FECHA_DEF >= as.Date("2020-03-22")) %>% 
  group_by(entidad_nac) %>% 
  mutate(rollmean = rollmean(n, k = 7, fill = NA)) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  ungroup() %>% 
  group_by(FECHA_DEF) %>% 
  summarise(rollmean = sum(rollmean)) %>% 
  ungroup() %>% 
  mutate(entidad_nac = "suma") %>% 
  select(entidad_nac, FECHA_DEF, rollmean) %>% 
  mutate(rollmean = (810 - rollmean)/2)


### Visualización de totales----
data %>% 
  filter(CLASIFICACION_FINAL %in% c(1:3),
         FECHA_DEF != is.na(FECHA_DEF)) %>% 
  rename(entidad_nac = ENTIDAD_RES) %>% 
  mutate(entidad_nac = case_when(entidad_nac == "01" ~ "AGUASCALIENTES",
                          entidad_nac == "02" ~ "BAJA CALIFORNIA",
                          entidad_nac == "03" ~ "BAJA CALIFORNIA SUR",
                          entidad_nac == "04" ~ "CAMPECHE",
                          entidad_nac == "05" ~ "COAHUILA",
                          entidad_nac == "06" ~ "COLIMA",
                          entidad_nac == "07" ~ "CHIAPAS",
                          entidad_nac == "08" ~ "CHIHUAHUA",
                          entidad_nac == "09" ~ "CIUDAD DE MÉXICO",
                          entidad_nac == "10" ~ "DURANGO",
                          entidad_nac == "11" ~ "GUANAJUATO",
                          entidad_nac == "12" ~ "GUERRERO",
                          entidad_nac == "13" ~ "HIDALGO",
                          entidad_nac == "14" ~ "JALISCO",
                          entidad_nac == "15" ~ "MÉXICO",
                          entidad_nac == "16" ~ "MICHOACÁN",
                          entidad_nac == "17" ~ "MORELOS",
                          entidad_nac == "18" ~ "NAYARIT",
                          entidad_nac == "19" ~ "NUEVO LEÓN",
                          entidad_nac == "20" ~ "OAXACA",
                          entidad_nac == "21" ~ "PUEBLA",
                          entidad_nac == "22" ~ "QUERÉTARO",
                          entidad_nac == "23" ~ "QUINTANA ROO",
                          entidad_nac == "24" ~ "SAN LUIS POTOSÍ",
                          entidad_nac == "25" ~ "SINALOA",
                          entidad_nac == "26" ~ "SONORA",
                          entidad_nac == "27" ~ "TABASCO",
                          entidad_nac == "28" ~ "TAMAULIPAS",
                          entidad_nac == "29" ~ "TLAXCALA",
                          entidad_nac == "30" ~ "VERACRUZ",
                          entidad_nac == "31" ~ "YUCATÁN",
                          entidad_nac == "32" ~ "ZACATECAS")) %>% 
  mutate(entidad_nac = str_to_title(entidad_nac)) %>% 
  mutate(entidad_nac = str_replace_all(entidad_nac, c("Ciudad De México" = "CDMX",
                                                      "México" = "Edomex"))) %>% 
  # filter(entidad_nac != "CDMX",
  #        entidad_nac != "Edomex") %>% 
  # count(entidad_nac, FECHA_DEF) %>% arrange(-n) %>% print(n = 400)
  mutate(entidad_nac = str_replace_all(entidad_nac, 
                                   c("Aguascalientes" = "Centro-Norte",
                                       "Baja California Sur" = "Noroeste",
                                       "Baja California" = "Noroeste",
                                       "Campeche" = "Sureste",
                                       "Chiapas" = "Suroeste",
                                       # "Chihuahua" = "Chihuahua",
                                       # "Ciudad de México" = "CDMX",
                                       "Coahuila" = "Noreste",
                                       "Colima" = "Occidente",
                                       "Durango"= "Noroeste",
                                       # "Guanajuato" = "Guanajuato",
                                       "Guerrero" = "Suroeste",
                                       "Hidalgo" = "Centro-Este",
                                       # "Jalisco" = "Occidente",
                                       # "Edomex" = "Edomex",
                                       "Michoacán" = "Occidente",
                                       "Morelos" = "Centro-Este",
                                       "Nayarit" = "Occidente",
                                       # "Nuevo León" = "NL",
                                       "Oaxaca" = "Suroeste",
                                       # "Puebla" = "Pue",
                                       "Querétaro" = "Centro-Norte",
                                       "Quintana Roo" = "Sureste",
                                       "San Luis Potosí" = "Centro-Norte",
                                       "Sinaloa" = "Noroeste",
                                       "Sonora" = "Noroeste",
                                       "Tabasco" = "Sureste",
                                       "Tamaulipas" = "Noreste",
                                       "Tlaxcala" = "Centro-Este",
                                       # "Veracruz" = "VER",
                                       "Yucatán" = "Sureste",
                                       "Zacatecas" = "Centro-Norte"))) %>% 
  count(entidad_nac, FECHA_DEF) %>% 
  arrange(FECHA_DEF, entidad_nac) %>% 
  pivot_wider(values_from = n,
              names_from = FECHA_DEF) %>%
  
  mutate_if(is.integer, ~replace(., is.na(.), 0)) %>%  pivot_longer(-entidad_nac,
               names_to = "FECHA_DEF",
               values_to = "n") %>% 
  mutate(FECHA_DEF = lubridate::ymd(FECHA_DEF)) %>% 
  filter(FECHA_DEF >= as.Date("2020-03-22")) %>% 
  group_by(entidad_nac) %>% 
  mutate(rollmean = rollmean(n, k = 7, fill = NA)) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  ungroup() %>% 
  group_by(FECHA_DEF) %>% 
  mutate(suma = sum(rollmean)) %>% 
  ungroup() %>% 
  select(entidad_nac, FECHA_DEF, rollmean) %>% 
  rbind(a) %>% 
  mutate(entidad_nac = factor(entidad_nac,
                              order = T,
                              levels = c("Chihuahua","Noroeste",
                                         "Nuevo León","Noreste",
                                          "Guanajuato","Centro-Norte",
                                         "Jalisco", "Occidente", 
                                         "CDMX", "Edomex",
                                         "Puebla","Veracruz","Centro-Este",
                                         "Suroeste", "Sureste",
                                         "suma"))) %>% 
  ggplot() + 
  geom_area(aes(x = FECHA_DEF,
                   y = rollmean,
                   group = entidad_nac,
                   fill = entidad_nac),
               show.legend = T, color = "transparent",
               position = "stack") +
  geom_vline(xintercept = as.Date("2021-01-07"),
             color = "grey0", size = 1, linetype = 2) +
  scale_fill_manual(values = c("coral3","coral",
                               "dodgerblue4","dodgerblue1",
                               "plum3","plum1",
                               "springgreen3", "springgreen1",
                               "goldenrod3", "goldenrod1", 
                               
                               "snow4","snow3","snow2",
                               "brown", "brown1",
                               "transparent"),
                    labels = c("**Chihuahua**","Resto del Noroeste<br>(BC, BCS, DUR, SIN, SON)",
                               "**Nuevo León**","Resto del Noreste<br>(COA, TAM)",
                               "**Guanajuato**","Resto del Centro-Norte<br>(AGS, QUE, SLP, ZAC)",
                               "**Jalisco**", "Resto del Occidente<br>(COL, MIC, NAY)", 
                               "**CDMX**", "**Edomex**",
                               "**Puebla**","**Veracruz**","Resto del Centro-Este<br>(HID, MOR, TLA)",
                               "Suroeste", "Sureste",
                               "")) +
  labs(x = "\nFecha de defunción\n",
       y = "",
       fill = "",
       title = "Defunciones diarias por COVID-19 en las regiones de México",
       subtitle = "Promedio móvil de 7 días por fecha de defunción (no de registro). Corte: 21-ene-21\n",
       caption = "Nota: las defunciones podrían ser mayores tanto por la subestimación reconocida por la autoridad sanitaria<br>como por el retraso del registro (sobre todo en las semanas más recientes).<br><br>Elaborado por Pablo de los Cobos (**@pCobosAlcala**) con datos de la Secretaría de Salud.<br>Réplica de las gráficas realizadas por Steven Bernard (@sdbernard) para el Financial Times") + 
  
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_minimal() +
  theme(title = element_text(size = 14),
        legend.text = element_markdown(size = 9.75),
        plot.caption = element_markdown(hjust = 0),
        axis.text.x = element_text(angle = 0, hjust = .5),
        panel.grid.major.x = element_line(size = 1.2))



ggsave("3_viz/ft.png",
       width = 8.5,
       height = 6,
       dpi = 600)



### Visualización de porcentajes----
data %>% 
  filter(CLASIFICACION_FINAL %in% c(1:3),
         FECHA_DEF != is.na(FECHA_DEF)) %>% 
  rename(entidad_nac = ENTIDAD_RES) %>% 
  mutate(entidad_nac = case_when(entidad_nac == "01" ~ "AGUASCALIENTES",
                                 entidad_nac == "02" ~ "BAJA CALIFORNIA",
                                 entidad_nac == "03" ~ "BAJA CALIFORNIA SUR",
                                 entidad_nac == "04" ~ "CAMPECHE",
                                 entidad_nac == "05" ~ "COAHUILA",
                                 entidad_nac == "06" ~ "COLIMA",
                                 entidad_nac == "07" ~ "CHIAPAS",
                                 entidad_nac == "08" ~ "CHIHUAHUA",
                                 entidad_nac == "09" ~ "CIUDAD DE MÉXICO",
                                 entidad_nac == "10" ~ "DURANGO",
                                 entidad_nac == "11" ~ "GUANAJUATO",
                                 entidad_nac == "12" ~ "GUERRERO",
                                 entidad_nac == "13" ~ "HIDALGO",
                                 entidad_nac == "14" ~ "JALISCO",
                                 entidad_nac == "15" ~ "MÉXICO",
                                 entidad_nac == "16" ~ "MICHOACÁN",
                                 entidad_nac == "17" ~ "MORELOS",
                                 entidad_nac == "18" ~ "NAYARIT",
                                 entidad_nac == "19" ~ "NUEVO LEÓN",
                                 entidad_nac == "20" ~ "OAXACA",
                                 entidad_nac == "21" ~ "PUEBLA",
                                 entidad_nac == "22" ~ "QUERÉTARO",
                                 entidad_nac == "23" ~ "QUINTANA ROO",
                                 entidad_nac == "24" ~ "SAN LUIS POTOSÍ",
                                 entidad_nac == "25" ~ "SINALOA",
                                 entidad_nac == "26" ~ "SONORA",
                                 entidad_nac == "27" ~ "TABASCO",
                                 entidad_nac == "28" ~ "TAMAULIPAS",
                                 entidad_nac == "29" ~ "TLAXCALA",
                                 entidad_nac == "30" ~ "VERACRUZ",
                                 entidad_nac == "31" ~ "YUCATÁN",
                                 entidad_nac == "32" ~ "ZACATECAS")) %>% 
  mutate(entidad_nac = str_to_title(entidad_nac)) %>% 
  mutate(entidad_nac = str_replace_all(entidad_nac, c("Ciudad De México" = "CDMX",
                                                      "México" = "Edomex"))) %>% 
  mutate(entidad_nac = str_replace_all(entidad_nac, 
                                       c("Aguascalientes" = "Centro-Norte",
                                         "Baja California Sur" = "Noroeste",
                                         "Baja California" = "Noroeste",
                                         "Campeche" = "Sureste",
                                         "Chiapas" = "Suroeste",
                                         "Chihuahua" = "Chihuahua",
                                         # "Ciudad de México" = "CDMX",
                                         "Coahuila" = "Noreste",
                                         "Colima" = "Occidente",
                                         "Durango"= "Noroeste",
                                         # "Guanajuato" = "Guanajuato",
                                         "Guerrero" = "Suroeste",
                                         "Hidalgo" = "Centro-Este",
                                         # "Jalisco" = "Occidente",
                                         # "Edomex" = "Edomex",
                                         "Michoacán" = "Occidente",
                                         "Morelos" = "Centro-Este",
                                         "Nayarit" = "Occidente",
                                         # "Nuevo León" = "NL",
                                         "Oaxaca" = "Suroeste",
                                         # "Puebla" = "Pue",
                                         "Querétaro" = "Centro-Norte",
                                         "Quintana Roo" = "Sureste",
                                         "San Luis Potosí" = "Centro-Norte",
                                         "Sinaloa" = "Noroeste",
                                         "Sonora" = "Noroeste",
                                         "Tabasco" = "Sureste",
                                         "Tamaulipas" = "Noreste",
                                         "Tlaxcala" = "Centro-Este",
                                         # "Veracruz" = "VER",
                                         "Yucatán" = "Sureste",
                                         "Zacatecas" = "Centro-Norte"))) %>% 
  count(entidad_nac, FECHA_DEF) %>% 
  arrange(FECHA_DEF, entidad_nac) %>% 
  pivot_wider(values_from = n,
              names_from = FECHA_DEF) %>%
  
  mutate_if(is.integer, ~replace(., is.na(.), 0)) %>%  pivot_longer(-entidad_nac,
                                                                    names_to = "FECHA_DEF",
                                                                    values_to = "n") %>% 
  mutate(FECHA_DEF = lubridate::ymd(FECHA_DEF)) %>% 
  filter(FECHA_DEF >= as.Date("2020-03-22")) %>% 
  group_by(entidad_nac) %>% 
  mutate(rollmean = rollmean(n, k = 7, fill = NA)) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  ungroup() %>% 
  group_by(FECHA_DEF) %>% 
  mutate(suma = sum(rollmean)) %>% 
  ungroup() %>% 
  select(entidad_nac, FECHA_DEF, rollmean) %>% 
  mutate(entidad_nac = factor(entidad_nac,
                              order = T,
                              levels = c("Chihuahua","Noroeste",
                                         "Nuevo León","Noreste",
                                         "Guanajuato","Centro-Norte",
                                         "Jalisco", "Occidente", 
                                         "CDMX", "Edomex",
                                         "Puebla","Veracruz","Centro-Este",
                                         "Suroeste", "Sureste"))) %>% 
  ggplot() + 
  geom_col(aes(x = FECHA_DEF,
                y = rollmean,
                group = entidad_nac,
                fill = entidad_nac),
            show.legend = T, color = "transparent",
            position = "fill") +
  geom_vline(xintercept = as.Date("2021-01-07"),
             color = "grey0", size = 1, linetype = 2) +
  scale_fill_manual(values = c("coral3","coral",
                               "dodgerblue4","dodgerblue1",
                               "plum3","plum1",
                               "springgreen3", "springgreen1",
                               "goldenrod3", "goldenrod1", 
                               
                               "snow4","snow3","snow2",
                               "brown", "brown1"),
                    labels = c("**Chihuahua**","Resto del Noroeste<br>(BC, BCS, DUR, SIN, SON)",
                               "**Nuevo León**","Resto del Noreste<br>(COA, TAM)",
                               "**Guanajuato**","Resto del Centro-Norte<br>(AGS, QUE, SLP, ZAC)",
                               "**Jalisco**", "Resto del Occidente<br>(COL, MIC, NAY)", 
                               "**CDMX**", "**Edomex**",
                               "**Puebla**","**Veracruz**","Resto del Centro-Este<br>(HID, MOR, TLA)",
                               "Suroeste", "Sureste")) +
  scale_y_continuous(label = percent_format()) + 
  labs(x = "\nFecha de defunción\n",
       y = "",
       fill = "",
       title = "Región de las defunciones diarias por COVID-19 en México",
       subtitle = "Promedio móvil de 7 días por fecha de defunción (no de registro). Corte: 21-ene-21\n",
       caption = "Nota: las defunciones podrían ser mayores tanto por la subestimación reconocida por la autoridad sanitaria<br>como por el retraso del registro (sobre todo en las semanas más recientes).<br><br>Elaborado por Pablo de los Cobos (**@pCobosAlcala**) con datos de la Secretaría de Salud.<br>Réplica de las gráficas realizadas por Steven Bernard (@sdbernard) para el Financial Times") + 
  
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_minimal() +
  theme(title = element_text(size = 14),
        legend.text = element_markdown(size = 9.75),
        plot.caption = element_markdown(hjust = 0),
        axis.text.x = element_text(angle = 0, hjust = .5),
        panel.grid.major.x = element_line(size = 1.2))



ggsave("3_viz/ft2.png",
       width = 8.5,
       height = 6,
       dpi = 600)




# noroeste: bc, bcs, sin, son, dur,chh*
# noreste: coah, NL*, TAM
# centro norte: zac, slp, ags, gto*, que
# occidente: jalisco*, nayarit, col, mich
# valle de mx**
# este: hidalgo, tlax, pue*, ver*, mor
# suroeste: gue, oax, chia
# sureste: tab, qroo, yuc, camp










