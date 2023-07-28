### Paquetes----
pacman::p_load(survey, tidyverse, scales, ggtext, mxmaps, sf)
windowsFonts(A = windowsFont("Roboto Condensed"))

### Base----
db <- read.csv("1_data/enigh2022_ns_concentradohogar_csv/concentradohogar.csv") %>% 
  as_tibble()

a = db %>% 
  select(upm, est_dis, factor, bene_gob, ubica_geo) %>% 
  mutate(bene = case_when(
    bene_gob > 0 ~ 1,
    T  ~ 0 
  )) %>% 
  mutate(ent = str_sub(ubica_geo, end = -4)) 

design = svydesign(id=~upm,
                   strata=~est_dis,
                   weights=~factor,
                   data=a,
                   nest = T)

cob = svyby(~bene, ~factor(ent), design, svymean, na.rm.all = T) %>%
  as_tibble() %>% 
  janitor::clean_names()

### Mapa----
hex = mxhexbin.map %>% 
  mutate(state_abbr = str_replace_all(state_abbr,
                                      c("CAMP" = "CAM",
                                        "CHIH" = "CHH",
                                        "CHPS" = "CHP",
                                        "COAH" = "COA",
                                        "DGO" = "DUR",
                                        "GTO" = "GUA",
                                        "GRO" = "GUE",
                                        "HGO" = "HID",
                                        "DF" = "CDMX",
                                        "MICH" = "MIC",
                                        "QROO" = "ROO",
                                        "QRO" = "QUE",
                                        "TLAX" = "TLA")))
im2  = cob %>% 
  rename(entidad = factor_ent) %>% 
  mutate(entidad = as.character(entidad)) %>% 
  mutate(entidad = as.numeric(entidad)) %>% 
  mutate(entidad = case_when(entidad == 1 ~ "Aguascalientes",
                             entidad == 2 ~ "Baja California",
                             entidad == 3 ~ "Baja California Sur",
                             entidad == 4 ~ "Campeche",
                             
                             entidad == 7 ~ "Chiapas",
                             entidad == 8 ~ "Chihuahua",
                             entidad == 9 ~ "Ciudad de MC)xico",
                             entidad == 5 ~ "Coahuila",
                             entidad == 6 ~ "Colima" ,
                             
                             entidad == 10 ~ "Durango",
                             entidad == 11 ~ "Guanajuato",
                             entidad == 12 ~ "Guerrero",
                             entidad == 13 ~ "Hidalgo",
                             entidad == 14 ~ "Jalisco",
                             entidad == 15 ~ "MC)xico",
                             entidad == 16 ~ "MichoacC!n",
                             entidad == 17 ~ "Morelos",
                             entidad == 18 ~ "Nayarit",
                             entidad == 19 ~ "Nuevo LeC3n",
                             entidad == 20 ~ "Oaxaca",
                             entidad == 21 ~ "Puebla",
                             entidad == 22 ~ "QuerC)taro",
                             entidad == 23 ~ "Quintana Roo",
                             entidad == 24 ~ "San Luis PotosC-",
                             entidad == 25 ~ "Sinaloa",
                             entidad == 26 ~ "Sonora",
                             entidad == 27 ~ "Tabasco",
                             entidad == 28 ~ "Tamaulipas",
                             entidad == 29 ~ "Tlaxcala",
                             entidad == 30 ~ "Veracruz",
                             entidad == 31 ~ "YucatC!n",
                             entidad == 32 ~ "Zacatecas")) %>% 
  mutate(values = bene) %>% 
  mutate(entidad = case_when(entidad == "Estado de MC)xico" ~ "MC)xico",
                             T ~ entidad)) %>% 
  mutate(short = str_replace_all(entidad,
                                 c("Aguascalientes" = "AGS",
                                   "Baja California Sur" = "BCS",
                                   "Baja California" = "BC",
                                   "Campeche" = "CAM",
                                   "Chiapas" = "CHP",
                                   "Chihuahua" = "CHH",
                                   "Ciudad de MC)xico" = "CDMX",
                                   "Coahuila" = "COA",
                                   "Colima" = "COL",
                                   "Durango"= "DUR",
                                   "Guanajuato" = "GUA",
                                   "Guerrero" = "GUE",
                                   "Hidalgo" = "HID",
                                   "Jalisco" = "JAL",
                                   "MC)xico" = "MEX",
                                   "MichoacC!n" = "MIC",
                                   "Morelos" = "MOR",
                                   "Nayarit" = "NAY",
                                   "Nuevo LeC3n" = "NL",
                                   "Oaxaca" = "OAX",
                                   "Puebla" = "PUE",
                                   "Quintana Roo" = "ROO",
                                   "QuerC)taro" = "QUE",
                                   "San Luis PotosC-" = "SLP",
                                   "Sinaloa" = "SIN",
                                   "Sonora" = "SON",
                                   "Tabasco" = "TAB",
                                   "Tamaulipas" = "TAM",
                                   "Tlaxcala" = "TLA",
                                   "Veracruz" = "VER",
                                   "YucatC!n" = "YUC",
                                   "Zacatecas" = "ZAC")))

state_labels_map = hex %>% 
  group_by(state_abbr) %>% 
  summarise(long = mean(long), lat = mean(lat), group = first(group)) %>%  
  left_join(im2, by = c("state_abbr" = "short")) %>%
  mutate(num = values)  %>% 
  mutate(state_abbr = str_c(state_abbr, percent(num, accuracy = .1), sep = "\n"))

hex %>% 
  left_join(im2, by = c("state_abbr" = "short")) %>% 
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = values), col = "white") +
  geom_label(data = state_labels_map, aes(long, lat, label = state_abbr), show.legend = F, alpha = .3, fill = "grey0", col = "white", size = 2.4, family = "A") +
  coord_map() + 
  scale_fill_viridis_c(direction = -1, breaks = c(0.215, 0.5),
                       labels = c("Menor cobertura", "Mayor cobertura")) + 
  scale_color_gradient(low = "grey80", high = "white") +
  theme_void() +
  
  labs(
    x = NULL,
    fill = NULL,
    y = NULL,
    title = "Porcentaje de hogares que recibe algC:n\nprograma gubernamental",
    caption = "Elaborado por @pcobosalcala con datos de la ENIGH."
  ) +
  
  theme(plot.title = element_markdown(face = "bold", size = 12),
        legend.position = c(.9,.7),
        legend.text = element_text(size = 10),
        plot.subtitle = element_markdown(),
        plot.caption = element_markdown(hjust = 0),
        text = element_text(family = "A"))

ggsave("3_viz/enigh2.png",
       width = 7,
       height = 5,
       dpi = 900)
