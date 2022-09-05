library(tidyverse)
library(vroom)
library(lubridate)
library(zoo)
library(DBI)
library(RSQL)
library(odbc)
library(RMariaDB)
library(patchwork)
library(tsibble)

remota <- vroom::vroom("https://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip")
#si tienen problemas con esta, recomiendo usar el paquete de https://github.com/RodrigoZepeda/covidmx para acceder a los datos

#poblacion ----
dict_ent <- vroom("data/dict_poblacion.txt")

#defunciones ----
defunciones_ent <-
  remota %>%
  select(FECHA_DEF, CLASIFICACION_FINAL, ENTIDAD_RES) %>%
  filter(CLASIFICACION_FINAL%in%1:3) %>%
  filter(FECHA_DEF > "2020-03-01") %>%
  group_by(ENTIDAD_RES, FECHA_DEF) %>%
  tally() %>%
  collect()

# 
# defunciones_ent.month <- 
#   defunciones_ent %>% 
#   mutate(year_month = yearmonth(FECHA_DEF)) %>% 
#   group_by(ENTIDAD_RES, year_month) %>% 
#   summarize(defunciones = sum(n)) %>% 
#   left_join(y = dict_ent, by=c("ENTIDAD_RES"="CVEGEO.num"))
# 
# defunciones_ent.month %>% vroom_write("results/defunciones_mensuales.txt")
defunciones_ent.month <- vroom("results/defunciones_mensuales.txt") %>% mutate(year_month=yearmonth(year_month))

p_def.ent.month <- 
  defunciones_ent.month %>% 
  mutate(defunciones_100k_habitantes = 100000*defunciones/POBTOT) %>% 
  ggplot() + 
  aes(year_month, defunciones_100k_habitantes, color=NOMGEO, fill = NOMGEO) + 
  geom_bar(stat="identity") + 
  facet_wrap(vars(NOMGEO), ncol = 8) +
  scale_x_yearmonth(breaks = "3 months") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle=90),
        legend.position = "none",
        strip.text.x = element_text(size = 12),
        text = element_text(size = 18)
        ) + 
  ylab("Defunciones por 100mil habitantes") + 
  xlab("Mes") 

p_def.ent.month

#notas del editor ---- 
#1) usar ncol = 8 para que matriz quede simétrica; 
#2) aumentar el tamaño del título de cada faceta; 
#y 3) aumentar el tamaño de etiquetas en los ejes

#hospitalizaciones ---- 

hospitalizaciones_ent <-
  remota %>%
  select(FECHA_SINTOMAS, CLASIFICACION_FINAL, TIPO_PACIENTE, ENTIDAD_RES) %>%
  filter(CLASIFICACION_FINAL%in%1:3) %>%
  filter(TIPO_PACIENTE==2) %>%
  group_by(ENTIDAD_RES, FECHA_SINTOMAS) %>%
  tally() %>%
  collect()


# hospitalizaciones_ent.month <- 
#   hospitalizaciones_ent %>% 
#   mutate(year_month = yearmonth(FECHA_SINTOMAS)) %>% 
#   group_by(ENTIDAD_RES, year_month) %>% 
#   summarize(hospitalizaciones = sum(n)) %>% 
#   left_join(y = dict_ent, by=c("ENTIDAD_RES"="CVEGEO.num"))
# 
# hospitalizaciones_ent.month %>% vroom_write("results/hospitalizaciones_mensuales.txt")
hospitalizaciones_ent.month <- vroom("results/hospitalizaciones_mensuales.txt") %>% mutate(year_month=yearmonth(year_month))

p_hos.ent.month <- 
  hospitalizaciones_ent.month %>% 
  mutate(hospitalizaciones_100k_habitantes = 100000*hospitalizaciones/POBTOT) %>% 
  ggplot() + 
  aes(year_month, hospitalizaciones_100k_habitantes, color=NOMGEO, fill=NOMGEO) + 
  # geom_line() + 
  # geom_point() +
  geom_bar(stat="identity") + 
  facet_wrap(vars(NOMGEO), ncol=8) +
  scale_x_yearmonth(breaks = "3 months") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle=90),
        legend.position = "none",
        strip.text.x = element_text(size = 12),
        text = element_text(size = 18)
  ) + 
  ylab("Hospitalizaciones por 100mil habitantes") + 
  xlab("Mes") 

p_hos.ent.month


ggsave(filename = "results/barras_p_hos.ent.month.pdf", 
       plot = p_hos.ent.month, 
       width = 15, 
       height = 10, 
       units = "in"
)

ggsave(filename = "results/barras_p_def.ent.month.pdf", 
       plot = p_def.ent.month, 
       width = 15, 
       height = 10, 
       units = "in"
)

#nacionales -----

p.hospitalizaciones <-
  hospitalizaciones_ent.month %>% 
  group_by(year_month) %>% 
  summarise(hospitalizaciones = as.numeric(sum(hospitalizaciones))) %>% 
  tail(-1) %>% #para empatar con abril
  ggplot() + 
  aes(year_month, hospitalizaciones) + 
  geom_bar(stat="identity") +
  scale_x_yearmonth(breaks = "1 month") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle=90),
        legend.position = "none", 
        text = element_text(size=18)
  )  + 
  xlab("Mes") 

p.hospitalizaciones

p.defunciones <- 
  defunciones_ent.month %>% 
  group_by(year_month) %>% 
  summarise(defunciones = as.numeric(sum(defunciones))) %>% 
  ggplot() + 
  aes(year_month, defunciones) + 
  geom_bar(stat="identity") +
  scale_x_yearmonth(breaks = "1 month") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle=90),
        legend.position = "none", 
        text = element_text(size=18)
  )  + 
  xlab("Mes") 

p_hospdef.nal <- p.hospitalizaciones/p.defunciones
ggsave(filename = "results/barras_hosp_def_nacional.pdf", 
       plot = p_hospdef.nal, 
       width = 15, 
       height = 10, 
       units = "in"
       )

# incidencia total ----


incidencia_ent <-
  remota %>% 
  select(FECHA_SINTOMAS, CLASIFICACION_FINAL, TIPO_PACIENTE, ENTIDAD_RES) %>% 
  filter(CLASIFICACION_FINAL%in%1:3) %>%
  group_by(ENTIDAD_RES, FECHA_SINTOMAS) %>% 
  tally() %>% 
  collect()


incidencia_ent.month <- 
  incidencia_ent %>% 
  mutate(year_month = yearmonth(FECHA_SINTOMAS)) %>% 
  group_by(ENTIDAD_RES, year_month) %>% 
  summarize(incidencias = sum(n) %>% as.numeric()) %>% 
  #left_join(y = dict_ent %>% mutate(CVEGEO.num = as.character(CVEGEO.num)), by=c("ENTIDAD_RES"="CVEGEO.num"))
  left_join(y = dict_ent, by=c("ENTIDAD_RES"="CVEGEO"))

#incidencia_ent.month %>% vroom_write("results/incidencias_mensuales.txt")
incidencia_ent.month <- vroom("results/incidencias_mensuales.txt") %>% mutate(year_month=yearmonth(year_month))














##########################################################################################
## para hacer gfx de incidencias vs gasto 
## correr 00_compose_data y leer los datos de incidencia 
library(ggpubr)

#combinar los datos 

aaa <- incidencia_ent.month %>% ungroup %>% select(NOM_ENT, year_month, incidencias, POBTOT)
bbb <- df %>% select(ENT_NOM, yearmon, mxn, derechohabientes = TOT_CASOS, mxn_per_derechohabiente)

ccc <- left_join(aaa, bbb, by=c("NOM_ENT"="ENT_NOM", "year_month"="yearmon")) %>% mutate(incidencia_100k = 100000*incidencias/POBTOT)

#grafica de incidencia vs gasto
#de chida ----

p_gasto_v_incidencia <- 
  ccc %>% 
  mutate(fecha = as.Date(year_month)) %>% 
  mutate(variante = case_when(fecha < ("2020-11-01") ~ "Wild Type",
                              fecha < ("2021-05-01") ~ "B.1.519",
                              fecha < ("2021-12-01") ~ "Delta",
                              fecha < ("2022-12-01") ~ "Omicron"
  )
  ) %>% 
  drop_na() %>% 
  mutate(variante = as_factor(variante)) %>% 
  ggplot() + 
  aes(incidencia_100k, mxn_per_derechohabiente, color = variante) + 
  geom_point() + 
  geom_smooth(method = "lm", linetype = 2, color = "black", alpha = 0.5, size=0.5) + 
  facet_wrap(vars(NOM_ENT), scales = "free", ncol=8) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle=90),
        legend.position = "none", 
        text = element_text(size=18)
  ) + 
  xlab("Incidencia por 100mil habitantes") + 
  ylab("Gasto por derechohabiente (MXN)")


p_gasto_v_incidencia
# booyah----

#PARA MAPEAR -----

library(sf)
shp_ent <- read_sf(mi_path) # path al shp de entidades del conjunto de datos geoestadísticos INEGI 2020

p_mapa_derechohabientes <- 
  ccc %>% 
  drop_na() %>% 
  mutate(derechohabientes_per_capita = derechohabientes/POBTOT) %>% 
  group_by(NOM_ENT) %>% 
  summarise(derechohabientes_per_capita = mean(derechohabientes_per_capita))  %>% 
  left_join(y = shp_ent, by=c("NOM_ENT"="NOMGEO")) %>% 
  ggplot() + 
  geom_sf(aes(fill=derechohabientes_per_capita, geometry=geometry), lwd=0)  + 
  coord_sf(datum=NA) + 
  theme_minimal()  + 
  scale_fill_viridis_c("Derechohabientes per capita", option = "B") + 
  theme(text = element_text(size = 15), 
        legend.text = element_text(angle=90, hjust = 0.75, size = 12),
        legend.position = "bottom")
  



ccc <- left_join(aaa, bbb, by=c("NOM_ENT"="ENT_NOM", "year_month"="yearmon")) %>% mutate(incidencia_100k = 100000*incidencias/POBTOT)
