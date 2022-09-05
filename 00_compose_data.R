library(tidyverse)
library(vroom)
library(lubridate)
library(tsibble)

#leer tablas - 

tabla_2020 <- vroom("data/tabla_2020.txt", delim = "|") %>% mutate(no = as.character(no)) 
tabla_2021 <- vroom("data/tabla_2021.txt", delim = "|") %>% mutate(no = as.character(no)) 
tabla_2022 <- vroom("data/tabla_2022.txt", delim = "|") %>% mutate(no = as.character(no)) 

#poner nombres adecuados ----

dict_ent <- vroom("data/diccionario_ent.txt.csv")
dict_ent <- dict_ent %>% mutate(no = as.character(no))

tabla_2020 <- left_join(tabla_2020, dict_ent, by="no") %>% select(ENT_NOM, everything()) %>% select(-no) %>% select(!contains("ooad"))
tabla_2021 <- left_join(tabla_2021, dict_ent, by="no") %>% select(ENT_NOM, everything()) %>% select(-no) %>% select(!contains("ooad"))
tabla_2022 <- left_join(tabla_2022, dict_ent, by="no") %>% select(ENT_NOM, everything()) %>% select(-no) %>% select(!contains("ooad"))


#agregar los estados que tienen más de una delegación del IMSS ----

tabla_2020 <- tabla_2020 %>% group_by(ENT_NOM) %>% summarise(across(where(is_numeric), sum))
tabla_2021 <- tabla_2021 %>% group_by(ENT_NOM) %>% summarise(across(where(is_numeric), sum))
tabla_2022 <- tabla_2022 %>% group_by(ENT_NOM) %>% summarise(across(where(is_numeric), sum))

#juntar en una tabla larga ----

df <- 
  list("2020" = tabla_2020 %>% pivot_longer(cols = -ENT_NOM, names_to = "month", values_to = "mxn"),
     "2021" = tabla_2021 %>% pivot_longer(cols = -ENT_NOM, names_to = "month", values_to = "mxn"),
     "2022" = tabla_2022 %>% pivot_longer(cols = -ENT_NOM, names_to = "month", values_to = "mxn")
     
     ) %>% bind_rows(.id = "year") %>% mutate(year = as.numeric(year)) %>% 
  select(ENT_NOM, year, month, mxn)


#hacer un dict de meses ---- 

dict.meses <- tibble(month.num = (1:12 %>% str_pad(width = 2, side = "left", pad = 0)), 
                     month = (tabla_2021 %>% colnames() %>% .[-c(1,14)])
                     )


df <-
  left_join(df, dict.meses) %>% 
  mutate(yearmon = make_yearmonth(year = year, month = month.num)
         )

df <- 
  df %>% drop_na()


#un primer plot nada más para ver que todo en orden ---
df %>% 
  ggplot() + 
  aes(yearmon, mxn, color = ENT_NOM) + 
  geom_line() + 
  geom_point()  + 
  scale_x_yearmonth(breaks = "3 months") + 
  theme(axis.text.x = element_text(angle=90), 
        legend.position = "none") + 
  facet_wrap(vars(ENT_NOM), scales = "free_y") 

#mismo plot con un poco mas de estilo --- 
p_gastos.crudos <- 
  df %>% 
  ggplot() + 
  aes(yearmon, mxn, color = ENT_NOM) + 
  geom_line() + 
  geom_point()  + 
  facet_wrap(vars(ENT_NOM), scales = "free_y", ncol = 8) +
  scale_x_yearmonth(breaks = "3 months") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle=90),
        legend.position = "none",
        strip.text.x = element_text(size = 12),
        text = element_text(size = 16)
  ) + 
  ylab("Gasto en salud (MXN)") + 
  xlab("Mes") + 
  scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6))

ggsave(filename = "results/p_gastos.crudos.pdf", 
       plot = p_gastos.crudos, 
       width = 15, 
       height = 10, 
       units = "in"
)


################################################################################

# poblacion derechohabiente ---- 



df_derechohabientes <- vroom("data/derechohabientes.txt")

df_derechohabientes <- df_derechohabientes %>% mutate(month.num = str_pad(string = month, width = 2, side = "left", pad = 0)) %>% select(-month)

df_derechohabientes %>% 
  group_by(ENT_NOM) %>% tally() %>% print(n=32)

df <- 
  left_join(df, df_derechohabientes) %>% 
  mutate(mxn_per_derechohabiente = mxn/TOT_CASOS) #%>% 

df %>% 
  ggplot() + 
  aes(yearmon, mxn_per_derechohabiente, color = ENT_NOM) + 
  geom_line() + 
  geom_point()  + 
  facet_wrap(vars(ENT_NOM), scales = "free_y", ncol = 8) +
  scale_x_yearmonth(breaks = "3 months") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle=90),
        legend.position = "none",
        strip.text.x = element_text(size = 12),
        text = element_text(size = 16)
  ) + 
  ylab("Gasto por derechohabiente (MXN)") + 
  xlab("Mes") #+ 
  #scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6))


