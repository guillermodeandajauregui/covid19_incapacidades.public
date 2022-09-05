library(tidyverse)
library(vroom)
library(lubridate)


dict_ent <- vroom("data/diccionario_ent.txt.csv")
dict_ent <- dict_ent %>% mutate(no = as.character(no))
#vamos a bajar los datos de la página del imss --- 

#aca sacamos los fines de mes
date.start.month=seq(as.Date("2020-03-01"),length=23,by="months")
mis_meses =data.frame(date.start.month)
mis_meses$date.end.month <- ceiling_date(mis_meses$date.start.month, "month") - days(1)
mis_meses$date.end.month
# 
# #e iteramos sobre eso 
# 
lista_derechohabientes <-
  lapply(mis_meses$date.end.month, function(i){
    
    print(i)
    
    mi_url <- paste0("http://datos.imss.gob.mx/sites/default/files/pda-",
                     i,
                     ".csv"
    )
    
    mi_mes <- month(ymd(i))
    mi_anu <- year(ymd(i))
    
    yyy <-
      vroom(mi_url) %>%
      group_by(ID_DELEG_RP) %>%
      summarise(TOT_CASOS = sum(TOT_CASOS)) #%>%
    #mutate(no = as.character(ID_DELEG_RP))
    
    
    r <-
      left_join(yyy, dict_ent, by="ID_DELEG_RP") %>%
      group_by(ENT_NOM) %>%
      summarize(TOT_CASOS = sum(TOT_CASOS)) %>%
      mutate(year = mi_anu,
             month = mi_mes)
    
    return(r)
  })

df_derechohabientes <- lista_derechohabientes %>% bind_rows()
df_derechohabientes %>% vroom_write("data/derechohabientes.txt")