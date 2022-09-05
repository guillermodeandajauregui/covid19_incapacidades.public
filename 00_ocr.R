library(tesseract)
library(tidyverse)

pngfile <- pdftools::pdf_convert('raw/ANEXO I SOLICITUD 3300180224771.pdf', dpi = 600)

# manipulacion de las imagenes con GIMP para mejorar el OCR.
# filter/enhance/sharpen radius 32, amount 1 

#leemos con ocr 
text_1 <- tesseract::ocr("ANEXO I SOLICITUD 3300180224771_1_enhanced.png", engine = tesseract(language = "spa"))
text_2 <- tesseract::ocr("ANEXO I SOLICITUD 3300180224771_2._enhanced.png", engine = tesseract(language = "spa"))
text_3 <- tesseract::ocr("ANEXO I SOLICITUD 3300180224771_3._enhanced.png", engine = tesseract(language = "spa"))

#ahora las copiamos y las trabajamos manualmente en el Geany para usar REGEX

text_1 %>% clipr::write_clip()
#dice: 
#Importe expedido por los certificados de ITT que generaron subsidio, otorgados a asegurados de empresas
#afiliadas al IMSS, cuya descripción como “CASO SOSPECHOSO"” incluye el diagnóstico médico: “COVID 19”,
#“CORONAVIRUS”, “SARS-COV”, “COV19” y “COV?”, desagregado por OOAD.

text_2 %>% clipr::write_clip()
#dice:
#Importe expedido por los certificados de ITT que generaron subsidio, otorgados a asegurados de empresas
#afiliadas al IMSS, cuya descripción como “CASO SOSPECHOSO” incluye el diagnóstico médico: “COVID 19”,
#“CORONAVIRUS”, “SARS-COV”, “COVI19” y “COV?”, desagregado por OOAD.
#(enero a diciembre 2021)

text_3 %>% clipr::write_clip()
#dice:
#Cuadro 3.
#Importe expedido por los certificados de ITT que generaron subsidio, otorgados a asegurados de empresas
#afiliadas al IMSS, cuya descripción como “CASO SOSPECHOSO” incluye el diagnóstico médico: “COVID 19”,
#“CORONAYVIRUS”, *“SARS-COV”, “COVI19” y “COV2”, desagregado por OOAD.
#(enero 2022)

#en geany:
# quitar comas
# validar números manualmente
# separar por | 

#escribir en tabla_2020.txt, tabla_2021.txt, tabla_2022.txt