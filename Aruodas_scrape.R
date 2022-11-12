library(rvest)
library(stargazer)
library(pander)
library(dplyr)
library(tidyr)
library(stringr)
library(stringi)
library(xlsx)
library(lubridate)
library(polite)
library(purrr)


url = 'https://m.aruodas.lt/butai/puslapis/1'
webp <- read_html(url)

page_info <- html_nodes(webp, ".page-select-v2")
page_info <- html_text(page_info)
page_info<-gsub("\n","",page_info)
puslapiu_skaicius = strtoi(gsub(" ","", str_split_fixed(stri_enc_toascii(page_info), ' i\032 ', 2)[2]))

butai = matrix(data = NA, nrow = puslapiu_skaicius * 50, ncol= 3)#50 random number
butuSkaicius = 1

for (i in 1:puslapiu_skaicius) {
  Sys.sleep(1)
  webp <- read_html(paste0('https://m.aruodas.lt/butai/puslapis/', i))
  
  item_price <- html_nodes(webp, ".item-price-main-v3")
  item_price <- html_text(item_price)
  
  item_address <- html_nodes(webp, ".item-address-v3")
  item_address <- html_text(item_address)
  
  item_description <- html_nodes(webp, ".item-description-v3")
  item_description <- html_text(item_description)
  
  for (j in 1:length(item_description)) {
    butai[butuSkaicius,1] = item_price[j];
    butai[butuSkaicius,2] = item_address[j];
    butai[butuSkaicius,3] = item_description[j];
    butuSkaicius <- butuSkaicius + 1
  }
  print(paste0('Einu per puslapi: ', i))
}
print(butai)
print(butuSkaicius)

butai<-na.omit(butai)
# Data-Preprocessing (adresas):


butai[,2]<-gsub("\n","",butai[,2])

buto_adresas<-str_split_fixed(butai[,2], ", ", 3)
didieji_m<- c("Vilnius", "Kaunas", "Klaip\032da", "\032iauliai", "Panev\032\032ys", "Alytus", "Marijampol\032s m.", "Utenos m.", "Palanga")
palang_vekt<-c("Palanga", "Sventoji")
viet_sutrumpinimai<-c("m.", "k.", "vs.", "gl\032. st.")
g_sutrumpinimai<-c("g.", "a.", "al.", "aklg.", "tak.", "pr.", "pl.", "skg.", "kel.")


bandm<-matrix(data = NA, nrow = length(butai[,2]), ncol = 4)
bandm[,1]<-buto_adresas[,1]
for (n in 1:length(butai[,2])) {
  if(str_detect(stri_enc_toascii(bandm[n,1]), paste(didieji_m, collapse = "|")) == TRUE){
    bandm[n,2]<-buto_adresas[n,1]
  } else if (str_detect(stri_enc_toascii(buto_adresas[n,1]), paste(viet_sutrumpinimai, collapse = "|")) == TRUE){
    bandm[n,2]<-buto_adresas[n,1]
  } else if (str_detect(stri_enc_toascii(buto_adresas[n,2]), paste(viet_sutrumpinimai, collapse = "|")) == TRUE){
    bandm[n,2]<-buto_adresas[n,2] 
  } else{NA}
}


didieji_m<- c("Vilnius", "Kaunas", "Klaip\032da", "\032iauliai", "Panev\032\032ys", "Alytus", "Marijampol\032s m.", "Utenos m.")
for (j in 1:length(butai[,2])) {
  if (str_detect(stri_enc_toascii(buto_adresas[j,3]), paste(g_sutrumpinimai, collapse = "|")) == TRUE){
    bandm[j,4]<-buto_adresas[j,3]
  } else if (str_detect(stri_enc_toascii(buto_adresas[j,2]), paste(g_sutrumpinimai, collapse = "|")) == TRUE){
    bandm[j,4]<-buto_adresas[j,2]
  }else {NA}
}

for (k in 1:length(butai[,2])) {
  if (str_detect(stri_enc_toascii(buto_adresas[k,2]), "\032ventoji") == TRUE){
    bandm[k,3]<-buto_adresas[k,2]
  } else if (str_detect(stri_enc_toascii(buto_adresas[k,1]), "Palanga") == TRUE){
    bandm[k,3]<-buto_adresas[k,1]
  } else if (str_detect(stri_enc_toascii(bandm[k,1]), paste(didieji_m, collapse = "|")) == TRUE){
    bandm[k,3]<-buto_adresas[k,2]
  } else{NA}
}
adresas<-bandm


# Data-Preprocessing (kaina):

butai[,1]<-gsub("\n","",butai[,1])
butai[,1]<-gsub("Skirtumai: 1","",butai[,1])
butai[,1]<-gsub("Skirtumai:0","",butai[,1])
  
price<-str_split_fixed(butai[,1], "???", 2)
price[,2]<-gsub(" ","", price[,2])
price[,2]<-gsub("Skirtumai:0","", price[,2])
  

# Data-Preprocessing (aprasymas): 
  
butai[,3]<-gsub("\n","",butai[,3])
  
descript<-str_split_fixed(butai[,3], ", ", 8)
descript[,1]<-gsub(" ","", descript[,1])
descript[,2]<-gsub(",",".", descript[,2])
descript[,2]<-gsub(" m²","", descript[,2])
descript[,2]<-gsub(")","", descript[,2])
descript[,3]<-str_split_fixed(descript[,3], "/", 2)[,1]
descript[,4]<-gsub(" m.","", descript[,4])
 
# Naujas stulpelis irengimui:
  
naujas_stulpelis<-matrix(data = NA, nrow = length(butai[,3]))

  for (m in 1:length(butai[,3])) {
    if(str_detect(stri_enc_toascii(butai[m,3]), ' \032rengtas') == 'TRUE'){
      naujas_stulpelis[m,]<-"Irengtas"
    } else if(str_detect(stri_enc_toascii(butai[m,3]), ' ne\032rengtas') == 'TRUE'){
      naujas_stulpelis[m,]<-"Neirengtas"
    } else if (str_detect(stri_enc_toascii(butai[m,3]), 'dalin\032 apdaila') == 'TRUE'){
      naujas_stulpelis[m,]<-"Daline apdaila"
    } else if(str_detect(stri_enc_toascii(butai[m,3]), 'Kita')== 'TRUE'){
      naujas_stulpelis[m,]<-"Kita"
    } else if(str_detect(stri_enc_toascii(butai[m,3]), 'Nebaigtas statyti')== 'TRUE'){
      naujas_stulpelis[m,]<-"Nebaigtas statyti"
    } else if(str_detect(stri_enc_toascii(butai[m,3]), 'Pamatai')== 'TRUE'){
      naujas_stulpelis[m,]<-"Pamatai"
    }  else {NULL}
    
  }
  
# Export'as i Excel:

  df<-data.frame(Savivaldybe = adresas[,1], Miestas = adresas[,2], Mikrorajonas = adresas[,3], Gatve = adresas[,4], Kaina = price[,1], Kv_m_kaina = price[,2], Kambariu_sk = descript[,1], Kv_m = descript[,2], Aukstas = descript[,3], Statybos_metai = descript[,4], Pastato_tipas = descript[,5], Irengimas = naujas_stulpelis)
  write.xlsx(df, file = "Aruodas_21_07_10.xlsx", sheetName = "2021-07-10", append = FALSE)#irasyti data YY-MM-DD
  
 