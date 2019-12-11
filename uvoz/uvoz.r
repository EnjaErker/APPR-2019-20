# 2. faza: Uvoz podatkov

library(knitr)
library(rvest)
library(gsubfn)
library(tidyr)
library(shiny)
library(readr)
#library(readx1)
#library(xlsx)

sl <- locale("sl", decimal_mark=",", grouping_mark=".")

#funkcija, ki uvozi csv datoteko z napeljavami
napeljave_2002 <- read_csv("podatki/napeljave_2002.csv")
napeljave_2002 <- read_csv2("podatki/napeljave_2002.csv", na="-", locale=locale(encoding="Windows-1250"), col_names=c("obcina", "vodovod", "kanalizacija", "elektricni tok", "centralno ogrevanje", "kopalnica", "stranisce", "kuhinja") )

#funkcija, ki uvozi excel datoteko z kazalniki
kazalniki <- read_xlsx("podatki/kazalniki.xlsx")

#funkcija, ki uvozi json datoteko s stevilom oseb in sob
st_sob_oseb <- read_json("podatki/st_sob_oseb.json",na="-", locale=locale(encoding="Windows-1250"))

#funkcija, ki uvozi xml datoteko o naseljenosti in pomankljivostih
naseljenost_in_pomanjkljivosti <- read_xml("podatki/naseljenost_in_pomanjkljivosti.xml", na="-", locale=locale(encoding="Windows-1250"))


# Funkcija, ki uvozi občine iz Wikipedije
uvozi.obcine <- function() {
  link <- "http://sl.wikipedia.org/wiki/Seznam_ob%C4%8Din_v_Sloveniji"
  stran <- html_session(link) %>% read_html()
  tabela <- stran %>% html_nodes(xpath="//table[@class='wikitable sortable']") %>%
    .[[1]] %>% html_table(dec=",")
  for (i in 1:ncol(tabela)) {
    if (is.character(tabela[[i]])) {
      Encoding(tabela[[i]]) <- "UTF-8"
    }
  }
  colnames(tabela) <- c("obcina", "povrsina", "prebivalci", "gostota", "naselja",
                        "ustanovitev", "pokrajina", "regija", "odcepitev")
  tabela$obcina <- gsub("Slovenskih", "Slov.", tabela$obcina)
  tabela$obcina[tabela$obcina == "Kanal ob Soči"] <- "Kanal"
  tabela$obcina[tabela$obcina == "Loški potok"] <- "Loški Potok"
  for (col in c("povrsina", "prebivalci", "gostota", "naselja", "ustanovitev")) {
    if (is.character(tabela[[col]])) {
      tabela[[col]] <- parse_number(tabela[[col]], na="-", locale=sl)
    }
  }
  for (col in c("obcina", "pokrajina", "regija")) {
    tabela[[col]] <- factor(tabela[[col]])
  }
  return(tabela)
}

# Funkcija, ki uvozi podatke iz datoteke druzine.csv
uvozi.druzine <- function(obcine) {
  data <- read_csv2("podatki/druzine.csv", col_names=c("obcina", 1:4),
                    locale=locale(encoding="CP1250"))
  data$obcina <- data$obcina %>% strapplyc("^([^/]*)") %>% unlist() %>%
    strapplyc("([^ ]+)") %>% sapply(paste, collapse=" ") %>% unlist()
  data$obcina[data$obcina == "Sveti Jurij"] <- "Sveti Jurij ob Ščavnici"
  data <- data %>% gather(`1`:`4`, key="velikost.druzine", value="stevilo.druzin")
  data$velikost.druzine <- parse_number(data$velikost.druzine)
  data$obcina <- parse_factor(data$obcina, levels=obcine)
  return(data)
}

# Zapišimo podatke v razpredelnico obcine
obcine <- uvozi.obcine()

# Zapišimo podatke v razpredelnico druzine.
druzine <- uvozi.druzine(levels(obcine$obcina))

# Če bi imeli več funkcij za uvoz in nekaterih npr. še ne bi
# potrebovali v 3. fazi, bi bilo smiselno funkcije dati v svojo
# datoteko, tukaj pa bi klicali tiste, ki jih potrebujemo v
# 2. fazi. Seveda bi morali ustrezno datoteko uvoziti v prihodnjih
# fazah.
