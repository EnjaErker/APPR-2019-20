# 2. faza: Uvoz podatkov

#paketi
install.packages("shiny")
install.packages("XML")
install.packages("rvest")
install.packages("xlsx")
install.packages("rJava")
install.packages("xlsxjars")
install.packages("rjson")
install.packages("jsonlite")
install.packages("dplyr")

#knjiznice
library(knitr)
library(rvest)
library(gsubfn)
library(tidyr)
library(shiny)
library(readr)
library(xlsx)
library(XML)
library(methods)
library(dplyr)

#nastavitev , in .
sl <- locale("sl", decimal_mark=",", grouping_mark=".")

#NAPELJAVE

#Funkcija, ki uvozi csv datoteko z napeljavami
napeljave <- read_csv2("podatki/napeljave.csv", skip =1, na="-", locale=locale(encoding="Windows-1250"))

#poimenovanje prvega stolpca
names(napeljave)[1]<-("Občina")
#izbris obcin s pomankljivmi podatki (NA)
napeljave <- na.omit(napeljave)
#izbris obcin s pomankljivmi podatki (z)
row_sub = apply(napeljave, 1, function(row) all(row !='z' ))
napeljave <- napeljave[row_sub,]

#KAZALNIKI

#Funkcija, ki uvozi excel datoteko s kazalniki
kazalniki <- read.xlsx("podatki/kazalniki1.xlsx", sheetIndex=1, startRow =1, colNames=TRUE, rowNames=FALSE, encoding="UTF-8")

#poimenovanje stolpcev
names(kazalniki) <- c("Občina", "Gostota prebivalstva (preb/km2)", "Povprečna mesečna neto plača (indeks)", "Stopnja registrirane brezposelnosti (odstotek)", "Število stanovanj (na 1000 prebivalcev)", "1", "2")
#izbris zadnjih dveh stolpcev
kazalniki=select(kazalniki,-6,-7)


#ŠTEVILO OSEB IN SOB

#Funkcija, ki uvozi csv datoteko s stevilom oseb in sob
st_oseb_sob <- read_csv2("podatki/st_oseb_in_sob.csv", skip =2, na="-", locale=locale(encoding="Windows-1250"))

#poimenovanje stolpcev
names(st_oseb_sob) <- c("Občina", "Število sob", "Število oseb", "Število stanovanj") 


#POMANKLJIVOSTI

#Funkcija, ki uvozi excel datoteko o pomankljivostih
pomankljivosti <- read.xlsx("podatki/pomankljivosti1.xlsx", sheetIndex=1, startRow=4,encoding="UTF-8")

#poimenovanje stolpcev
names(pomankljivosti) <- c("Občina", "Število vseh stanovanj","Število stanovanj brez centralnega ogrevanja", "Število stanovanj brez vode", "Število stanovanj brez elektrike", "Število stanovanj brez priklopa na javno kanalizacijo")
#izbris zadnjih 20 vrstic z nepomembnimi podatki
pomankljivosti=pomankljivosti[ -c(213, 214, 215, 216, 217, 218, 219, 220, 221, 222, 223, 224, 225, 226, 227, 228, 229, 230, 231, 232, 233), ]



 




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
