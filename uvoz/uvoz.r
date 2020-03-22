# 2. faza: Uvoz podatkov

#paketi
install.packages("shiny")
install.packages("XML")
install.packages("rvest")
install.packages("xlsx")
install.packages("rJava")
install.packages("xlsxjars")
install.packages("dplyr")
install.packages("data.table")

#nastavitev , in .
osnovne.nastavitve <- function(){
  sl <- locale("sl", decimal_mark=",", grouping_mark=".")
}

#NAPELJAVE

#funkcija, ki uvozi csv datoteko z napeljavami, poimenuje prvi stolpec, izbriše občine s pomankljivimi podatki (NA in z)
uvozi.napeljave <- function(){
  napeljave <- read_csv2("podatki/napeljave.csv", skip =1, na="-", locale=locale(encoding="Windows-1250"))
  names(napeljave)[1]<-("Obcina")
  napeljave <- na.omit(napeljave)
  row_sub = apply(napeljave, 1, function(row) all(row !='z' ))
  napeljave <- napeljave[row_sub,]
  return(napeljave)
}

#zapis podatkov v razpredelnico napeljave
napeljave <- uvozi.napeljave()



#POMANKLJIVOSTI

#funkcija, ki uvozi excel datoteko o pomankljivostih, poimenuje stolpce in izbriše zadnjih 20 vrstic z nepomembnimi podatki
uvozi.pomankljivosti <- function(){
  pomankljivosti <- read.xlsx("podatki/pomankljivosti1.xlsx", sheetIndex=1, startRow=4,encoding="UTF-8")
  names(pomankljivosti) <- c("Obcina", "st_vseh_stanovanj","centralno_ogrevanje", "voda", "elektrika", "javna_kanalizacija")
  pomankljivosti=pomankljivosti[ -c(213:233), ]
  return(pomankljivosti)
}

#zapis podatkov v razpredelnico pomankljivosti
pomankljivosti <- uvozi.pomankljivosti()

#KAZALNIKI

#funkcija, ki uvozi excel datoteko s kazalniki, poimenuje stolpce, izbriše zadnje dva stolpca
uvozi.kazalnike <- function(){
  kazalniki <- read.xlsx("podatki/kazalniki1.xlsx", sheetIndex=1, startRow =1, colNames=TRUE, rowNames=FALSE, encoding="UTF-8")
  names(kazalniki) <- c("Obcina", "gostota_prebivalstva", "pov_mes_neto_placa_indeks", "st_reg_brezposelnosti_ods", "st_stan_na_tisoc_preb", "1", "2")
  kazalniki=select(kazalniki,-6,-7)
  return(kazalniki)
}

#zapis podatkov v razpredelnico kazalniki
kazalniki <- uvozi.kazalnike()





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
