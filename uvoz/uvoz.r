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
  names(napeljave)[1]<-("Občina")
  napeljave <- na.omit(napeljave)
  row_sub = apply(napeljave, 1, function(row) all(row !='z' ))
  napeljave <- napeljave[row_sub,]
  return(napeljave)
}

#zapis podatkov v razpredelnico napeljave
napeljave <- uvozi.napeljave()

#funckija, ki izdela podtabelo osnovne tabele o napeljavah (posebej le vodovode) za lazje dostopanje do podatkov kasneje in popravi imena stolpcev, uvede spremembo: character->numeric
naredi.podtabelo.vodovoda <- function(){
  vodovod <- napeljave[,c("Občina","2002 Vodovod","2003 Vodovod","2004 Vodovod","2005 Vodovod","2006 Vodovod","2007 Vodovod", "2008 Vodovod")]
  names(vodovod) <- c("Občina","2002","2003","2004","2005","2006","2007","2008")
  num.cols <- c('2002','2003','2004','2005','2006','2007','2008')
  vodovod[num.cols] <- sapply(vodovod[num.cols], as.numeric)
  return(vodovod)
}

#zapis podatkov v razpredelnico vodovod
vodovod <- naredi.podtabelo.vodovoda()

#funckija, ki izdela podtabelo osnovne tabele o napeljavah (posebej le kanalizacije) za lazje dostopanje do podatkov kasneje in popravi imena stolpcev, uvede spremembo: character->numeric
naredi.podtabelo.kanalizacije <- function(){
  kanalizacija <- napeljave[,c("Občina","2002 Kanalizacija","2003 Kanalizacija","2004 Kanalizacija","2005 Kanalizacija","2006 Kanalizacija","2007 Kanalizacija", "2008 Kanalizacija")]
  names(kanalizacija) <- c("Občina","2002","2003","2004","2005","2006","2007","2008")
  num.cols <- c('2002','2003','2004','2005','2006','2007','2008')
  kanalizacija[num.cols] <- sapply(kanalizacija[num.cols], as.numeric)
  return(kanalizacija)
}

#zapis podatkov v razpredelnico kanalizacija
kanalizacija <- naredi.podtabelo.kanalizacije()

#funckija, ki izdela podtabelo osnovne tabele o napeljavah (posebej le električni tok) za lazje dostopanje do podatkov kasneje in popravi imena stolpcev, uvede spremembo: character->numeric
naredi.podtabelo.elektricnega_toka <- function(){
  električni_tok <- napeljave[,c("Občina","2002 Električni tok","2003 Električni tok","2004 Električni tok","2005 Električni tok","2006 Električni tok","2007 Električni tok", "2008 Električni tok")]
  names(vodovod) <- c("Občina","2002","2003","2004","2005","2006","2007","2008")
  names(električni_tok) <- c("Občina","2002","2003","2004","2005","2006","2007","2008")
  num.cols <- c('2002','2003','2004','2005','2006','2007','2008')
  električni_tok[num.cols] <- sapply(električni_tok[num.cols], as.numeric)
  return(električni_tok)
}

#zapis podatkov v razpredelnico električni_tok
električni_tok <- naredi.podtabelo.elektricnega_toka()

#funckija, ki izdela podtabelo osnovne tabele o napeljavah (posebej le centralno ogrevanje) za lazje dostopanje do podatkov kasneje in popravi imena stolpcev, uvede spremembo: character->numeric
naredi.podtabelo.centralnega_ogrevanja <- function(){
  centralno_ogrevanje <- napeljave[,c("Občina","2002 Centralno ogrevanje","2003 Centralno ogrevanje","2004 Centralno ogrevanje","2005 Centralno ogrevanje","2006 Centralno ogrevanje","2007 Centralno ogrevanje", "2008 Centralno ogrevanje")]
  names(centralno_ogrevanje) <- c("Občina","2002","2003","2004","2005","2006","2007","2008")
  num.cols <- c('2002','2003','2004','2005','2006','2007','2008')
  centralno_ogrevanje[num.cols] <- sapply(centralno_ogrevanje[num.cols], as.numeric)
  return(centralno_ogrevanje)
}

#zapis podatkov v razpredelnico centralno_ogrevanje
centralno_ogrevanje <- naredi.podtabelo.centralnega_ogrevanja()

#funckija, ki izdela podtabelo osnovne tabele o napeljavah (posebej le kopalnica) za lazje dostopanje do podatkov kasneje in popravi imena stolpcev, uvede spremembo: character->numeric
naredi.podtabelo.kopalnice <- function(){
  kopalnica <- napeljave[,c("Občina","2002 Kopalnica","2003 Kopalnica","2004 Kopalnica","2005 Kopalnica","2006 Kopalnica","2007 Kopalnica", "2008 Kopalnica")]
  names(kopalnica) <- c("Občina","2002","2003","2004","2005","2006","2007","2008")
  num.cols <- c('2002','2003','2004','2005','2006','2007','2008')
  kopalnica[num.cols] <- sapply(kopalnica[num.cols], as.numeric)
  return(kopalnica)
}

#zapis podatkov v razpredelnico kopalnica
kopalnica <- naredi.podtabelo.kopalnice()

#funckija, ki izdela podtabelo osnovne tabele o napeljavah (posebej le stranišče) za lazje dostopanje do podatkov kasneje in popravi imena stolpcev, uvede spremembo: character->numeric
naredi.podtabelo.stranisca <- function(){
  stranišče <- napeljave[,c("Občina","2002 Stranišče","2003 Stranišče","2004 Stranišče","2005 Stranišče","2006 Stranišče","2007 Stranišče", "2008 Stranišče")]
  names(stranišče) <- c("Občina","2002","2003","2004","2005","2006","2007","2008")
  num.cols <- c('2002','2003','2004','2005','2006','2007','2008')
  stranišče[num.cols] <- sapply(stranišče[num.cols], as.numeric)
  return(stranišče)
}

#zapis podatkov v razpredelnico stranišče
stranišče <- naredi.podtabelo.stranisca()

#funckija, ki izdela podtabelo osnovne tabele o napeljavah (posebej le kuhinjo) za lazje dostopanje do podatkov kasneje in popravi imena stolpcev, uvede spremembo: character->numeric
naredi.podtabelo.kuhinje <- function(){
  kuhinja <- napeljave[,c("Občina","2002 Kuhinja","2003 Kuhinja","2004 Kuhinja","2005 Kuhinja","2006 Kuhinja","2007 Kuhinja", "2008 Kuhinja")]
  names(kuhinja) <- c("Občina","2002","2003","2004","2005","2006","2007","2008")
  num.cols <- c('2002','2003','2004','2005','2006','2007','2008')
  kuhinja[num.cols] <- sapply(kuhinja[num.cols], as.numeric)
  return(kuhinja)
}

#zapis podatkov v razpredelnico kuhinja
kuhinja <- naredi.podtabelo.kuhinje()

#KAZALNIKI

#funkcija, ki uvozi excel datoteko s kazalniki, poimenuje stolpce, izbriše zadnje dva stolpca
uvozi.kazalnike <- function(){
  kazalniki <- read.xlsx("podatki/kazalniki1.xlsx", sheetIndex=1, startRow =1, colNames=TRUE, rowNames=FALSE, encoding="UTF-8")
  names(kazalniki) <- c("Občina", "Gostota prebivalstva (preb/km2)", "Povprečna mesečna neto plača (indeks)", "Stopnja registrirane brezposelnosti (odstotek)", "Število stanovanj (na 1000 prebivalcev)", "1", "2")
  kazalniki=select(kazalniki,-6,-7)
  return(kazalniki)
}

#zapis podatkov v razpredelnico kazalniki
kazalniki <- uvozi.kazalnike()

#POMANKLJIVOSTI

#funkcija, ki uvozi excel datoteko o pomankljivostih, poimenuje stolpce in izbriše zadnjih 20 vrstic z nepomembnimi podatki
uvozi.pomankljivosti <- function(){
  pomankljivosti <- read.xlsx("podatki/pomankljivosti1.xlsx", sheetIndex=1, startRow=4,encoding="UTF-8")
  names(pomankljivosti) <- c("Občina", "Število vseh stanovanj","Število stanovanj brez centralnega ogrevanja", "Število stanovanj brez vode", "Število stanovanj brez elektrike", "Število stanovanj brez priklopa na javno kanalizacijo")
  pomankljivosti=pomankljivosti[ -c(213:233), ]
  return(pomankljivosti)
}

#zapis podatkov v razpredelnico pomankljivosti
pomankljivosti <- uvozi.pomankljivosti()







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
