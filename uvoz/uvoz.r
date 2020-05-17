# UVOZ

#nastavitev , in .
osnovne.nastavitve <- function(){sl <- locale("sl", decimal_mark=",", grouping_mark=".")}

#NAPELJAVE

#funkcija, ki uvozi csv datoteko z napeljavami, poimenuje prvi stolpec, izbriše občine s pomankljivimi podatki (NA in z)
uvozi.napeljave <- function(){
  napeljave <- read_csv2("podatki/napeljave.csv", skip =1, na="-", locale=locale(encoding="Windows-1250"))
  names(napeljave)[1]<-("Obcina")
  napeljave <- na.omit(napeljave)
  row_sub = apply(napeljave, 1, function(row) all(row !='z' ))
  napeljave <- napeljave[row_sub,]
  return(napeljave)}

#zapis podatkov v razpredelnico napeljave
napeljave <- uvozi.napeljave()

#POMANKLJIVOSTI

#funkcija, ki uvozi excel datoteko o pomankljivostih, poimenuje stolpce in izbriše zadnjih 20 vrstic z nepomembnimi podatki
uvozi.pomankljivosti <- function(){
  pomankljivosti <- read_xlsx("podatki/pomankljivosti1.xlsx", sheet=1, skip=3)
  names(pomankljivosti) <- c("Obcina", "st_vseh_stanovanj","centralno_ogrevanje", "voda", "elektrika", "javna_kanalizacija")
  pomankljivosti=pomankljivosti[ -c(213:233), ]
  return(pomankljivosti)}

#zapis podatkov v razpredelnico pomankljivosti
pomankljivosti <- uvozi.pomankljivosti()

#KAZALNIKI

#funkcija, ki uvozi excel datoteko s kazalniki, poimenuje stolpce, izbriše zadnje dva stolpca
uvozi.kazalnike <- function(){
  kazalniki <- read_xlsx("podatki/kazalniki1.xlsx", sheet=1)
  names(kazalniki) <- c("Obcina", "gostota_prebivalstva", "pov_mes_neto_placa_indeks", "st_reg_brezposelnosti_ods", "st_stan_na_tisoc_preb")
  return(kazalniki)}

#zapis podatkov v razpredelnico kazalniki in kazalniki_za_zemljevide
kazalniki <- uvozi.kazalnike()
kazalniki_za_zemljevide <- uvozi.kazalnike()

#OBCINE

#uvoz zemljevida obcin
source("https://raw.githubusercontent.com/jaanos/APPR-2019-20/master/lib/uvozi.zemljevid.r")
#obcine <- uvozi.zemljevid("http://baza.fmf.uni-lj.si/OB.zip", "OB",
             #             pot.zemljevida="OB", encoding="Windows-1250")

