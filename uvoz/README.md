# Obdelava, uvoz in čiščenje podatkov.

library(knitr)
library(rvest)
library(gsubfn)
library(tidyr)
library(shiny)

napeljave_2002 <- read_csv2("podatki/napeljave.csv", na="-", locale=locale(encoding="Windows-1250"), col_names=c("obcina", "vodovod", "kanalizacija", "elektricni tok", "centralno ogrevanje", "kopalnica", "stranisce", "kuhinja") )


kazalniki <- read_xlsx("podatki/kazalniki.xlsx", sheet ="List1", na="-", locale=locale(encoding="Windows-1250"))
#dodati se list 2,15,22



napeljave <- read.csv2("podatki/napeljave.csv", na=c("", " ", "...", "-"))
napeljave <- read_csv2("podatki/napeljave.csv", na="-", locale=locale(encoding="Windows-1250"))


st_sob_oseb <- read_json("podatki/st_sob_oseb.json",na="-", locale=locale(encoding="Windows-1250"))

naseljenost_in_pomanjkljivosti <- read_xml("podatki/naseljenost_in_pomanjkljivosti.xml", na="-", locale=locale(encoding="Windows-1250"))


