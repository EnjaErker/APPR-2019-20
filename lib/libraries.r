# KNJIZNJICE

library(knitr)
library(rvest)
library(gsubfn)
library(tidyr)
library(shiny)
library(readr)
library(xlsx)
library(methods)
library(dplyr)
library(ggplot2)
library(data.table)
library(rgeos)
library(mosaic)
library(rgdal)
library(maptools)
library(tmap)

# Uvozimo funkcije za pobiranje in uvoz zemljevida.
#source("lib/uvozi.zemljevid.r",encoding="UTF-8")

#source("https://raw.githubusercontent.com/jaanos/APPR-2019-20/master/lib/uvozi.zemljevid.r")
#obcine <- uvozi.zemljevid("http://baza.fmf.uni-lj.si/OB.zip", "OB",
                          #pot.zemljevida="OB", encoding="Windows-1250")

#tm_shape(obcine) + tm_polygons("OB_UIME") + tm_legend(show=FALSE)

#tm_shape(merge(obcine, gostota, by.x="vrednost", by.y="obcina")) 

#tm_polygons("povprecje")
