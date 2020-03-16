# 4. faza: Analiza podatkov

#KAZALNIKI:

#funkcija za sortiranje občin glede na gostoto
fun1 <- function(x) if (x<261) {'zelo redka'} else if (x<521){'redka'} else if (x<780){'gosta'} else {'zelo gosta'}

#uporaba fun1 na podatkih za kazalnike,dodani stolpec za razdelitev občin v različne skupine glede na gostoto prebivalstva (zelo gosto(780-1045), gosto(521-780), redko(261-520), zelo redko(0-260))
kazalniki$`Gostota prebivalstva` <- mapply(fun1, kazalniki$`Gostota prebivalstva (preb/km2)`)

#funkcija za sortiranjem občin glede na stopnjo registrirane brezposelnosti
fun2 <- function(x) if (x<12.3) {'ne kritična'} else {'kritična'}

#uporaba fun2 na podatkih za kazalnike, dodani stolpec za razdelitev občin v dva kategoriji (nekritična, kritična)
kazalniki$`Kritičnost brezposelnosti` <- mapply(fun2, kazalniki$`Stopnja registrirane brezposelnosti (odstotek)`)

#funkcija za sortiranje občin glede na višino povprečne mesečne neto plače oziroma indeksa (v tri skupine)
fun3 <- function(x) if (x<80) {'1.'} else if (x<90) {'2.'} else {'3.'} 

#uporaba fun3 na podatkih za kazalnike, dodani stolpec za razdelitev občin v tri kategorije (1. nizka in 3. visoka)
kazalniki$`Uvrstitev glede na povprečno mesečno neto plačo (indeks)(1. nizka, 3. visoka)` <- mapply(fun3, kazalniki$`Povprečna mesečna neto plača (indeks)`)

#iskanje občin, ki iztopajo glede na gostoto prebivalstva

UPORABA FUNKCIJ SELECT IN FILTER!!!!!!!!!!!!!  

#iskanje občin, ki iztopajo glede na povprečno mesečno neto plačo

#iskanje občin, ki iztopajo glede na stopnjo registrirane brezposelnosti

#iskanje občin, ki iztopajo glede na število stanovanj


#POMANKLJIVOSTI:
 
#izračun deležev stanovanj z vsaj eno pomankljivostjo (nov stolpec: Okviren delež stanovanj s pomankljivostmi)
pomankljivosti[, "Okviren delež stanovanj s pomankljivostmi (v %)"] <- round((pomankljivosti$'Število vseh stanovanj' - (apply(pomankljivosti[, 3:6], 1, max)))/(pomankljivosti$`Število vseh stanovanj`)*100,0)

#ocenitev kritičnosti situacije glede na slovensko povprečje
povprečje <- mean(pomankljivosti$'Okviren delež stanovanj s pomankljivostmi (v %)')
pomankljivosti$`Kritičnost situacije (glede na slovensko povprečje)` <- ifelse(pomankljivosti$'Okviren delež stanovanj s pomankljivostmi (v %)'<povprečje,"Ne kritična","Kritična")

#funkcija za sortiranjem občin glede na pomankljivosti v opremljenosti stanovanj
fun4 <- function(x) if (x<25) {'zelo nizka'} else if (x<50) {'nizka'} else if (x<75) {'visoka'} else {'zelo visoka'}

#uporaba fun4 na podatkih za pomankljivosti, dodani stolpec za razdelitev občin v štiri kategoriji (zelo nizka, nizka, visoka, zelo visoka)
pomankljivosti$`Pomankljivost opremljenosti stanovanj` <- mapply(fun4, pomankljivosti$`Okviren delež stanovanj s pomankljivostmi (v %)`)

#iskanje občin, ki iztopajo glede na število vseh stanovanj 

#iskanje občin, ki iztopajo glede na število stanovanj brez centralnega ogrevanja

#iskanje občin, ki iztopajo glede na število stanovanj brez vode

#iskanje občin, ki iztopajo glede na število stanovanj brez elektrike

#iskanje občin, ki iztopajo glede na število stanovanj brez priklopa na javno kanalizacijo

#iskanje občin, ki iztopajo glede na višino deleža stanovanj s pomakljivostmi
                                                                               
#NAPELJAVE

#dodatek novih stolpcev (Sprememba od 2002 do 2008,Najvišji skok (leti najvišjega skoka))
vodovod$'Sprememba od 2002 do 2008' <- (vodovod$'2008' - vodovod$'2002')
kanalizacija$'Sprememba od 2002 do 2008' <- (kanalizacija$'2008' - kanalizacija$'2002')
električni_tok$'Sprememba od 2002 do 2008' <- (električni_tok$'2008' - električni_tok$'2002')
centralno_ogrevanje$'Sprememba od 2002 do 2008' <- (centralno_ogrevanje$'2008' - centralno_ogrevanje$'2002')
kopalnica$'Sprememba od 2002 do 2008' <- (kopalnica$'2008' - kopalnica$'2002')
stranišče$'Sprememba od 2002 do 2008' <- (stranišče$'2008' - stranišče$'2002')
kuhinja$'Sprememba od 2002 do 2008' <- (kuhinja$'2008' - kuhinja$'2002')
                     
#določanje gostote (dodatek stolpcev z gostoto, sortiranje občin glede na gostoto: uporaba fun1) 
gostota_preb <- kazalniki[,2]
nova_gostota <- gostota_preb[c(-1,-3,-4,-23,-42,-44,-50,-51,-60,-66,-69,-71,-87,-95,-97,-101,-102,-104,-107,-110,-117,-127,-141,-142,-143,-158,-159,-161,-163,-166,-167,-174,-179,-190,-194,-196,-204)]
kanalizacija$'Gostota prebivalstva (preb/km)' <- nova_gostota
vodovod$'Gostota prebivalstva (preb/km)' <- nova_gostota
električni_tok$'Gostota prebivalstva (preb/km)' <- nova_gostota
centralno_ogrevanje$'Gostota prebivalstva (preb/km)' <- nova_gostota
kopalnica$'Gostota prebivalstva (preb/km)' <- nova_gostota
stranišče$'Gostota prebivalstva (preb/km)' <- nova_gostota
kuhinja$'Gostota prebivalstva (preb/km)' <- nova_gostota
kanalizacija$`Gostota` <- mapply(fun1, kanalizacija$`Gostota prebivalstva (preb/km)`)
vodovod$`Gostota` <- mapply(fun1, kanalizacija$`Gostota prebivalstva (preb/km)`)
električni_tok$`Gostota` <- mapply(fun1, kanalizacija$`Gostota prebivalstva (preb/km)`)
centralno_ogrevanje$`Gostota` <- mapply(fun1, kanalizacija$`Gostota prebivalstva (preb/km)`)
kopalnica$`Gostota` <- mapply(fun1, kanalizacija$`Gostota prebivalstva (preb/km)`)
stranišče$`Gostota` <- mapply(fun1, kanalizacija$`Gostota prebivalstva (preb/km)`)
kuhinja$`Gostota` <- mapply(fun1, kanalizacija$`Gostota prebivalstva (preb/km)`)

#izdelava manjših podtabel za lažje risanje grafov za posamezno napeljavo
st_zelo_redkih <- sum(kanalizacija$'Gostota' == 'zelo redka')
st_redkih <- sum(kanalizacija$'Gostota' == 'redka')
st_gostih <- sum(kanalizacija$'Gostota' == 'gosta')
st_zelo_gostih <- sum(kanalizacija$'Gostota' == 'zelo gosta')
Gostota <- c("Zelo redka", "Redka","Gosta","Zelo gosta")
Število <- c(st_zelo_redkih, st_redkih, st_gostih, st_zelo_gostih)
kanalizacija_sort <- data.table(Gostota,Število)
vodovod_sort <- data.table(Gostota,Število)
električni_tok_sort <- data.table(Gostota,Število)
centralno_ogrevanje_sort <- data.table(Gostota,Število)
kopalnica_sort <- data.table(Gostota,Število)
stranišče_sort <- data.table(Gostota,Število)
kuhinja_sort <- data.table(Gostota,Število)

#sestavitev ene tabele za kanalizacijo iz dobljenih stolpcev
dva <- aggregate(kanalizacija$'2002', by=list(Gostota=kanalizacija$Gostota), FUN=sum)
tri <- aggregate(kanalizacija$'2003', by=list(Gostota=kanalizacija$Gostota), FUN=sum)
štiri <- aggregate(kanalizacija$'2004', by=list(Gostota=kanalizacija$Gostota), FUN=sum)
pet <- aggregate(kanalizacija$'2005', by=list(Gostota=kanalizacija$Gostota), FUN=sum)
šest <- aggregate(kanalizacija$'2006', by=list(Gostota=kanalizacija$Gostota), FUN=sum)
sedem <- aggregate(kanalizacija$'2007', by=list(Gostota=kanalizacija$Gostota), FUN=sum)
osem <- aggregate(kanalizacija$'2008', by=list(Gostota=kanalizacija$Gostota), FUN=sum)

kanalizacija_sort$'2002' <- dva[2] 
kanalizacija_sort$'2003' <- tri[2]
kanalizacija_sort$'2004' <- štiri[2]
kanalizacija_sort$'2005' <- pet[2]
kanalizacija_sort$'2006' <- šest[2]
kanalizacija_sort$'2007' <- sedem[2]
kanalizacija_sort$'2008' <- osem[2]

#sestavitev ene tabele za vodovod
dva1 <- aggregate(vodovod$'2002', by=list(Gostota=vodovod$Gostota), FUN=sum)
tri1 <- aggregate(vodovod$'2003', by=list(Gostota=vodovod$Gostota), FUN=sum)
štiri1 <- aggregate(vodovod$'2004', by=list(Gostota=vodovod$Gostota), FUN=sum)
pet1 <- aggregate(vodovod$'2005', by=list(Gostota=vodovod$Gostota), FUN=sum)
šest1 <- aggregate(vodovod$'2006', by=list(Gostota=vodovod$Gostota), FUN=sum)
sedem1 <- aggregate(vodovod$'2007', by=list(Gostota=vodovod$Gostota), FUN=sum)
osem1 <- aggregate(vodovod$'2008', by=list(Gostota=vodovod$Gostota), FUN=sum)

vodovod_sort$'2002' <- dva1[2]
vodovod_sort$'2003' <- tri1[2]
vodovod_sort$'2004' <- štiri1[2]
vodovod_sort$'2005' <- pet1[2]
vodovod_sort$'2006' <- šest1[2]
vodovod_sort$'2007' <- sedem1[2]
vodovod_sort$'2008' <- osem1[2]

#sestavitev ene tabele za električni tok
dva2 <- aggregate(električni_tok$'2002', by=list(Gostota=električni_tok$Gostota), FUN=sum)
tri2 <- aggregate(električni_tok$'2003', by=list(Gostota=električni_tok$Gostota), FUN=sum)
štiri2 <- aggregate(električni_tok$'2004', by=list(Gostota=električni_tok$Gostota), FUN=sum)
pet2 <- aggregate(električni_tok$'2005', by=list(Gostota=električni_tok$Gostota), FUN=sum)
šest2 <- aggregate(električni_tok$'2006', by=list(Gostota=električni_tok$Gostota), FUN=sum)
sedem2 <- aggregate(električni_tok$'2007', by=list(Gostota=električni_tok$Gostota), FUN=sum)
osem2 <- aggregate(električni_tok$'2008', by=list(Gostota=električni_tok$Gostota), FUN=sum)

električni_tok_sort$'2002' <- dva2[2]
električni_tok_sort$'2003' <- tri2[2]
električni_tok_sort$'2004' <- štiri2[2]
električni_tok_sort$'2005' <- pet2[2]
električni_tok_sort$'2006' <- šest2[2]
električni_tok_sort$'2007' <- sedem2[2]
električni_tok_sort$'2008' <- osem2[2]

#sestavitev ene tabele za centralno ogrevanje
dva3 <- aggregate(centralno_ogrevanje$'2002', by=list(Gostota=centralno_ogrevanje$Gostota), FUN=sum)
tri3 <- aggregate(centralno_ogrevanje$'2003', by=list(Gostota=centralno_ogrevanje$Gostota), FUN=sum)
štiri3 <- aggregate(centralno_ogrevanje$'2004', by=list(Gostota=centralno_ogrevanje$Gostota), FUN=sum)
pet3 <- aggregate(centralno_ogrevanje$'2005', by=list(Gostota=centralno_ogrevanje$Gostota), FUN=sum)
šest3 <- aggregate(centralno_ogrevanje$'2006', by=list(Gostota=centralno_ogrevanje$Gostota), FUN=sum)
sedem3 <- aggregate(centralno_ogrevanje$'2007', by=list(Gostota=centralno_ogrevanje$Gostota), FUN=sum)
osem3 <- aggregate(centralno_ogrevanje$'2008', by=list(Gostota=centralno_ogrevanje$Gostota), FUN=sum)

centralno_ogrevanje_sort$'2002' <- dva3[2]
centralno_ogrevanje_sort$'2003' <- tri3[2]
centralno_ogrevanje_sort$'2004' <- štiri3[2]
centralno_ogrevanje_sort$'2005' <- pet3[2]
centralno_ogrevanje_sort$'2006' <- šest3[2]
centralno_ogrevanje_sort$'2007' <- sedem3[2]
centralno_ogrevanje_sort$'2008' <- osem3[2]

#sestavitev ene tabele za kopalnico
dva4 <- aggregate(kopalnica$'2002', by=list(Gostota=kopalnica$Gostota), FUN=sum)
tri4 <- aggregate(kopalnica$'2003', by=list(Gostota=kopalnica$Gostota), FUN=sum)
štiri4 <- aggregate(kopalnica$'2004', by=list(Gostota=kopalnica$Gostota), FUN=sum)
pet4 <- aggregate(kopalnica$'2005', by=list(Gostota=kopalnica$Gostota), FUN=sum)
šest4 <- aggregate(kopalnica$'2006', by=list(Gostota=kopalnica$Gostota), FUN=sum)
sedem4 <- aggregate(kopalnica$'2007', by=list(Gostota=kopalnica$Gostota), FUN=sum)
osem4 <- aggregate(kopalnica$'2008', by=list(Gostota=kopalnica$Gostota), FUN=sum)

kopalnica_sort$'2002' <- dva4[2]
kopalnica_sort$'2003' <- tri4[2]
kopalnica_sort$'2004' <- štiri4[2]
kopalnica_sort$'2005' <- pet4[2]
kopalnica_sort$'2006' <- šest4[2]
kopalnica_sort$'2007' <- sedem4[2]
kopalnica_sort$'2008' <- osem4[2]

#sestavitev ene tabele za stranišče
dva5 <- aggregate(stranišče$'2002', by=list(Gostota=stranišče$Gostota), FUN=sum)
tri5 <- aggregate(stranišče$'2003', by=list(Gostota=stranišče$Gostota), FUN=sum)
štiri5 <- aggregate(stranišče$'2004', by=list(Gostota=stranišče$Gostota), FUN=sum)
pet5 <- aggregate(stranišče$'2005', by=list(Gostota=stranišče$Gostota), FUN=sum)
šest5 <- aggregate(stranišče$'2006', by=list(Gostota=stranišče$Gostota), FUN=sum)
sedem5 <- aggregate(stranišče$'2007', by=list(Gostota=stranišče$Gostota), FUN=sum)
osem5 <- aggregate(stranišče$'2008', by=list(Gostota=stranišče$Gostota), FUN=sum)

stranišče_sort$'2002' <- dva5[2]
stranišče_sort$'2003' <- tri5[2]
stranišče_sort$'2004' <- štiri5[2]
stranišče_sort$'2005' <- pet5[2]
stranišče_sort$'2006' <- šest5[2]
stranišče_sort$'2007' <- sedem5[2]
stranišče_sort$'2008' <- osem5[2]

#sestavitev ene tabele za kuhinjo
dva6 <- aggregate(kuhinja$'2002', by=list(Gostota=kuhinja$Gostota), FUN=sum)
tri6 <- aggregate(kuhinja$'2003', by=list(Gostota=kuhinja$Gostota), FUN=sum)
štiri6 <- aggregate(kuhinja$'2004', by=list(Gostota=kuhinja$Gostota), FUN=sum)
pet6 <- aggregate(kuhinja$'2005', by=list(Gostota=kuhinja$Gostota), FUN=sum)
šest6 <- aggregate(kuhinja$'2006', by=list(Gostota=kuhinja$Gostota), FUN=sum)
sedem6 <- aggregate(kuhinja$'2007', by=list(Gostota=kuhinja$Gostota), FUN=sum)
osem6 <- aggregate(kuhinja$'2008', by=list(Gostota=kuhinja$Gostota), FUN=sum)

kuhinja_sort$'2002' <- dva6[2]
kuhinja_sort$'2003' <- tri6[2]
kuhinja_sort$'2004' <- štiri6[2]
kuhinja_sort$'2005' <- pet6[2]
kuhinja_sort$'2006' <- šest6[2]
kuhinja_sort$'2007' <- sedem6[2]
kuhinja_sort$'2008' <- osem6[2]







podatki <- obcine %>% transmute(obcina, povrsina, gostota,
                                gostota.naselij=naselja/povrsina) %>%
  left_join(povprecja, by="obcina")
row.names(podatki) <- podatki$obcina
podatki$obcina <- NULL

# Število skupin
n <- 5
skupine <- hclust(dist(scale(podatki))) %>% cutree(n)
