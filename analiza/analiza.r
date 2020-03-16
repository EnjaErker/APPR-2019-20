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








podatki <- obcine %>% transmute(obcina, povrsina, gostota,
                                gostota.naselij=naselja/povrsina) %>%
  left_join(povprecja, by="obcina")
row.names(podatki) <- podatki$obcina
podatki$obcina <- NULL

# Število skupin
n <- 5
skupine <- hclust(dist(scale(podatki))) %>% cutree(n)
