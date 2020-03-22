
# 3. faza: Vizualizacija podatkov

#KAZALNIKI

#graficni prikaz gostote prebivalstva v Sloveniji 
prikaz_gostote_preb <- ggplot(gostota, aes(x="", y=vrednost, fill=gostota)) + geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  scale_fill_manual(values=c("#55DDE0", "#33658A", "#2F4858", "#F6AE2D")) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Gostota prebivalstva v Sloveniji") +
  theme_classic() + theme(axis.line = element_blank(),
                          axis.text = element_blank(),
                          axis.ticks = element_blank(),
                          plot.title = element_text(hjust = 0.5, color = "#666666"))

plot(prikaz_gostote_preb)

#graficni prikaz kriticnosti brezposelnosti v Sloveniji (izračun kritičnih in nekritičnih, nov data table s podatki, izris tortnega diagrama)
st_kriticnih <- sum(kazalniki$'Kritičnost brezposelnosti' == 'kritična')
st_nekriticnih <- sum(kazalniki$'Kritičnost brezposelnosti' == 'ne kritična')

Kritičnost <- c("Kritična","Ne kritična")
Število <- c(st_kriticnih, st_nekriticnih)
pct <- round((Število/sum(Število)*100),2)

kriticnost <- data.table(Kritičnost, Število)


prikaz_kriticnosti_brezposelnosti <- ggplot(kriticnost, aes(x="", y=Število, fill=Kritičnost)) + geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  scale_fill_manual(values=c("#F26419", "#999999")) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Kritičnost brezposelnosti v Sloveniji") +
  theme_classic() + theme(axis.line = element_blank(),
                          axis.text = element_blank(),
                          axis.ticks = element_blank(),
                          plot.title = element_text(hjust = 0.5, color = "#666666"))

plot(prikaz_kriticnosti_brezposelnosti)

#graficni prikaz povprečne mesečne neto plače (indeks) v Sloveniji (glede na delitev na tri skupine) ((izračun 'razredov', nov data table s podatki, izris tortnega diagrama))
prva <- sum(kazalniki$'Uvrstitev glede na povprečno mesečno neto plačo (indeks)(1. nizka, 3. visoka)' == '1.')
druga <- sum(kazalniki$'Uvrstitev glede na povprečno mesečno neto plačo (indeks)(1. nizka, 3. visoka)' == '2.')
tretja <- sum(kazalniki$'Uvrstitev glede na povprečno mesečno neto plačo (indeks)(1. nizka, 3. visoka)' == '3.')

Višina <- c("Nizka", "Srednja", "Visoka")
Število <- c(prva, druga, tretja)
place <- data.table(Višina, Število)
pct <- round((Število/sum(Število)*100),2)

prikaz_povprecne_mesecne_neto_place <- ggplot(place, aes(x="", y=Število, fill=Višina)) + geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  scale_fill_manual(values=c("#2F4858", "#F6AE2D", "#999999")) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Povprečne mesečno neto plače v Sloveniji glede na indeks") +
  theme_classic() + theme(axis.line = element_blank(),
                          axis.text = element_blank(),
                          axis.ticks = element_blank(),
                          plot.title = element_text(hjust = 0.5, color = "#666666"))

plot(prikaz_povprecne_mesecne_neto_place)

#POMANKLJIVOSTI

#graficni prikaz pomankljivo opremljenih stanovanj v Sloveniji (razporeditev v skupine, nov data table z ustreznimi podatki, izris tortnega diagrama)
st_zelo_nizkih <- sum(pomankljivosti$'Pomankljivost opremljenosti stanovanj' == 'zelo nizka')
st_nizkih <- sum(pomankljivosti$'Pomankljivost opremljenosti stanovanj' == 'nizka')
st_visokih <- sum(pomankljivosti$'Pomankljivost opremljenosti stanovanj' == 'visoka')
st_zelo_visokih <- sum(pomankljivosti$'Pomankljivost opremljenosti stanovanj' == 'zelo visoka')

Stopnja <- c("Zelo nizka", "Nizka", "Visoka", "Zelo visoka")
Število <- c(st_zelo_nizkih, st_nizkih, st_visokih, st_zelo_visokih)
pct <- round((Število/sum(Število)*100),2)

pomankljivi <- data.table(Stopnja, Število) 

prikaz_pomankljivo_opremljenih_stanovanj <- ggplot(pomankljivi, aes(x="", y=Število, fill=Stopnja)) + geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73")) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Pomankljivost opremljenosti stanovanj v Sloveniji") +
  theme_classic() + theme(axis.line = element_blank(),
                          axis.text = element_blank(),
                          axis.ticks = element_blank(),
                          plot.title = element_text(hjust = 0.5, color = "#666666")) 

plot(prikaz_pomankljivo_opremljenih_stanovanj)

#NAPELJAVE

#graficni prikaz vodovoda in gostote
graf1 <- ggplot(leta.skupine.vod) + aes(x=leto, y=povprecje, color=skupina) + geom_line() + geom_point() +
  labs(x="Leto",y="Povprečno število stanovanj z vodovodom",title="Spremembe v številu vodovodnih napeljav glede na gostoto poselitve", color = "Gostota poselitve") +
  theme(legend.position = "bottom") 

#graficni prikaz kanalizacije in gostote
graf2 <- ggplot(leta.skupine.kan) + aes(x=leto, y=povprecje, color=skupina) + geom_line() + geom_point() +
  labs(x="Leto",y="Povprečno število stanovanj s kanalizacijo",title="Spremembe v številu napeljav kanalizacije glede na gostoto poselitve", color = "Gostota poselitve") +
  theme(legend.position = "bottom") 

#graficni prikaz plac in ogrevanja
graf3 <- ggplot(leta.razredi) + aes(x=leto, y=povprecje, color=razred) + geom_line() + geom_point() +
  labs(x="Leto",y="Povprečno število stanovanj z ogrevanjem",title="Spremembe v številu ogrevalnih napeljav glede na plačni razred prebivalstva", color = "Plačni razred") +
  theme(legend.position = "bottom") 

#graficni prikaz stanovanj in kuhinj
graf4 <- ggplot(leta.st_stan) + aes(x=leto, y=povprecje, color=st_stan) + geom_line() + geom_point() +
  labs(x="Leto",y="Povprečno število stanovanj s kuhinjo",title="Spremembe v številu kuhinj glede na število stanovanj", color = "Število stanovanj") +
  theme(legend.position = "bottom") 











# Uvozimo zemljevid.
zemljevid <- uvozi.zemljevid("http://baza.fmf.uni-lj.si/OB.zip", "OB",
                             pot.zemljevida="OB", encoding="Windows-1250")
levels(zemljevid$OB_UIME) <- levels(zemljevid$OB_UIME) %>%
  { gsub("Slovenskih", "Slov.", .) } %>% { gsub("-", " - ", .) }
zemljevid$OB_UIME <- factor(zemljevid$OB_UIME, levels=levels(obcine$obcina))
zemljevid <- fortify(zemljevid)

# Izračunamo povprečno velikost družine
povprecja <- druzine %>% group_by(obcina) %>%
  summarise(povprecje=sum(velikost.druzine * stevilo.druzin) / sum(stevilo.druzin))
