# VIZUALIZACIJA

#KAZALNIKI, TORTNI DIAGRAMI

#graficni prikaz gostote prebivalstva v Sloveniji 
st_zelo_redkih <- sum(gostota$'skupina' == 'zelo_redka')
st_redkih <- sum(gostota$'skupina' == 'redka')
st_gostih <- sum(gostota$'skupina' == 'gosta')
st_zelo_gostih <- sum(gostota$'skupina' == 'zelo_gosta')
Število <- c(st_zelo_redkih, st_redkih, st_gostih, st_zelo_gostih)
Skupina <- c("zelo redka","redka","gosta","zelo gosta")
gostota_zag <- data.table(Skupina, Število)

prikaz_gostote_preb <- ggplot(gostota_zag, aes(x="", y=Število, fill=Skupina)) + geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + scale_fill_manual(values=c("#55DDE0", "#33658A", "#2F4858", "#F6AE2D")) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Gostota prebivalstva v Sloveniji leta 2015") +
  theme_classic() + theme(axis.line = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(),plot.title = element_text(hjust = 0.5, color = "#666666")) +
  geom_text(aes(label = paste(round(Število / sum(Število) * 100, 1), "%"), x = 1.3),position = position_stack(vjust = 0.5))
plot(prikaz_gostote_preb)

#graficni prikaz kriticnosti brezposelnosti v Sloveniji (izračun kritičnih in nekritičnih, nov data table s podatki, izris tortnega diagrama)
st_kriticnih <- sum(brezposelni$'kriticnost' == 'kritična')
st_nekriticnih <- sum(brezposelni$'kriticnost' == 'ne kritična')
Kritičnost <- c("kritična","ne kritična")
Število <- c(st_kriticnih, st_nekriticnih)
kriticnost <- data.table(Kritičnost, Število)

prikaz_kriticnosti_brezposelnosti <- ggplot(kriticnost, aes(x="", y=Število, fill=Kritičnost)) + geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + scale_fill_manual(values=c("#F26419", "#999999")) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Kritičnost brezposelnosti v Sloveniji leta 2015") +
  theme_classic() + theme(axis.line = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(),plot.title = element_text(hjust = 0.5, color = "#666666")) +
  geom_text(aes(label = paste(round(Število / sum(Število) * 100, 1), "%"), x = 1.3),position = position_stack(vjust = 0.5))
plot(prikaz_kriticnosti_brezposelnosti)

#graficni prikaz povprečne mesečne neto plače (indeks) v Sloveniji (glede na delitev na tri skupine) ((izračun 'razredov', nov data table s podatki, izris tortnega diagrama))
prva <- sum(placa_indeks$'razred' == '1.')
druga <- sum(placa_indeks$'razred' == '2.')
tretja <- sum(placa_indeks$'razred' == '3.')
Višina <- c("nizka", "srednja", "visoka")
Število <- c(prva, druga, tretja)
place <- data.table(Višina, Število)

prikaz_povprecne_mesecne_neto_place <- ggplot(place, aes(x="", y=Število, fill=Višina)) + geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + scale_fill_manual(values=c("#2F4858", "#F6AE2D", "#999999")) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Povprečne mesečno neto plače v Sloveniji glede na indeks leta 2015") +
  theme_classic() + theme(axis.line = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(),plot.title = element_text(hjust = 0.5, color = "#666666"))+
  geom_text(aes(label = paste(round(Število / sum(Število) * 100, 1), "%"), x = 1.3),position = position_stack(vjust = 0.5))
plot(prikaz_povprecne_mesecne_neto_place)

#NAPELJAVE, GRAFI

#graficni prikaz vodovoda in gostote
graf1 <- ggplot(leta.skupine.vod) + aes(x=leto, y=povprecje, color=skupina) + geom_line() + geom_point() +
  labs(x="Leto",y="Povprečno število stanovanj z vodovodom",title="Število vodovodnih napeljav glede na gostoto poselitve med 2002 in 2010", color = "Gostota poselitve") +
  theme(legend.position = "bottom") 
plot(graf1)

#graficni prikaz kanalizacije in gostote
graf2 <- ggplot(leta.skupine.kan) + aes(x=leto, y=povprecje, color=skupina) + geom_line() + geom_point() +
  labs(x="Leto",y="Povprečno število stanovanj s kanalizacijo",title="Število napeljav kanalizacije glede na gostoto poselitve med 2002 in 2010", color = "Gostota poselitve") +
  theme(legend.position = "bottom") 
plot(graf2)

#graficni prikaz plac in ogrevanja
graf3 <- ggplot(leta.razredi) + aes(x=leto, y=povprecje, color=razred) + geom_line() + geom_point() +
  labs(x="Leto",y="Povprečno število stanovanj z ogrevanjem",title="Število ogrevalnih napeljav glede na plačni razred prebivalstva med 2002 in 2010", color = "Plačni razred") +
  theme(legend.position = "bottom") 
plot(graf3)

#graficni prikaz stanovanj in kuhinj
graf4 <- ggplot(leta.st_stan) + aes(x=leto, y=povprecje, color=st_stan) + geom_line() + geom_point() +
  labs(x="Leto",y="Povprečno število stanovanj s kuhinjo",title="Število kuhinj glede na število stanovanj med 2002 in 2010", color = "Število stanovanj") +
  theme(legend.position = "bottom") 
plot(graf4)

#POMANKLJIVOSTI, TORTNI PRIKAZ

#graficni prikaz pomankljivo opremljenih stanovanj v Sloveniji
st_zelo_nizkih <- sum(delezi$'pomankljivost' == 'zelo nizka')
st_nizkih <- sum(delezi$'pomankljivost' == 'nizka')
st_visokih <- sum(delezi$'pomankljivost' == 'visoka')
st_zelo_visokih <- sum(delezi$'pomankljivost' == 'zelo visoka')
Stopnja <- c("zelo nizka", "nizka", "visoka", "zelo visoka")
Število <- c(st_zelo_nizkih, st_nizkih, st_visokih, st_zelo_visokih)
pomankljivi <- data.table(Stopnja, Število) 

prikaz_pomankljivo_opremljenih_stanovanj <- ggplot(pomankljivi, aes(x="", y=Število, fill=Stopnja)) + geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73")) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Pomankljivost opremljenosti stanovanj v Sloveniji") +
  theme_classic() + theme(axis.line = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(),plot.title = element_text(hjust = 0.5, color = "#666666")) +
  geom_text(aes(label = paste(round(Število / sum(Število) * 100, 1), "%"), x = 1.3),position = position_stack(vjust = 0.5))
plot(prikaz_pomankljivo_opremljenih_stanovanj)

#KAZALNIKI, ZEMLJEVIDI

#gostota prebivalstva
tm_shape(merge(obcine, gostota_z, by.x="OB_UIME", by.y="Obcina")) + tm_polygons("vrednost") +
  tm_layout(title="Gostota prebivalstva Slovenije po občinah 2015", legend.position=c(0.82,0.005), legend.height=0.7, title.size=20)
#+ tm_add_legend(title="Gostota naseljenosti v preb/km^2")

#place
tm_shape(merge(obcine, placa_indeks_z, by.x="OB_UIME", by.y="Obcina")) + tm_polygons("vrednost") + 
  tm_layout(title="Povprečne mesečne neto plače v Sloveniji glede na indeks po občinah 2015",legend.position=c(0.82,0.005), legend.height=0.7) 

#brezposelni
tm_shape(merge(obcine, brezposelni_z, by.x="OB_UIME", by.y="Obcina")) + tm_polygons("vrednost") + 
  tm_layout(title="Število brezposelnih (indeks) v Sloveniji po občinah v letu 2015",legend.position=c(0.82,0.005), legend.height=0.7) 

#stevilo stanovanj na 1000 prebivalcev
tm_shape(merge(obcine, stanovanja_z, by.x="OB_UIME", by.y="Obcina")) + tm_polygons("vrednost") + 
  tm_layout(title="Število stanovanj na 1000 prebivalcev v Sloveniji po občinah v letu 2015",legend.position=c(0.82,0.005), legend.height=0.7) 
