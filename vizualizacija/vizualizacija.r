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

t1 <- ggplot(gostota_zag, aes(x="", y=Število, fill=Skupina)) + geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + scale_fill_manual(values=c("#55DDE0", "#33658A", "#2F4858", "#F6AE2D")) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Gostota prebivalstva v Sloveniji leta 2015") +
  theme_classic() + theme(axis.line = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(),plot.title = element_text(hjust = 0.5, color = "#666666")) +
  geom_text(aes(label = paste(round(Število / sum(Število) * 100, 1), "%"), x = 1.3),position = position_stack(vjust = 0.5))
plot(t1)

#graficni prikaz kriticnosti brezposelnosti v Sloveniji (izračun kritičnih in nekritičnih, nov data table s podatki, izris tortnega diagrama)
st_kriticnih <- sum(brezposelni$'kriticnost' == 'kritična')
st_nekriticnih <- sum(brezposelni$'kriticnost' == 'nekritična')
Kritičnost <- c("kritična","nekritična")
Število <- c(st_kriticnih, st_nekriticnih)
kriticnost <- data.table(Kritičnost, Število)

t2 <- ggplot(kriticnost, aes(x="", y=Število, fill=Kritičnost)) + geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + scale_fill_manual(values=c("#F26419", "#999999")) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Kritičnost brezposelnosti v Sloveniji leta 2015") +
  theme_classic() + theme(axis.line = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(),plot.title = element_text(hjust = 0.5, color = "#666666")) +
  geom_text(aes(label = paste(round(Število / sum(Število) * 100, 1), "%"), x = 1.3),position = position_stack(vjust = 0.5))
plot(t2)

#graficni prikaz povprečne mesečne neto plače (indeks) v Sloveniji (glede na delitev na tri skupine) ((izračun 'razredov', nov data table s podatki, izris tortnega diagrama))
prva <- sum(placa_indeks$'razred' == 'nizka')
druga <- sum(placa_indeks$'razred' == 'srednja')
tretja <- sum(placa_indeks$'razred' == 'visoka')
Višina <- c("nizka", "srednja", "visoka")
Število <- c(prva, druga, tretja)
place <- data.table(Višina, Število)

t3 <- ggplot(place, aes(x="", y=Število, fill=Višina)) + geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + scale_fill_manual(values=c("#2F4858", "#F6AE2D", "#999999")) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Povprečne mesečne neto plače na ravni občin glede na slovensko povprečje leta 2015 po skupinah") +
  theme_classic() + theme(axis.line = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(),plot.title = element_text(hjust = 0.5, color = "#666666"))+
  geom_text(aes(label = paste(round(Število / sum(Število) * 100, 1), "%"), x = 1.3),position = position_stack(vjust = 0.5))
plot(t3)

#NAPELJAVE, GRAFI

#graficni prikaz vodovoda in gostote
g1 <- ggplot(leta.skupine.vod) + aes(x=leto, y=povprecje, color=skupina) + geom_line() + geom_point() +
  labs(x="Leto",y="Povprečno število stanovanj z vodovodom",title="Število vodovodnih napeljav glede na gostoto poselitve, 2002 - 2010", color = "Gostota poselitve") +
  theme(legend.position = "bottom") 
plot(g1)

#graficni prikaz kanalizacije in gostote
g2 <- ggplot(leta.skupine.kan) + aes(x=leto, y=povprecje, color=skupina) + geom_line() + geom_point() +
  labs(x="Leto",y="Povprečno število stanovanj s kanalizacijo",title="Število napeljav kanalizacije glede na gostoto poselitve, 2002 - 2010", color = "Gostota poselitve") +
  theme(legend.position = "bottom") 
plot(g2)

#graficni prikaz plac in ogrevanja
g3 <- ggplot(leta.razredi) + aes(x=leto, y=povprecje, color=razred) + geom_line() + geom_point() +
  labs(x="Leto",y="Povprečno število stanovanj z ogrevanjem",title="Število ogrevalnih napeljav in plače prebivalstva, 2002 - 2010", color = "Povprečna višina plač") +
  theme(legend.position = "bottom") 
plot(g3)

#POMANKLJIVOSTI, TORTNI PRIKAZ

#graficni prikaz pomankljivo opremljenih stanovanj v Sloveniji
st_zelo_nizkih <- sum(delezi$'pomankljivost' == 'zelo nizka')
st_nizkih <- sum(delezi$'pomankljivost' == 'nizka')
st_visokih <- sum(delezi$'pomankljivost' == 'visoka')
st_zelo_visokih <- sum(delezi$'pomankljivost' == 'zelo visoka')
Stopnja <- c("zelo nizka", "nizka", "visoka", "zelo visoka")
Število <- c(st_zelo_nizkih, st_nizkih, st_visokih, st_zelo_visokih)
pomankljivi <- data.table(Stopnja, Število) 

tr <- ggplot(pomankljivi, aes(x="", y=Število, fill=Stopnja)) + geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73")) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Delež občin s pomankljivo opremljenostjo stanovanj leta 2015") +
  theme_classic() + theme(axis.line = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(),plot.title = element_text(hjust = 0.5, color = "#666666")) +
  geom_text(aes(label = paste(round(Število / sum(Število) * 100, 1), "%"), x = 1.3),position = position_stack(vjust = 0.5))
plot(tr)

#KAZALNIKI, ZEMLJEVIDI

#gostota prebivalstva
z1 <- tm_shape(merge(obcine, gostota_z, by.x="OB_UIME", by.y="Obcina")) + tm_polygons("vrednost", title="Gostota naseljenosti v preb/km^2") +
  tm_layout(title="Gostota prebivalstva Slovenije po občinah leta 2015", legend.position=c(0.82,0.005), legend.height=0.7, title.size=1.5)

#place
z2 <- tm_shape(merge(obcine, placa_indeks_z, by.x="OB_UIME", by.y="Obcina")) + tm_polygons("vrednost", title="Vrednost indeksa") + 
  tm_layout(title="Povprečne mesečne neto plače glede na slovensko povprečje (indeks = 100) po občinah leta 2015",legend.position=c(0.82,0.005), legend.height=0.7, title.size=1.5) 

#brezposelni
z3 <- tm_shape(merge(obcine, brezposelni_z, by.x="OB_UIME", by.y="Obcina")) + tm_polygons("vrednost", title="Odstotki") + 
  tm_layout(title="Stopnja registrirane brezposelnosti (v %) po občinah v Sloveniji v letu 2015",legend.position=c(0.82,0.005), legend.height=0.7, title.size=1.5) 

#stevilo stanovanj na 1000 prebivalcev
z4 <- tm_shape(merge(obcine, stanovanja_z, by.x="OB_UIME", by.y="Obcina")) + tm_polygons("vrednost", title="Število stanovanj na 1000 preb.") + 
  tm_layout(title="Število stanovanj na 1000 prebivalcev v Sloveniji po občinah v letu 2015",legend.position=c(0.82,0.005), legend.height=0.7, title.size=1.5) 