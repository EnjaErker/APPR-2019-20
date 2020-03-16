
# 3. faza: Vizualizacija podatkov

#KAZALNIKI

#graficni prikaz gostote prebivalstva v Sloveniji (seštevek posameznih občin glede na lastnost gostote, izdelava novega data tabla gostote, izris tortnega diagrama)
st_zelo_redkih <- sum(kazalniki$'Gostota prebivalstva' == 'zelo redka')
st_redkih <- sum(kazalniki$'Gostota prebivalstva' == 'redka')
st_gostih <- sum(kazalniki$'Gostota prebivalstva' == 'gosta')
st_zelo_gostih <- sum(kazalniki$'Gostota prebivalstva' == 'zelo gosta')

Gostota <- c("Zelo redka", "Redka","Gosta","Zelo gosta")
Število <- c(st_zelo_redkih, st_redkih, st_gostih, st_zelo_gostih)
pct <- round((Število/sum(Število)*100),2)
  
gostota <- data.table(Gostota, Število)

prikaz_gostote_preb <- ggplot(gostota, aes(x="", y=Število, fill=Gostota)) + geom_bar(stat="identity", width=1) +
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

#koeficienti rasti po letih za kuhinjo
g1 <- round((kuhinja_sort$'2003' - kuhinja_sort$'2002')/(kuhinja_sort$'2002')*100,2)
h1 <- round((kuhinja_sort$'2004' - kuhinja_sort$'2003')/(kuhinja_sort$'2003')*100,2)
i1 <- round((kuhinja_sort$'2005' - kuhinja_sort$'2004')/(kuhinja_sort$'2004')*100,2)
j1 <- round((kuhinja_sort$'2006' - kuhinja_sort$'2005')/(kuhinja_sort$'2005')*100,2)
k1 <- round((kuhinja_sort$'2007' - kuhinja_sort$'2006')/(kuhinja_sort$'2006')*100,2)
l1 <- round((kuhinja_sort$'2008' - kuhinja_sort$'2007')/(kuhinja_sort$'2007')*100,2)
koef_rast_kuhinja  <- data.table(Gostota,g1,h1,i1,j1,k1,l1)
names(koef_rast_kuhinja) <- c("Gostota","2003","2004","2005","2006","2007","2008")
koef_rast_kuhinja[4,6] <- 0.4
k1 <- c(k1[1],k1[2],k1[3],0.4)

#grafi: odvisnost koeficientov rasti glede na leto za kuhinje

leto <- c("2003","2004","2005","2006","2007","2008")
st1 <- c(g1[1],h1[1],i1[1],j1[1],k1[1],l1[1])
st2 <- c(g1[2],h1[2],i1[2],j1[2],k1[2],l1[2])
st3 <- c(g1[3],h1[3],i1[3],j1[3],k1[3],l1[3])
st4 <- c(g1[4],h1[4],i1[4],j1[4],k1[4],l1[4])
df <- data.frame(leto,st1,st2,st3,st4)
names(df) <- c("Leto","Zelo_redka","Redka","Gosta","Zelo_gosta")
am <- df[,1:2] 
am$'Grupa' <- c(1,1,1,1,1,1)
bm <- df[,c("Leto","Redka")]
bm$'Grupa' <- c(2,2,2,2,2,2)
cm <- df[,c("Leto","Gosta")]
cm$'Grupa' <- c(3,3,3,3,3,3)
dm <- df[,c("Leto","Zelo_gosta")]
dm$'Grupa' <- c(4,4,4,4,4,4)        

ggplot() +
  geom_line(data=am,aes(x=Leto, y=Zelo_redka, group=1), color="red") +
  geom_line(data=bm,aes(x=Leto, y=Redka, group=2), color="blue") +
  geom_line(data=cm,aes(x=Leto, y=Gosta, group=3), color="black") +
  geom_line(data=dm,aes(x=Leto, y=Zelo_gosta, group=4), color="green") +
  ggtitle("Koeficient rasti za kuhinje") + xlab("Leto") + ylab("Koeficient rasti") + 
  theme(legend.position="bottom") + guides(color="none") + scale_fill_discrete(name="Legenda",labels=c("Zelo redka","Redka","Gosta","Zelo gosta"))










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
