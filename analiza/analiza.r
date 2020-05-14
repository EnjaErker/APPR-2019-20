# OSNOVNA ANALIZA

#NAPELJAVE

#pretvorba v obliko tidy data
napeljave <- napeljave %>% 
  gather(key="leto.tip", value="stevilo", -Obcina) %>%
  separate(leto.tip, c("leto", "tip"), "(?<=[0-9]) ")

#konstrukcija podtabel vodovoda, kanalizacije, centralnega ogrevanja, električnega toka, kopalnice, stranišča,kuhinje
vodovod <- napeljave %>% filter(tip == "Vodovod")
centralno_ogrevanje <- napeljave %>% filter(tip == "Centralno ogrevanje")
elektricni_tok <- napeljave %>% filter(tip == "Električni tok")
kopalnica <- napeljave %>% filter(tip == "Kopalnica")
stranisce <- napeljave %>% filter(tip == "Stranišče")
kuhinja <- napeljave %>% filter(tip == "Kuhinja")
kanalizacija <- napeljave %>% filter(tip == "Kanalizacija")
vodovod <- vodovod[,-3]
centralno_ogrevanje <- centralno_ogrevanje[,-3]
elektricni_tok <- elektricni_tok[,-3]
kopalnica <- kopalnica[,-3]
stranisce <- stranisce[,-3]
kuhinja <- kuhinja[,-3]
kanalizacija <- kanalizacija[,-3]

#KAZALNIKI

#pretvorba v obliko tidy data
kazalniki <- kazalniki %>% gather(key = "tip", value = "vrednost", -Obcina)

#konstrukcija podtabel gostota, placa_indeks, brezposelni,stanovanja 
gostota <- kazalniki %>% filter(tip == "gostota_prebivalstva")
gostota <- gostota[,-2]
placa_indeks <- kazalniki %>% filter(tip == "pov_mes_neto_placa_indeks")
placa_indeks <- placa_indeks[,-2]
brezposelni <- kazalniki %>% filter(tip == "st_reg_brezposelnosti_ods")
brezposelni <- brezposelni[,-2]
stanovanja <- kazalniki %>% filter(tip == "st_stan_na_tisoc_preb")
stanovanja <- stanovanja[,-2]

#sprememba nekaterih imen
gostota$Obcina <- gsub("Koper/Capodistria1)", "Koper/Capodistria", gostota$Obcina)
gostota$Obcina <- gsub("Ankaran/Ancarano1)", "Ankaran/Ancarano", gostota$Obcina)
gostota$Obcina <- gsub("Trebnje2)", "Trebnje", gostota$Obcina)

#gostota_vodovod
gostota_vodovod <- left_join(vodovod,gostota,by="Obcina")
colnames(gostota_vodovod) <- c("obcina", "leto", "stevilo", "gostota")
#sortiranje obcin glede na gostoto
levels <- c(0,261,521, 780, Inf)
labels <- c("zelo_redka", "redka", "gosta", "zelo_gosta")
gostota_vodovod <- gostota_vodovod %>% mutate(skupina = cut(gostota, levels, labels = labels))
skupina <- gostota_vodovod
gostota_vodovod$stevilo <- parse_integer(gostota_vodovod$stevilo)
#izracun povprecij
leta.skupine.vod <- gostota_vodovod %>% group_by(leto, skupina) %>% summarise(povprecje = mean(stevilo))
leta.skupine.vod$leto <- parse_integer(leta.skupine.vod$leto)

#gostota_kanalizacija
gostota_kanalizacija <- left_join(kanalizacija,gostota,by="Obcina")
colnames(gostota_kanalizacija) <- c("obcina", "leto", "stevilo", "gostota")
levels <- c(0,261,521, 780, Inf)
labels <- c("zelo_redka", "redka", "gosta", "zelo_gosta")
gostota_kanalizacija <- gostota_kanalizacija %>% mutate(skupina = cut(gostota, levels, labels = labels))
gostota_kanalizacija$stevilo <- parse_integer(gostota_kanalizacija$stevilo)
leta.skupine.kan <- gostota_kanalizacija %>% group_by(leto, skupina) %>% summarise(povprecje = mean(stevilo))
leta.skupine.kan$leto <- parse_integer(leta.skupine.kan$leto)

#placa_ogrevanje
placa_ogrevanje <- left_join(centralno_ogrevanje,placa_indeks,by="Obcina")
colnames(placa_ogrevanje) <- c("obcina", "leto", "stevilo", "placa_indeks")
levels <- c(0, 90, 110, Inf)
labels <- c("nizka","srednja","visoka")
placa_ogrevanje <- placa_ogrevanje %>% mutate(razred = cut(placa_indeks, levels, labels = labels))
placa_ogrevanje$stevilo <- parse_integer(placa_ogrevanje$stevilo)
leta.razredi <- placa_ogrevanje %>% group_by(leto, razred) %>% summarise(povprecje = mean(stevilo))
leta.razredi <- na.omit(leta.razredi)
leta.razredi$leto <- parse_integer(leta.razredi$leto)

#POMANKLJIVOSTI:

#pretvorba v obliko tidy data
pomankljivosti <- pomankljivosti %>% gather(key = "tip", value = "vrednost", -Obcina)

#delezi stanovanj s pomakljivostmi v posamezni obcini v odstotkih
delezi <- pomankljivosti %>% group_by(Obcina) %>% top_n(2) %>% summarise(delez = min(vrednost)/max(vrednost)*100)
levels <- c(0,25,50,75,Inf)
labels <- c("zelo nizka","nizka","visoka","zelo visoka")
delezi <- delezi %>% mutate(pomankljivost = cut(delez, levels, labels=labels))

stan <- pomankljivosti %>% filter(tip == "st_vseh_stanovanj")
stan <- stan[,-2]

#PREOSTALO ZA VIZUALIZACIJE

#brezposelni
levels <- c(0,12,Inf)
labels <- c("nekritična","kritična")
brezposelni <- brezposelni %>% mutate(kriticnost = cut(vrednost, levels, labels = labels))
#obcini z najmanj in najbolj kriticno brezposelnostjo
y_max <- max(brezposelni[,"vrednost"])
najbolj_kriticna <- brezposelni[brezposelni$'vrednost' == y_max, "Obcina"]
y_min <- min(brezposelni[,"vrednost"])
najmanj_kriticna <- brezposelni[brezposelni$'vrednost' == y_min, "Obcina"]

#place
levels <- c(0, 90, 110, Inf)
labels <- c("nizka","srednja","visoka")
placa_indeks <- placa_indeks %>% mutate(razred = cut(vrednost, levels, labels = labels))
#place, obcini z najvisjim in najnizjim indeksom
y_max <- max(placa_indeks[,"vrednost"])
najvisji_indeks <- placa_indeks[placa_indeks$'vrednost' == y_max, "Obcina"]
y_min <- min(placa_indeks[,"vrednost"])
najnizji_indeks <- placa_indeks[placa_indeks$'vrednost' == y_min, "Obcina"]

#gostota
levels <- c(0,261,521, 780, Inf)
labels <- c("zelo_redka", "redka", "gosta", "zelo_gosta")
gostota <- gostota %>% mutate(skupina = cut(vrednost, levels, labels = labels))
#obcini z najvecjo in najmanjso gostoto
y_max <- max(gostota[,"vrednost"])
najbolj_gosta <- gostota[gostota$'vrednost' == y_max, "Obcina"]
y_min <- min(gostota[,"vrednost"])
najmanj_gosta <- gostota[gostota$'vrednost' == y_min, "Obcina"]

#KAZALNIKI ZA ZEMLJEVIDE

#pretvorba v obliko tidy data
kazalniki_za_zemljevide <- kazalniki_za_zemljevide %>% gather(key = "tip", value = "vrednost", -Obcina)
#preimenovanje nekaterih imen za risanje zemljevida
kazalniki_za_zemljevide$Obcina = gsub("Ankaran/Ancarano1)", "Ankaran", kazalniki_za_zemljevide$Obcina)
kazalniki_za_zemljevide$Obcina <- gsub("Koper/Capodistria1)", "Koper", kazalniki_za_zemljevide$Obcina)
kazalniki_za_zemljevide$Obcina <- gsub("Trebnje2)", "Trebnje", kazalniki_za_zemljevide$Obcina)
kazalniki_za_zemljevide$Obcina <- gsub("Miren - Kostanjevica", "Miren-Kostanjevica", kazalniki_za_zemljevide$Obcina)
kazalniki_za_zemljevide$Obcina <- gsub("Mirna2)", "Mirna", kazalniki_za_zemljevide$Obcina)
kazalniki_za_zemljevide$Obcina <- gsub("Rače - Fram", "Rače-Fram", kazalniki_za_zemljevide$Obcina)
kazalniki_za_zemljevide$Obcina <- gsub("Šempeter - Vrtojba", "Šempeter-Vrtojba", kazalniki_za_zemljevide$Obcina)
kazalniki_za_zemljevide$Obcina <- gsub("Piran/Pirano", "Piran", kazalniki_za_zemljevide$Obcina)
kazalniki_za_zemljevide$Obcina <- gsub("Mokronog - Trebelno", "Mokronog-Trebelno", kazalniki_za_zemljevide$Obcina)
kazalniki_za_zemljevide$Obcina <- gsub("Renče - Vogrsko", "Renče-Vogrsko", kazalniki_za_zemljevide$Obcina)
kazalniki_za_zemljevide$Obcina <- gsub("Dobrova - Polhov Gradec", "Dobrova-Polhov Gradec", kazalniki_za_zemljevide$Obcina)
kazalniki_za_zemljevide$Obcina <- gsub("Hrpelje - Kozina", "Hrpelje-Kozina", kazalniki_za_zemljevide$Obcina)
kazalniki_za_zemljevide$Obcina <- gsub("Sveta Trojica v Slov. goricah", "Sveta Trojica v Slovenskih goricah", kazalniki_za_zemljevide$Obcina)
kazalniki_za_zemljevide$Obcina <- gsub("Sveti Jurij v Slov. goricah", "Sveti Jurij v Slovenskih goricah", kazalniki_za_zemljevide$Obcina)
kazalniki_za_zemljevide$Obcina <- gsub("Gorenja vas - Poljane", "Gorenja vas-Poljane", kazalniki_za_zemljevide$Obcina)
kazalniki_za_zemljevide$Obcina <- gsub("Hoče - Slivnica", "Hoče-Slivnica", kazalniki_za_zemljevide$Obcina)
kazalniki_za_zemljevide$Obcina <- gsub("Izola/Isola", "Izola", kazalniki_za_zemljevide$Obcina)
kazalniki_za_zemljevide$Obcina <- gsub("Log - Dragomer", "Log-Dragomer", kazalniki_za_zemljevide$Obcina)
kazalniki_za_zemljevide$Obcina <- gsub("Lendava/Lendva", "Lendava", kazalniki_za_zemljevide$Obcina)
kazalniki_za_zemljevide$Obcina <- gsub("Dobrovnik/Dobronak", "Dobrovnik", kazalniki_za_zemljevide$Obcina)
kazalniki_za_zemljevide$Obcina <- gsub("Hodoš/Hodos", "Hodoš", kazalniki_za_zemljevide$Obcina)
#urejanje
#konstrukcija podtabel gostota, placa_indeks, brezposelni,stanovanja 
gostota_z <- kazalniki_za_zemljevide %>% filter(tip == "gostota_prebivalstva")
gostota_z <- gostota_z[,-2]
gostota_z <- gostota_z[-1,]
placa_indeks_z <- kazalniki_za_zemljevide %>% filter(tip == "pov_mes_neto_placa_indeks")
placa_indeks_z <- placa_indeks_z[,-2]
placa_indeks_z <- placa_indeks_z[-1,]
brezposelni_z <- kazalniki_za_zemljevide %>% filter(tip == "st_reg_brezposelnosti_ods")
brezposelni_z <- brezposelni_z[,-2]
brezposelni_z <- brezposelni_z[-1,]
stanovanja_z <- kazalniki_za_zemljevide %>% filter(tip == "st_stan_na_tisoc_preb")
stanovanja_z <- stanovanja_z[,-2]
stanovanja_z <- stanovanja_z[-1,]

# NAPREDNA ANALIZA

#centralno ogrevanje kot pomankljivost
cent_ogr_poman <- pomankljivosti %>% filter(tip == "centralno_ogrevanje")
cent_ogr_poman <- cent_ogr_poman[,-2]

#poenotenje obcin
cent_ogr_poman$Obcina <- gsub("Ankaran/Ancarano", "Ankaran", cent_ogr_poman$Obcina)
cent_ogr_poman$Obcina <- gsub("Koper/Capodistria", "Koper", cent_ogr_poman$Obcina)
cent_ogr_poman$Obcina <- gsub("Trebnje)", "Trebnje", cent_ogr_poman$Obcina)
cent_ogr_poman$Obcina <- gsub("Miren - Kostanjevica", "Miren-Kostanjevica", cent_ogr_poman$Obcina)
cent_ogr_poman$Obcina <- gsub("Rače - Fram", "Rače-Fram", cent_ogr_poman$Obcina)
cent_ogr_poman$Obcina <- gsub("Šempeter - Vrtojba", "Šempeter-Vrtojba", cent_ogr_poman$Obcina)
cent_ogr_poman$Obcina <- gsub("Piran/Pirano", "Piran", cent_ogr_poman$Obcina)
cent_ogr_poman$Obcina <- gsub("Mokronog - Trebelno", "Mokronog-Trebelno", cent_ogr_poman$Obcina)
cent_ogr_poman$Obcina <- gsub("Renče - Vogrsko", "Renče-Vogrsko", cent_ogr_poman$Obcina)
cent_ogr_poman$Obcina <- gsub("Dobrova - Polhov Gradec", "Dobrova-Polhov Gradec", cent_ogr_poman$Obcina)
cent_ogr_poman$Obcina <- gsub("Hrpelje - Kozina", "Hrpelje-Kozina", cent_ogr_poman$Obcina)
cent_ogr_poman$Obcina <- gsub("Sveta Trojica v Slov. goricah", "Sveta Trojica v Slovenskih goricah", cent_ogr_poman$Obcina)
cent_ogr_poman$Obcina <- gsub("Sveti Jurij v Slov. goricah", "Sveti Jurij v Slovenskih goricah", cent_ogr_poman$Obcina)
cent_ogr_poman$Obcina <- gsub("Gorenja vas - Poljane", "Gorenja vas-Poljane", cent_ogr_poman$Obcina)
cent_ogr_poman$Obcina <- gsub("Hoče - Slivnica", "Hoče-Slivnica", cent_ogr_poman$Obcina)
cent_ogr_poman$Obcina <- gsub("Izola/Isola", "Izola", cent_ogr_poman$Obcina)
cent_ogr_poman$Obcina <- gsub("Log - Dragomer", "Log-Dragomer", cent_ogr_poman$Obcina)
cent_ogr_poman$Obcina <- gsub("Lendava/Lendva", "Lendava", cent_ogr_poman$Obcina)
cent_ogr_poman$Obcina <- gsub("Dobrovnik/Dobronak", "Dobrovnik", cent_ogr_poman$Obcina)
cent_ogr_poman$Obcina <- gsub("Hodoš/Hodos", "Hodoš", cent_ogr_poman$Obcina)

stan$Obcina <- gsub("Ankaran/Ancarano", "Ankaran", stan$Obcina)
stan$Obcina <- gsub("Koper/Capodistria", "Koper", stan$Obcina)
stan$Obcina <- gsub("Trebnje)", "Trebnje", stan$Obcina)
stan$Obcina <- gsub("Miren - Kostanjevica", "Miren-Kostanjevica", stan$Obcina)
stan$Obcina <- gsub("Rače - Fram", "Rače-Fram", stan$Obcina)
stan$Obcina <- gsub("Šempeter - Vrtojba", "Šempeter-Vrtojba", stan$Obcina)
stan$Obcina <- gsub("Piran/Pirano", "Piran", stan$Obcina)
stan$Obcina <- gsub("Mokronog - Trebelno", "Mokronog-Trebelno", stan$Obcina)
stan$Obcina <- gsub("Renče - Vogrsko", "Renče-Vogrsko", stan$Obcina)
stan$Obcina <- gsub("Dobrova - Polhov Gradec", "Dobrova-Polhov Gradec", stan$Obcina)
stan$Obcina <- gsub("Hrpelje - Kozina", "Hrpelje-Kozina", stan$Obcina)
stan$Obcina <- gsub("Sveta Trojica v Slov. goricah", "Sveta Trojica v Slovenskih goricah", stan$Obcina)
stan$Obcina <- gsub("Sveti Jurij v Slov. goricah", "Sveti Jurij v Slovenskih goricah", stan$Obcina)
stan$Obcina <- gsub("Gorenja vas - Poljane", "Gorenja vas-Poljane", stan$Obcina)
stan$Obcina <- gsub("Hoče - Slivnica", "Hoče-Slivnica", stan$Obcina)
stan$Obcina <- gsub("Izola/Isola", "Izola", stan$Obcina)
stan$Obcina <- gsub("Log - Dragomer", "Log-Dragomer", stan$Obcina)
stan$Obcina <- gsub("Lendava/Lendva", "Lendava", stan$Obcina)
stan$Obcina <- gsub("Dobrovnik/Dobronak", "Dobrovnik", stan$Obcina)
stan$Obcina <- gsub("Hodoš/Hodos", "Hodoš", stan$Obcina)

#zdruzitev centralnega ogrevanja in plac
a1 <- left_join(cent_ogr_poman,placa_indeks_z,by="Obcina")
names(a1) <- c("Obcina","Brez_ogrevanja","Place")

#zdruzitev centralnega ogrevanja in brezposelosti
a2 <- left_join(cent_ogr_poman,brezposelni_z,by="Obcina")
names(a2) <- c("Obcina","Brez_ogrevanja","Brezposelni_ods")

a <- left_join(a1,a2,by="Obcina")
t <- left_join(a,stan,by="Obcina")
t$'delez' <- (t$'Brez_ogrevanja.x' / t$'vrednost')*100

#javna kanalizacija kot pomankljivost
jav_kan_poman <- pomankljivosti %>% filter(tip == "javna_kanalizacija")
jav_kan_poman <- jav_kan_poman[,-2]

#poenotenje imen obcin
jav_kan_poman$Obcina <- gsub("Ankaran/Ancarano", "Ankaran", jav_kan_poman$Obcina)
jav_kan_poman$Obcina <- gsub("Koper/Capodistria", "Koper", jav_kan_poman$Obcina)
jav_kan_poman$Obcina <- gsub("Trebnje)", "Trebnje", jav_kan_poman$Obcina)
jav_kan_poman$Obcina <- gsub("Miren - Kostanjevica", "Miren-Kostanjevica", jav_kan_poman$Obcina)
jav_kan_poman$Obcina <- gsub("Rače - Fram", "Rače-Fram", jav_kan_poman$Obcina)
jav_kan_poman$Obcina <- gsub("Šempeter - Vrtojba", "Šempeter-Vrtojba", jav_kan_poman$Obcina)
jav_kan_poman$Obcina <- gsub("Piran/Pirano", "Piran", jav_kan_poman$Obcina)
jav_kan_poman$Obcina <- gsub("Mokronog - Trebelno", "Mokronog-Trebelno", jav_kan_poman$Obcina)
jav_kan_poman$Obcina <- gsub("Renče - Vogrsko", "Renče-Vogrsko", jav_kan_poman$Obcina)
jav_kan_poman$Obcina <- gsub("Dobrova - Polhov Gradec", "Dobrova-Polhov Gradec", jav_kan_poman$Obcina)
jav_kan_poman$Obcina <- gsub("Hrpelje - Kozina", "Hrpelje-Kozina", jav_kan_poman$Obcina)
jav_kan_poman$Obcina <- gsub("Sveta Trojica v Slov. goricah", "Sveta Trojica v Slovenskih goricah", jav_kan_poman$Obcina)
jav_kan_poman$Obcina <- gsub("Sveti Jurij v Slov. goricah", "Sveti Jurij v Slovenskih goricah", jav_kan_poman$Obcina)
jav_kan_poman$Obcina <- gsub("Gorenja vas - Poljane", "Gorenja vas-Poljane", jav_kan_poman$Obcina)
jav_kan_poman$Obcina <- gsub("Hoče - Slivnica", "Hoče-Slivnica", jav_kan_poman$Obcina)
jav_kan_poman$Obcina <- gsub("Izola/Isola", "Izola", jav_kan_poman$Obcina)
jav_kan_poman$Obcina <- gsub("Log - Dragomer", "Log-Dragomer", jav_kan_poman$Obcina)
jav_kan_poman$Obcina <- gsub("Lendava/Lendva", "Lendava", jav_kan_poman$Obcina)
jav_kan_poman$Obcina <- gsub("Dobrovnik/Dobronak", "Dobrovnik", jav_kan_poman$Obcina)
jav_kan_poman$Obcina <- gsub("Hodoš/Hodos", "Hodoš", jav_kan_poman$Obcina)
