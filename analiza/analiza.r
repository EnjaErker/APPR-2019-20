# ANALIZA

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
levels <- c(0, 80, 90, Inf)
labels <- c("1.","2.","3.")
placa_ogrevanje <- placa_ogrevanje %>% mutate(razred = cut(placa_indeks, levels, labels = labels))
placa_ogrevanje$stevilo <- parse_integer(placa_ogrevanje$stevilo)
leta.razredi <- placa_ogrevanje %>% group_by(leto, razred) %>% summarise(povprecje = mean(stevilo))
leta.razredi$leto <- parse_integer(leta.razredi$leto)

#stanovanja_kuhinja
stanovanja_kuhinja <- left_join(kuhinja,stanovanja,by="Obcina")
colnames(stanovanja_kuhinja) <- c("obcina", "leto", "stevilo", "stanovanja")
levels <- c(0,350,450,Inf)
labels <- c("malo","srednje","veliko")
stanovanja_kuhinja <- stanovanja_kuhinja %>% mutate(st_stan = cut(stanovanja, levels, labels = labels))
stanovanja_kuhinja$stevilo <- parse_integer(stanovanja_kuhinja$stevilo)
leta.st_stan <- stanovanja_kuhinja %>% group_by(leto, st_stan) %>% summarise(povprecje = mean(stevilo))
leta.st_stan$leto <- parse_integer(leta.st_stan$leto)

#POMANKLJIVOSTI:

#pretvorba v obliko tidy data
pomankljivosti <- pomankljivosti %>% gather(key = "tip", value = "vrednost", -Obcina)

#delezi stanovanj s pomakljivostmi v posamezni obcini v odstotkih
delezi <- pomankljivosti %>% group_by(Obcina) %>% top_n(2) %>% summarise(delez = min(vrednost)/max(vrednost)*100)
levels <- c(0,25,50,75,Inf)
labels <- c("zelo nizka","nizka","visoka","zelo visoka")
delezi <- delezi %>% mutate(pomankljivost = cut(delez, levels, labels=labels))

#PREOSTALO ZA VIZUALIZACIJE

#brezposelni
levels <- c(0,12,Inf)
labels <- c("ne kritična","kritična")
brezposelni <- brezposelni %>% mutate(kriticnost = cut(vrednost, levels, labels = labels))
#obcini z najmanj in najbolj kriticno brezposelnostjo
y_max <- max(brezposelni[,"vrednost"])
najbolj_kriticna <- brezposelni[brezposelni$'vrednost' == y_max, "Obcina"]
y_min <- min(brezposelni[,"vrednost"])
najmanj_kriticna <- brezposelni[brezposelni$'vrednost' == y_min, "Obcina"]

#place
levels <- c(0, 80, 90, Inf)
labels <- c("1.","2.","3.")
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
