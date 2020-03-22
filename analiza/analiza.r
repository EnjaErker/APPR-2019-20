# 4. faza: Analiza podatkov

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
kazalniki$Obcina <- gsub("Koper/Capodistria1)", "Koper/Capodistria", kazalniki$Obcina)
kazalniki$Obcina <- gsub("Ankaran/Ancarano1)", "Ankaran/Ancarano", kazalniki$Obcina)
kazalniki$Obcina <- gsub("Trebnje2)", "Trebnje", kazalniki$Obcina)

#konstrukcija podtabel gostota, placa_indeks, brezposelni,stanovanja 
gostota <- kazalniki %>% filter(tip == "gostota_prebivalstva")
gostota <- gostota[,-2]
placa_indeks <- kazalniki %>% filter(tip == "pov_mes_neto_placa_indeks")
placa_indeks <- placa_indeks[,-2]
brezposelni <- kazalniki %>% filter(tip == "st_reg_brezposelnosti_ods")
brezposelni <- brezposelni[,-2]
stanovanja <- kazalniki %>% filter(tip == "st_stan_na_tisoc_preb")
stanovanja <- stanovanja[,-2]

#gostota_vodovod
#združitev v novo tabelo gostota_vodovod
gostota_vodovod <- left_join(vodovod,gostota,by="Obcina")
colnames(gostota_vodovod) <- c("obcina", "leto", "stevilo", "gostota")
#sortiranje obcin glede na gostoto
levels <- c(0,261,521, 780, Inf)
labels <- c("zelo_redka", "redka", "gosta", "zelo_gosta")
gostota_vodovod <- gostota_vodovod %>% mutate(skupina = cut(gostota, levels, labels = labels))
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

#izračun deležev stanovanj z vsaj eno pomankljivostjo (nov stolpec: Okviren delež stanovanj s pomankljivostmi)
pomankljivosti[, "Okviren delež stanovanj s pomankljivostmi (v %)"] <- round((pomankljivosti$'Število vseh stanovanj' - (apply(pomankljivosti[, 3:6], 1, max)))/(pomankljivosti$`Število vseh stanovanj`)*100,0)

#ocenitev kritičnosti situacije glede na slovensko povprečje
povprečje <- mean(pomankljivosti$'Okviren delež stanovanj s pomankljivostmi (v %)')
pomankljivosti$`Kritičnost situacije (glede na slovensko povprečje)` <- ifelse(pomankljivosti$'Okviren delež stanovanj s pomankljivostmi (v %)'<povprečje,"Ne kritična","Kritična")

#funkcija za sortiranjem občin glede na pomankljivosti v opremljenosti stanovanj
fun4 <- function(x) if (x<25) {'zelo nizka'} else if (x<50) {'nizka'} else if (x<75) {'visoka'} else {'zelo visoka'}

#uporaba fun4 na podatkih za pomankljivosti, dodani stolpec za razdelitev občin v štiri kategoriji (zelo nizka, nizka, visoka, zelo visoka)
pomankljivosti$`Pomankljivost opremljenosti stanovanj` <- mapply(fun4, pomankljivosti$`Okviren delež stanovanj s pomankljivostmi (v %)`)

