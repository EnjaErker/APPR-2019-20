# Analiza opremljenosti stanovanj po slovenskih občinah (v letih od 2011 do 2018)
Repozitorij z gradivi za projekt pri predmetu APPR v študijskem letu 2019/20

* [![Shiny](http://mybinder.org/badge.svg)](http://mybinder.org/v2/gh/jaanos/APPR-2019-20/master?urlpath=shiny/APPR-2019-20/projekt.Rmd) Shiny
* [![RStudio](http://mybinder.org/badge.svg)](http://mybinder.org/v2/gh/jaanos/APPR-2019-20/master?urlpath=rstudio) RStudio

## Tematika

Osnovna ideja: poiskati povezave in medsebojne vplive določenih spremenljivk (npr. dohodek, urbanost/ruralnost, turizem, tipi lastništva, leto izgradnje itd.) z opremljenostjo stanovanj po slovenskih občinah (oz. ponekod po statističnih regijah)

Število podatkovnih enot – število občin cca. 210

Časovno obdobje: največ podatkov za leta 2011,2015,2018, torej od 2011 do 2018

Oblike vhodnih podatkov:
-	Csv – z različnimi ločili 
-	Xml – excel preglednica 
-	Json datoteka
-	Json-stat datoteka
-	Excel datoteka

Vir podatkov: SiStat (v oklepajih številke virov)

Osnovni podatki:
-	napeljave: vodovod, kanalizacija, električni tok, centralno ogrevanje (1)
-	pomožni prostori: kopalnica, stranišče, kuhinja (1)
-	povprečna uporabna površina m2 na stanovalca, delež stanovanj z manj kot 10m2 uporabne površine na osebo (2,4)
-	delež naseljenih stanovanj ki nimajo vseh elementov osnovne infrastrukture (2)
-	število sob, število oseb (3)
-	ogrevanja: centralno, daljinsko, ni ogrevanja, drugo ogrevanje (5)
-	stanovanja brez: kopalnice, notranjega stranišča, kuhinje (6)
-	stanovanja brez: centralnega ogrevanja, vode, elektrike, priklopa na javno kanalizacijo (7)

Spremenljivke: (niso vsi podatki po občinah, nekateri po statističnih regijah(13))
-	dohodek (10)
-	urbanost /ruralnost (?)
-	turizem (17)
-	stanovanjski stroški kot breme (8), preživetje gospodinjstev s svojimi prihodki (9)
-	tip lastništva: lastniška, najeta, drugi tipi (11)
-	gostota naseljenosti (12)
-	starost oz. leto izgradnje (13)
-	stanovanjske razmere: svetlost, hrup, onesnaženost, kriminal itd. (14)
-	izbrane dobrine: telefon, tv, računalnik pralni stroj, avto (15)
-	prenaseljenost stanovanja (16)

Zasnova podatkovnega modela:
-	ime tabele: stolpci (vsaka tabela 3-krat in sicer za leta 2011,2015,2018)
-	Stanovanja brez osnovne infrastrukture glede na finančno stanje: brez osnovne infrastrukture, dohodek, stanovanjski stroški kot breme, preživetje gospodinjstev s svojimi prihodki
-	Število sob in stanovalcev glede na naseljenost: število sob, število oseb, uporabna površina, gostota naseljenosti, prenaseljenost stanovanja
-	Opremljenost stanovanj z napeljavo in ogrevanjem glede na okoliščine: napeljave, ogrevanja, urbanost/ruralnost, starost oz. leto izgradnje
-	Neopremljenost stanovanj (brez) glede na stanovanjske razmere: stanovanja brez kopalnice, notranjega stranišča kuhinja in centralnega ogrevanja, vode, elektrike, priklopa na javno kanalizacijo, stanovanjske razmere, izbrane dobrine
-	Stanovanja brez osnovne infrastrukture po tipu lastništva in turizmu občine: brez osnovne infrastrukture, tip lastništva, turizem

Plan dela: 
-	Razvrščanje podatkov (iskanje občin s podobnimi lastnostmi):
o	po dohodku
o	turizmu
o	letu izgradnje stanovanj
o	tipu lastništva
o	urbane in ruralne občine
o	gostota naseljenosti
-	Predikcija (napovedovanje časovne vrste, trendi): 
o	napovedovanje razvoja opremljenosti stanovanj in občin
o	večja opremljenost, večji dohodki (inflacija)
o	večanje gostote v urbanih občinah in zmanjševanje le-te v ruralnih (slabša opremljenost na deželi, boljša v mestih)
o	slabša opremljenost starih stanovanj in boljša novih
o	opremljenost glede na razvoj turizma


## Program

Glavni program in poročilo se nahajata v datoteki `projekt.Rmd`.
Ko ga prevedemo, se izvedejo programi, ki ustrezajo drugi, tretji in četrti fazi projekta:

* obdelava, uvoz in čiščenje podatkov: `uvoz/uvoz.r`
* analiza in vizualizacija podatkov: `vizualizacija/vizualizacija.r`
* napredna analiza podatkov: `analiza/analiza.r`

Vnaprej pripravljene funkcije se nahajajo v datotekah v mapi `lib/`.
Podatkovni viri so v mapi `podatki/`.
Zemljevidi v obliki SHP, ki jih program pobere,
se shranijo v mapo `../zemljevidi/` (torej izven mape projekta).

## Potrebni paketi za R

Za zagon tega vzorca je potrebno namestiti sledeče pakete za R:

* `knitr` - za izdelovanje poročila
* `rmarkdown` - za prevajanje poročila v obliki RMarkdown
* `shiny` - za prikaz spletnega vmesnika
* `DT` - za prikaz interaktivne tabele
* `rgdal` - za uvoz zemljevidov
* `rgeos` - za podporo zemljevidom
* `digest` - za zgoščevalne funkcije (uporabljajo se za shranjevanje zemljevidov)
* `readr` - za branje podatkov
* `rvest` - za pobiranje spletnih strani
* `tidyr` - za preoblikovanje podatkov v obliko *tidy data*
* `dplyr` - za delo s podatki
* `gsubfn` - za delo z nizi (čiščenje podatkov)
* `ggplot2` - za izrisovanje grafov
* `mosaic` - za pretvorbo zemljevidov v obliko za risanje z `ggplot2`
* `maptools` - za delo z zemljevidi
* `extrafont` - za pravilen prikaz šumnikov (neobvezno)

## Binder

Zgornje [povezave](#analiza-podatkov-s-programom-r-201819)
omogočajo poganjanje projekta na spletu z orodjem [Binder](https://mybinder.org/).
V ta namen je bila pripravljena slika za [Docker](https://www.docker.com/),
ki vsebuje večino paketov, ki jih boste potrebovali za svoj projekt.

Če se izkaže, da katerega od paketov, ki ji potrebujete, ni v sliki,
lahko za sprotno namestitev poskrbite tako,
da jih v datoteki [`install.R`](install.R) namestite z ukazom `install.packages`.
Te datoteke (ali ukaza `install.packages`) **ne vključujte** v svoj program -
gre samo za navodilo za Binder, katere pakete naj namesti pred poganjanjem vašega projekta.

Tako nameščanje paketov se bo izvedlo pred vsakim poganjanjem v Binderju.
Če se izkaže, da je to preveč zamudno,
lahko pripravite [lastno sliko](https://github.com/jaanos/APPR-docker) z želenimi paketi.

Če želite v Binderju delati z git,
v datoteki `gitconfig` nastavite svoje ime in priimek ter e-poštni naslov
(odkomentirajte vzorec in zamenjajte s svojimi podatki) -
ob naslednjem zagonu bo mogoče delati commite.
Te podatke lahko nastavite tudi z `git config --global` v konzoli
(vendar bodo veljale le v trenutni seji).
