# Analiza stanovanjskih razmer in opremljenosti stanovanj po občinah v Sloveniji

Repozitorij z gradivi za projekt pri predmetu APPR v študijskem letu 2019/20

* [![Shiny](http://mybinder.org/badge.svg)](http://mybinder.org/v2/gh/EnjaErker/APPR-2019-20/master?urlpath=shiny/APPR-2019-20/projekt.Rmd) Shiny
* [![RStudio](http://mybinder.org/badge.svg)](http://mybinder.org/v2/gh/EnjaErker/APPR-2019-20/master?urlpath=rstudio) RStudio

## Tematika

Vir: SiStat

Oblike vhodnih podatkov:
-	Csv – z različnimi ločili 
-	Excel datoteka

Osnovna ideja: 
-	analizirati osnovne indikatorje opremljenosti stanovanj po občinah (napeljave, prostori, površine, …) za leto 2015 (napeljave za zadnje razpoložljivo leto 2010),
-	analizirati druge indikatorje (razvoja) občin (neto plače, brezposelnost, gostota prebivalstva, starost prebivalstva) za leto 2015,
-	poiskati povezave (korelacije) med izbranimi spremenljivkami za leto 2015,
-	ugotoviti medsebojne vplive (regresije) določenih spremenljivk (npr. vpliv brezposelnosti na opremljenost ali vpliv starosti prebivalstva na opremljenost) za leto 2015,
-	analizirati časovno serijo za napeljave  (2002-2010, letni podatki).

Število podatkovnih enot: število občin 211 (oziroma manj v kolikor bodo podatki manjkajoči)

Spremenljivke opremljenosti:
-	napeljave: vodovod, kanalizacija, električni tok, centralno ogrevanje (2002-2010) (1),
-	delež naseljenih stanovanj ki nimajo vseh elementov osnovne infrastrukture (2),
-	število sob, število oseb (2011,2015,2018)(3),
-	povprečna uporabna površina stanovanj m2 (4),
-	število stanovanj na 1000 prebivalcev (4),
-	tri ali več sobna stanovanja (% vseh) (4).

Druge spremenljivke občin: 
-	povprečna mesečna neto plača (4),
-	gostota prebivalstva (4),
-	povprečna starost prebivalstva (4),
-	stopnja registrirane brezposelnosti (4). 

Zasnova podatkovnega modela:
-	vrstice = občine
-	stolpci = spremenljivke
-	Tabela 1: Napeljave: vodovod, kanalizacija, električni tok, centralno ogrevanje,
-	Tabela 2: Delež naseljenih stanovanj brez osnovne infrastrukture: centralno ogrevanje, voda, elektrika, priklop na javno kanalizacijo,
-	Tabela 3: Število naseljenih stanovanj po številu sob in številu oseb: 1,2,3,4,5 sob ali več, 1,2,3,4,5,6,7 oseb ali več,
-	Tabela 4: Kazalniki razvoja občin: povprečna uporabna površina stanovanj, število stanovanj na 1000 prebivalcev, tri ali več sobna stanovanja, povprečna mesečna neto plača, gostota prebivalstva, povprečna starost prebivalstva, stopnja registrirane brezposelnosti. 

Plan dela: 
-	Razvrščanje podatkov (iskanje občin s podobnimi lastnostmi): po opremljenosti, po razmerah,
-	Predikcija (napovedovanje časovne vrste, trendi): napovedovanje razvoja opremljenosti stanovanj glede na razvitost občine, večja opremljenost <- večje neto plače, slabše/boljše razmere <– večja/manjša gostota naseljenosti občine.


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
