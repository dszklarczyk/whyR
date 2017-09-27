setwd("C://Users/Darek/Documents/MEGAsync/Projekty FuRBS/whyR")

#wczytanie danych z CSV
baza1=read.csv ("survey-data.csv", header=T, 
                fileEncoding = "UTF-8")
install.packages("dplyr")
library(dplyr)

#podgl¹d na bazê...
View(baza1)
glimpse(baza1)
baza1=read.csv ("survey-data.csv", header=T, encoding="UTF-8", stringsAsFactors = T)

#CZYSZCZENIE DANYCH
#krok 1 - braki danych
#braki danych
#w czym problem
wektor=c(1,4,6,7,1,5,7,NA,4,NA,4,1,4,3,2,2,NA,12,9,NA,5)
mean(wektor)
mean(wektor, na.rm=T)
sum(is.na(wektor))

#funkcja zliczaj¹ca procent braków danych dla zmiennej
sapply(baza1, function (x) {sum(is.na(x))})
procNA= function(x){sum(is.na(x))/length(x)*100}

#zapêtlenie funkcji po wszystkich kolumnach
apply(baza1,2,procNA)

#wektor z brakami dla zmiennych, dla wygodnego podgl¹du
x=apply(baza1,2,procNA)

#wyœwietlamy zmienne, w których %NA > 5
x[x>5]
x[x<3]
wybor=names(x[x<3])
baza1m=select(baza1, wybor)
class(baza1m$age)
baza1m$age=as.numeric(baza1m$age)
mean(baza1m$age)
mean(baza1m$age, na.rm=T)

#z braku miejsca - nie zajmujemy siê podczas warsztatów imputacj¹ braków danych!!!
#podejœcie NIEZALECANE
baza1m_narm=na.omit(baza1m)

#krok 2 - b³êdy
install.packages("editrules")
library(editrules)

reg1=editset(c("age>7","age<100"))
reg1
violatedEdits(reg1,baza1m)
bledy_reg1=violatedEdits(reg1,baza1m)
violatedEdits(reg1,baza1m_narm)
bledy_reg1=violatedEdits(reg1,baza1m_narm)
summary(bledy_reg1)
plot(bledy_reg1)
baza1m$age[baza1m$age<=7]=baza1m$age=8

#opcjonalnie, w celu dokonania "masowych poprawek"
install.packages("deducorrect")
library(deducorrect)

#krok 3 - outliers
boxplot(baza1$spendings)
#jaka wartoœæ stanowi 95 percentyl 
quantile(baza1$spendings,0.95, na.rm=T)
#wy³¹czenie outlierów na boxplocie
boxplot(baza1$spendings, outline=F)

summary(baza1$spendings)
#wartoœci dla czêœci boxplota
boxplot.stats(baza1$spendings)[1]
#liczba obserwacji bez NA
boxplot.stats(baza1$spendings)[2]
boxplot.stats(baza1$spendings)[4]
#regu³a Tukeya
(200-15)*1.5
(200-15)*3
200+277.5
#ile mamy outlierów (powy¿ej 460 z³)
length(baza1$spendings[baza1$spendings>460]) #to niestety liczy z NA
outliers=baza1[baza1$spendings>460&!is.na(baza1$spendings),]


#SZYBKIE ANALIZY W R
#wczytanie danych z SPSS
library(foreign)
baza2=read.spss("ESS7e02_1.sav", use.value.labels = T, 
                to.data.frame = T,use.missings = T, 
                max.value.labels =10)

etykiety=attr(baza2,"variable.labels")
write.csv2(etykiety,"etykiety.csv")
baza2_PL=filter(baza2,cntry=="PL")

#tabele
table(baza2_PL$imptrad)
prop.table(table(baza2_PL$imptrad))
prop.table(table(baza2_PL$imptrad))*100
margin.table(table(baza2_PL$imptrad))
addmargins(table(baza2_PL$imptrad))
addmargins(prop.table(table(baza2_PL$imptrad)))

#szybki wykresik
tradycja=prop.table(table(baza2_PL$imptrad))*100
barplot(tradycja)
par(mar=c(2,12,2,2))
barplot(tradycja, horiz=T, las=1, col="green")

#samodzielnie: czy Polacy lubi¹ byæ pos³uszni (ipfrule)
prop.table(table(baza2_PL$ipfrule))*100

#gdy chcemy wyœwietliæ przez table liczbê NA
table(baza2_PL$imptrad,exclude=NULL)

#ale co, jeœli nasz wynik musi zostaæ ZWA¯ONY?
library(weights)
tradycja_w=wpct(baza2_PL$imptrad,baza2_PL$pspwght)
tradycja_w*100
barplot(tradycja_w, horiz=T, las=1, col="green")

library(questionr)
#pytania wielokrotnego wyboru

which(colnames(baza2_PL)=="hltprhc")
which(colnames(baza2_PL)=="hltprna")

#zestaw wielokrotnych odpowiedzi
multi.table(baza2_PL[,262:276])
#true codes= mo¿liwoœæ wprowadzenia jako "1" wartoœci innych ni¿ 1,
#np.tekst
multi.table(baza2_PL[,262:276],true.codes="Marked")
multi.table(baza2_PL[,262:276],true.codes="Marked", weights=baza2_PL$pspwght)
irec()

#tabele krzy¿owe
library(gmodels)
CrossTable(baza2_PL$ipfrule,baza2_PL$jbspv, format="SPSS")
CrossTable(baza2_PL$ipfrule,baza2_PL$jbspv, 
           prop.chisq=F, prop.r=F,format="SPSS")

#do samodzielnego sprawdzenia: wa¿noœæ regu³ a typ miejsca pracy? (tporgwk)
CrossTable(baza2_PL$ipfrule,baza2_PL$tporgwk, prop.chisq=F, prop.r=F, prop.t=F,format="SPSS",
           max.width = 4)

#wiele mo¿liwoœci edycji: funkcja crosstab()
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")

#ale gdy chcemy korzystaæ z wag... faworyt z pakietu (descr)
install.packages("descr")
library(descr)
descr::crosstab(baza2_PL$ipfrule,baza2_PL$jbspv, prop.c=T,format="SPSS")
wynik2=descr::crosstab(baza2_PL$ipfrule,baza2_PL$jbspv, 
                       prop.c=T,format="SPSS", 
                       weight = baza2_PL$pspwght)
wynik2
plot(wynik2, las=1)

#tabela krzy¿owa z zestawem wielokrotnych odpowiedzi
cross.multi.table(baza2_PL[,262:276], baza2_PL$hltprca, true.codes="Marked", freq=TRUE, n=T,
                  weights=baza2_PL$pspwght, na.rm=T)

wynik1=as.table(cross.multi.table(baza2_PL[,262:276], baza2_PL$hltprca, true.codes="Marked", freq=TRUE, n=T,
                         weights=baza2_PL$pspwght, na.rm=T))
#eksport tabel do XLS 
library(XLConnect)
wyniki=loadWorkbook("wynik.xls", create=T)
createSheet (wyniki, name="wynik1")
writeWorksheet(wyniki,wynik1,sheet="wynik1")
saveWorkbook(wyniki)





#WA¯ONE PODSUMOWANIA ZMIENNYCH ILOŒCIOWYCH
dane$age=as.numeric(dane$age)
dane$waga=runif(3346,min=0.8, max=3)

#zwyk³e podsumowanie zmiennej iloœciowej
summary(dane$age)

#wa¿ona œrednia
wtd.mean(dane$age, weights=dane$waga)

#wa¿ona wariancja i odchylenie standardowe
wtd.var(dane$age, weights=dane$waga)
sqrt(wtd.var(dane$age, weights=dane$waga))

#wa¿one kwartyle
wtd.quantile(dane$age, weights = dane$waga)

#LOSOWANIE WARSTWOWE - PRZYK£AD GMIN POLSKI
install.packages("sampling")
library (sampling)
baza=read.csv2("operat_R.csv", header=T, encoding="UTF-8")
set.seed(13)
próba=strata(baza,stratanames=c("Kategoria.gminy"),size=c(145,20,52,14,130,17,4,109),method="srswor")
baza$ID_unit=1:2478
selektor=próba$ID_unit
wybrane=baza[selektor,]
write.csv2(wybrane, "próba_gmin.csv")
