# whyR

Program warsztatu

Część pierwsza: 9.00 - 10.30

1. Przywitanie uczestników, cel i plan warsztatu
2. Przygotowanie danych do analizy
a) ładowanie danych - jak to jest w przypadku danych sondażowych?
- przykład z CSV; w dalszej części - ładowanie pliku SPSS (foreign)

baza2<- read.spss("ESS7e02_1.sav", use.value.labels=T, to.data.frame = T,  add.undeclared.levels = "no", use.missings = TRUE, max.value.labels<-10)

#etykiety ze zmiennych
etykiety<-attr(baza_2, "variable.labels") 
write.csv2(etykiety, "etykiety.csv")

b) czyszczenie danych
- co to są czyste dane w przypadku danych sondażowych?
- nie tyle (tidyr), co ... - ćwiczenia z czyszczenia danych
- braki danych w pigułce
- operacje na danych: zmienianie wartości zmiennych, tworzenie nowych zmiennych, podzbiorów itd.
3) Szybkie analizy w R 
c)szybka droga do wyników - tabele
- rozkłady procentowe
- zestawy wielokrotnych odpowiedzi
- ważenie rozkładów (weights, questionr)
d) w stronę raportu
- eksport tabeli do Excela (XLConnect)
- eksport tabeli do Worda (kable)

Część druga: 11.00 - 12.30

4) Dalsze analizy danych sondażowych
a) podsumowania zmiennych ilościowych
b) wizualizacja rozkładów w (ggplot2)
c) co, gdy tabele są zbyt złożone? (MCA)

5) Prosty dobór próby z operatu - losowanie proste, losowanie warstwowe
