
# Metody Statystyczne w Zarządzaniu Wierzytelnościami Masowymi
# Laboratorium 11



### Wymagane biblioteki 

```
library(InformationValue)
```

## Zadanie 1

Przygotuj dane do budowy modelu, spełniając następujące warunki:

* Zdefiniuj zmienną modelowaną bazując na zbiorze danych `events` (np. czy klient wpłacił więcej niż 300 w pierwszych
12 miesiącach obsługi).

* Imputuj braki danych tam gdzie widzisz tego sens.

* Wskaż zmienne skorelowane ze sobą i zapisz macierz korelacji.

## Zadanie 2

Sprawdź siłę predykcyjną wszystkich cech za pomocą współczynnika IV (Information Value).

## Zadanie 3

Przekoduj zmienne na WoE.

## Zadanie 4

Zbuduj kilka wersji modeli metodą regresji logistycznej (np. zmieniając predyktory, uwględniając wagi).

## Zadanie 5

Porównaj powstałe modele za pomocą m.in. współczynnika Gini lub AUC, macierzy klasyfikacji (dla wybranego punktu *cut-off*) i wykresu separacji dobrych i złych klientów.
