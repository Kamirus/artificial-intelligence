# 1 - wnioskowanie w obrazkach logicznych

Niech:
 - n - ilość wierszy
 - m - ilość kolumn

**Część wspólna min-to-max**
 - wygeneruj wiersz z minimalnymi spacjami. Jeden wiersz dopchany do lewej, drugi do prawej. Zaznacz miejsca, które są w obu i dotyczą tego samego bloku!

jeśli zaznaczone pole należy do skrajnego bloku to wykreśl nieosiągalne

jeśli blok gotowy to wykreśl sąsiednie pola

jeśli wymaganie spełnione, wykreśl pozostałe

wykreśl za krótkie puste bloki (np. pierwszy dł 4, a są 3 puste pola)

wykreśl nieosiągalne pola w wierszu (np. wszystkie możliwości, jakie pola w żadnej)

`Rozważ wywnioskowane bloki. Zobacz na listę ich długości. Można ograniczyć zakres pozycji niektórych bloków`

`Zawężenie obszaru, gdy blok nie może się zmieścić gdzieś`

Rozważ pierwszy wywnioskowany ciągły blok.
Jeśli nie można wstawić przed niego pierwszego bloku to jest to blok pierwszy. 
Zaznacz część wspólną dopełnienia tego bloku max do lewej z max do prawej (koniec mapy albo przed następnym blokiem)

Jeśli liczba wywnioskowanych bloków jest równa liczbie wynikowych, to weź część wspólną dopełnień min-max

Jeśli liczba wywnioskowanych bloków jest większa od liczby wynikowych, zobacz które mogą zostać połączone, jeśli łączenie jest pewne to połącz. Można łączyć, gdy nie ma zakazanego na drodze i nie jest za długi

Jeśli wiemy, że wywnioskowane pole należy do bloku i w każdej możliwości taki blok zawiera inne wywnioskowane pole -> dopełnij między tymi polami

`Rozważmy dwa ciągłe bloki już wywnioskowane. Jeśli nie można ich połączyć ...`

**Jak generować wszystkie możliwości mając już coś wywnioskowane i coś wykreślone**

