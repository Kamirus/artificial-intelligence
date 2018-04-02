# 1

> Zaproponuj optymistyczną heurystykę dla zadania z końcówkami szachowymi z P1

mat z czarnym i białym królem + białą wieżą możliwy **tylko gdy czarny przy ścianie**

wieża dojdzie wszędzie w dwóch ruchach

**2 * odległość-czarnego-od-najbliższej-ściany**

> Jak zmieniłoby się to zadanie (i heurystyka), gdyby czarne też miały wieżę?

czarna wieża nie ułatwia zadania

> A gdyby celem był jakikolwiek mat?

**minimum z obu**


# 3

> Pokaż, że spójna heurystyka jest zawsze optymistyczna

h jest rozsądna **wtw** h(s_end) = 0

h jest spójna **wtw** cost(s1, s2) + h(s2) >= h(s1)

h jest optymistyczna (dopuszczalna) **wtw** h(s) <= cost*(s)

Pokażmy h - spójna & rozsądna => h - optymistyczna

1. Dla s takich, że cost*(s) = 0 działa z rozsądności.
2. Zał. działa dla stanów s: cost*(s) < k. Weźmy s: cost*(s) = k
  * k != 0 więc istnieje s' bliżej celu
  * cost*(s) = cost(s, s') + cost*(s')
  * cost*(s') < k więc z założenia indukcyjnego h(s') <= cost*(s')
  * ze spójności mamy: h(s) <= cost(s, s') + h(s')
  * h(s) <= cost(s, s') + h(s') <= cost*(s') + cost(s', s) = cost*(s)

> Podaj przykład heurystyki, która jest optymistyczna, a nie jest spójna

Graf: v1 -> v2 -> v3 -> v4; gdzie v4 jest końcowym stanem; wagi krawędzi równe 1

cost*(v1) = 3; cost*(v2) = 2; cost*(v3) = 1; cost*(v4) = 0

Niech h(v1) = 2; h(vi) = 0

h jest optymistyczna i rozsądna. Nie jest spójna

2 = h(v1) <= cost(v1, v2) + h(v2) = 1 + 0 = 1. Fałsz


# 4

> Heurystyka h zwraca odległości od najbliższego stanu końcowego. Pokaż, że h jest spójna.

odległość m - nieujemna funkcja:

* m(x,x) = 0
* m(x,y) = m(y,x)
* m(x,y) + m(y,z) >= m(x,z)

Chcemy pokazać

h(v) <= cost(v, v') + h(v')

1. end jest najbliższym stanem końcowym dla v i v'
  * h(v) = **m(v, end) <= m(v, v') + m(v', end)** = cost(v,v') + h(v')

2. end, end' są najbliższymi stanami końcowymi odpowiednio dla v, v' (end != end')
  * zał (A.C) h(v) > cost(v, v') + h(v')
  * czyli m(v, end) > m(v,v') + m(v', end')
  * wiemy że: m(v, end') <= m(v,v') + m(v', end')
  * łącząc: m(v, end') < m(v, end)
  * sprzeczność, end' nie jest najbliższym stanem końcowym


# 5 meh

> Udowodnij, że jeśli przestrzeń stanów jest drzewem to do optymalności A* wystarczy optymistyczna heurystyka

optymistyczność: h(v) <= m(v, end) dla każdego end - stan końcowy

Dla dowolnego end: h(end) <= m(end, end) = 0, zatem mamy też rozsądność

Niech end to optymalny stan końcowy

f(end) = h(end) + g(end) = g(end) **bo h rozsądna**

Pierwszym stanem końcowym oglądanym przez A* będzie end (bądź inny równie optymalny), każdy inny end' jest nie bliżej: g(end') >= g(end)


# 6

> Jaki preprocessing do zadania o podróżowaniu (paliwo, paczki, dzieci) przydatny do liczenia h.

> Podaj optymistyczną h do każdego wariantu:

> paliwo

> paczki

> dzieci


# 7

> Sudoku więzy

```
Xij in 1..9

all_different(Xi1..Xi9)
all_different(X1j..X9j)
all_different(Xi  j, Xi  j+1, Xi  j+2,
              Xi+1j, Xi+1j+1, Xi+1j+2,
              Xi+2j, Xi+2j+1, Xi+2j+2) i,j <- [1,4,7]
```

> Zaproponuj inne sformułowanie Sudoku jako problemu więzowego spełniające: 
> - dziedziny wszystkich zmiennych są istotnie mniejsze niż 9!
> - wszystkie więzy są binarne

all_different(X1..Xn) wyrazić więzami binarnymi

```
X1 != X2, ..., X1 != Xn
X2 != X3, ..., X1 != Xn
...
Xn-1 != Xn
```

# 8

> n-arne więzy do 2- lub 3-arnych

[Binarization](https://ktiml.mff.cuni.cz/~bartak/constraints/binary.html)

**c(X1..Xn)**
to więz którego arność chcemy zredukować

**U in {(X1..Xn): c(X1..Xn)}**
tworzymy zmienną U, której dziedziną są wszystkie możliwe krotki rozwiązań więzu c(...)

**Xi = U[i]**
nasze więzy binarne mają teraz postać binarną

jeśli U to 'wynik' to Xi jest i-tym jego elementem

> 2a + 4b > 7c + d^2 + ef + g^3
W
A > B

A = 2a + 4b

B = C + D

C = 7c + d^2

D = ef + g^3
