# Interpreter języka imperatywnego While

1. Wywołanie programu
2. Semantyka
3. Składnia, lekser, parser
4. Lint (analiza statyczna programu)
5. Ściągawka składni

## Wywoływanie programu

Kompilowanie poleceniem `make`, powinno utworzyć plik `wlang` w bieżącym katalogu.

* `./wlang interpret <sciezka do pliku>` - interpretuje dany kod. Ewentualnie można na koniec dodać opcję `--dump`, to po wykonaniu programu wyświetli się zawartość "pamięci" po wykonaniu programu.
* `./wlang ast <sciezka do pliku>` - wyświetla AST
* `./wlang check <sciezka do pliku>` - uruchamia linta


## Semantyka

Semantyka języka jest taka, jaka była zaprezentowana na wykładzie z programowania M, z dodatkiem argumentów procedur, które były przerabiane na ćwiczeniach.

## Składnia

W katalogu `examples/` postarałem się zawrzeć wystarczającą ilość przykładów, żeby spokojnie zrozumieć składnię. Na samym końcu tego dokumentu jest ściągawka ze składni.
Nie wszystkie przykłady powinny się poprawnie kompilować i wykonywać - niektóre sprawdzają jakąś funkcjonalność leksera, parsera albo linta. Dla wygody, na początku programu jest komentarz z krótkim opisem co dany kod sprawdza i co powinno się pojawić na wyjściu.

Składnia jest też oczywiście opisana przez pliki leksera i parsera.

Zdaję sobie sprawę, że parser ma 2 konflikty shift/reduce w miejscu gdzie zajmujemy się łapaniem wyjątków, ale domyślne zachowanie parsera (reduce) jest takie jakie miało być.

Dodatkowe zalety parsera:

* potrafi wyrzucać jak najwięcej błędów w kodzie, nie zatrzymuje się na jednym
* przy wyrzucaniu informacji o błędach, pokazuje w których liniach one są



## Lint

To taka rzecz nadprogramowo.
Lint przeprowadza bardzo prostą analizę statyczną programu.

### Wykrywane problemy

* użycie niezadeklarowanych zmiennych lub procedur
* "przesłanianie" zmiennych lub procedur - czyli ostrzeżenie, że deklarujemy zmienną o nazwie, która już jest zajęta przez inną zmienną
* niepoprawna arność wywoływanych funkcji
* wyjątki, które mogą być rzucone, a nie są poniżej "ratowane"

### Uwagi do Linta

* przy przesłanianiu, nie zwraca uwagi na inne byty - tzn. jeśli mamy zadeklarowaną *procedurę* o nazwie `p`, to przy deklaracji *zmiennej* `p`, nie otrzymamy żadnego komunikatu (mój świadomy wybór, że to tak działa, łatwo zmodyfikować)
* Lint nie zwraca uwagi na wyjątki, które są rzucane bezpośrednio w programie, a nie wewnątrz jakiegoś bloku `try`. (Tutaj się trochę zagapiłem i po prostu nie chciało mi się tego modyfikować, ponieważ całkiem sporo bym musiał zmieniać i przechowywać cały czas informację o tym, jakie wyjątki w danej chwili mogą być rzucone.)

## Ściągawka do składni

### Uwagi

* `begin ... end` możemy zastąpić inną pojedyńczą instrukcją zakończoną średnikiem, jest to też równoważne `{ ...}`

### Instrukcje

* `skip` - nie robi nic
* `x := 5 * 7` - przypisuje wartość do zmiennej - zmienna *musi* być wcześniej zadeklarowana w strefie deklaracji zmiennych jakiegoś bloku
* `write y` - wyświetla na wyjściu "y = <wartość y>"
* `read x` - czyta z wejścia wartość x'a
* `raise NazwaWyjatku` - rzuca wyjątek NazwaWyjatku
* `while x <= y do begin ... end` - pętla while
* `call p()`, `call q(3 * 5)` - wywołanie funkcji
* `if x = y then begin ... end else begin ... end` - instrukcja warunkowa. `else` jest obowiązkowe.
* `try { ... } rescue FstExc with { ... } rescue SndExc with { ... }` - obsługa wyjątków

### Operatory arytmetyczne

Arytmetyka obsługuje dodawanie, mnożenie, odejmowanie, unarny minus oraz oczywiście zmienne, stałe i nawiasowanie.
Przykładowe wyrażenie: `(-x + 5) * 2 + 9`

### Operatory logiczne

Wartości `true` i `false`, operatory to `=`, `<=` (te operatory są aplikowane do liczb) oraz `&` - koniunkcja i `!` - negacja. Ubogo, ale wszystkie wyrażenia logiczne da się zareprezentować.

### Bloki

Bloki mogą służyć jako zwykłe konkatenację instrukcji, żeby wywołać więcej niż jedną instrukcję w treści `if`, `while` itd., ale również w każdym bloku można zadeklarować dowolną ilość zmiennych i procedur (procedury mogą być rekurencyjne, ale nie ma zaimplementowanej wzajemnej rekursji). Każda zmienna której chcemy użyć, musi być zadeklarowana w jakimś bloku.

Ogólny wzór na bloki:

<pre>
begin
	[deklaracje zmiennych]
	[deklaracje procedur]
	[instrukcje]
end
</pre>

Przykładowy blok:

<pre>
begin
	var x := 5;
	var y := 3 * x;
	proc p() is begin
		skip;
	end
	proc q(a) is begin
		write a;
	end

	write y;
end
</pre>

Zwróćmy uwagę na brak średników po deklaracji funkcji, oraz na fakt, że przy deklaracji zmiennych możemy korzystać z tych zadeklarowanych wyżej, w tym samym bloku.