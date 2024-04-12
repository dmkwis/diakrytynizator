# SO-1-Diakrytynizator
Assembly x86_64 programme applying polynomial to UTF-8 words, input/output buffering and validating data.

Compile with: 
```
nasm -f elf64 -w+all -w+error -o diakrytynizator.o diakrytynizator.asm
ld --fatal-warnings -o diakrytynizator diakrytynizator.o
```

Example usage:
```
echo "ŁOŚ" | ./diakrytynizator 1075041 623420 1; echo $?
```
_ _ _ _
## Diakrytynizator

Zaimplementuj w asemblerze x86_64 program, który czyta ze standardowego wejścia tekst, modyfikuje go w niżej opisany sposób, a wynik wypisuje na standardowe wyjście. Do kodowania tekstu używamy UTF-8, patrz https://en.wikipedia.org/wiki/UTF-8. Program nie zmienia znaków o wartościach unicode z przedziału od 0x00 do 0x7F. Natomiast każdy znak o wartości unicode większej od 0x7F przekształca na znak, którego wartość unicode wyznacza się za pomocą niżej opisanego wielomianu.
Wielomian diakrytynizujący

Wielomian diakrytynizujący definiuje się przez parametry wywołania diakrytynizatora:

./diakrytynizator a0 a1 a2 ... an

jako:
```math
w(x) = a_n * x^n + ... + a_2 * x^2 + a_1 * x + a_0 
```
Współczynniki wielomianu są nieujemnymi liczbami całkowitymi podawanymi przy podstawie dziesięć. Musi wystąpić przynajmniej parametr a0.

Obliczanie wartości wielomianu wykonuje się modulo 0x10FF80. W tekście znak o wartości unicode x zastępuje się znakiem o wartości unicode w(x - 0x80) + 0x80.
Zakończenie programu i obsługa błędów

Program kwituje poprawne zakończenia działania, zwracając kod 0. Po wykryciu błędu program kończy się, zwracając kod 1.

Program powinien sprawdzać poprawność parametrów wywołania i danych wejściowych. Przyjmujemy, że poprawne są znaki UTF-8 o wartościach unicode od 0 do 0x10FFFF, kodowane na co najwyżej 4 bajtach i poprawny jest wyłącznie najkrótszy możliwy sposób zapisu.
