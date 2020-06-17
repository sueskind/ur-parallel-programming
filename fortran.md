# Fortran Cheatsheet

Hier ein denglisches Cheatsheet, bzw. Zusammenfassung über alle Fortran Syntax- und Sprachfeatures aus dem Parallel Programming Kurs. Wird laufend übers Semester erweitert. Dann muss man in den Hausaufgaben nicht immer erst 3 PDFs mit 50 Slides durch skippen bis man die eine Sache gefunden hat. :)

## Table of contents
 * [Hello World](#Hello-World)
 * [Basic program structure](#Basic-program-structure)
   * [Subroutines](#Subroutines)
   * [Functions](#Functions)
 * [Variablen](#Variablen)
   * [Attributes](#Attributes)
   * [Strings/Characters](#StringsCharacters)
   * [Numeric precision aka kind values](#Numeric-precision-aka-kind-values)
 * [Arrays](#Arrays)
   * [Static Arrays](#Static-Arrays)
   * [Allocatable Arrays](#Allocatable-Arrays)
   * [Initialisierung](#Initialisierung)
   * [Array intrinsics](#Array-intrinsics)
 * [Control structures](#Control-structures)
   * [Loops](#Loops)
   * [if ... then ... else](#if--then--else)
 * [Intrinsic functions](#Intrinsic-functions)
   * [Mathe](#Mathe)
   * [Type Conversion](#Type-Conversion)
   * [Hilfsmethoden für Numerics](#Hilfsmethoden-für-Numerics)
   * [Weitere](#Weitere)
 * [Eigene Types](#Eigene-Types)
   * [Vererbung bzw. extends](#Vererbung-bzw-extends)
 * [Modules](#Modules)
 * [Mehrere Files nutzen](#Mehrere-Files-nutzen)
 * [Kleiner Style Guide](#Kleiner-Style-Guide)

TODO:
 * Logic .or. .and. ...
 * cpu_time()
 * print format

## Hello World
File `helloworld.f90`
```fortran
program hello_world
    print *, "Hello World!"
end program hello_world
```
Kompilieren mit `gfortran helloworld.f90` oder `gfortran -o outname helloworld.f90`.


## Basic program structure
```fortran
program name
    ! Comments mit '!'
    
    ! >>>Variable declarations here<<<
    
    ! >>>Program statements here<<<
end program name
```

Generell werden Programme von oben nach unten kompiliert - man kann nur Sachen benutzen, die man weiter oben definiert hat.

### Subroutines
Wenn man Teile eines Programs "outsourcen" will (meistens aus Übersichtlichkeitsgründen oder wenn man öfter das gleiche macht, z.B. Logging-Routine). Haben keinen Rückgabewert (dafür siehe Function).
```fortran
subroutine name(var1, var2)   ! natürlich beliebig viele Variablen
  implicit none   ! muss man immer extra machen

  ! Variablen deklarieren. Parameter und temporäre Variablen in dieser subroutine.
  integer     :: var1
  real        :: var2
  integer     :: a    

  ! >>>Program statements here<<<
end subroutine name
```
Subroutinen werden im programm mit `call name(var1, ...)` aufgerufen.

### Functions

Es gibt 3 Varianten eine Funktion zu definieren - alle äquivalent (Beispiel `distance()` function):

#### Variante 1
```fortran
real function distance(x, y)  ! Rückgabetyp im Titel
  implicit none
  real  :: x, y

  distance = abs(x - y)       ! Dem Funktionsnamen am Ende den Rückgabewert assignen
end function distance
```

#### Variante 2
```fortran
function distance(x, y)
  implicit none
  real  :: x, y
  real  :: distance       ! Hier den Rückgabetyp über Funktionsnamen definieren

  distance = abs(x - y)   ! Dem Funktionsnamen am Ende den Rückgabewert assignen
end function distance
```

#### Variante 2
```fortran
function distance(x, y) result(d)  ! Hier sagen, welche Variable den Rückgabewert enthält
  implicit none
  real  :: x, y
  real  :: d      ! Hier den Rückgabetyp definieren

  d = abs(x - y)  ! Den Rückgabewert irgendwo assignen
end function distance
```


## Variablen
```fortran
implicit none                   ! einfach immer hinmachen, sonst sind bestimmte Variablennamen schon vordeklariert
integer             :: i        ! Ganzzahl
real                :: r        ! Fließkommazahl
complex             :: c        ! Komplexe Zahl
logical             :: l        ! Wahr oder Falsch
character           :: ch       ! Buchstabe oder Strings

integer             :: a, b     ! Mehrere vom selben Typ
```

### Attributes
Kommen hinter den Datentyp, z.B.:
```fortran
integer, parameter  :: const1
```
```fortran
parameter       ! Konstante Zahl, muss direkt initialisiert werden (mit = 1.5345 z.B.)

! Für functions/subroutines:
intent(in)      ! Diese Variable kann nur gelesen werden
intent(out)     ! Diese Variable soll nicht gelesen werden
intent(inout)   ! Unnötig, aber signalisiert, dass die Variable gelesen und beschrieben wird
optional        ! Optionaler Parameter, kann mit present() überprüft werden, ob übergeben wurde

save            ! Wert wird über mehrere Funktionencalls gespeichert
! Achtung: integer :: i = 5 ist das gleiche wie integer, save :: i = 5. Am besten nur Konstanten (parameter) in der Deklaration initialisieren.

! Für modules:
private         ! Variable nur im Modul sichtbar
public          ! Variable auch außerhalb des Moduls sichtbar
protected       ! Variable read only
```

### Strings/Characters
```fortran
character(len=10)       :: var1                             ! Dieser String kann maximal 10 Zeichen lang sein
character(*), parameter :: var2 = "Wer das liest ist doof"  ! Bei Konstanten Strings kann man die String Länge mit `*` erkennen lassen
```

### Numeric precision aka kind values
Jeder Datentyp hat (weil PC Speicher halt endlich ist) einen maximalen Wertebereich. Wenn man diesen größer oder kleiner machen will kann man das wie folgt:
```fortran
! 'kind' value bestimmen (immer vom Typ 'integer, parameter'):
integer, parameter  :: ik = selected_int_kind(e)     ! kind value für einen integer in range -10^e bis 10^e
integer, parameter  :: rk = selected_real_kind(p)    ! kind value für einen real mit garantiert p präzisen Stellen nach dem Komma
integer, parameter  :: sk = selected_real_kind(p, e) ! kind value für einen real mit p Kommastellen und range -10^e bis 10^e

! 'kind' value übergeben
integer(kind=ik)    :: i
real(kind=rk)       :: r
real(sk)            :: s ! 'kind' kann man auch weg lassen
complex(sk)         :: c ! complex nimmt einen real kind

! auch literal constants brauchen einen kind, wenn sie zu lang werden:
integer, parameter      :: pik = selected_real_kind(18)
real(rk), parameter     :: pi = 3.141592653589793239_pik    ! Einfach hinten anhängen
```
Für die kind Variable ein `k`, `_k` oder `_kind` an den Variablennamen anzuhängen ist Konvention (und Konventionen sind gut, vor allem bei so unübersichtlichen Sprachen wie Fortran lol).


## Arrays
Hier geht der Spaß los.

Indices gehen standardmäßig bei 1 los (iihh...), der letzte ist inklusive. Die Anzahl der Elemente bekommt man also mit `oben - unten + 1`. Mann kann aber selber festlegen (auch negative) wo die Indices losgehen/aufhören. Anders als in C, Java, Python (und jeder anderen guten Sprache) ist der erste Index der schnelle und der letzte der langsame.

### Static Arrays
Größe wird zur Compiletime festgelegt. Werden auf Stack gespeichert -> nicht mega viel RAM für Stack, aber sollte reichen.
```fortran
integer, dimension(10)        :: v    ! Integer Array mit 10 Elementen: v(1), v(2), ..., v(10)
real, dimension(10, 5)        :: A    ! Real 2D-Array mit 10x5 Elementen: A(1, 1), A(2, 1), ..., A(10, 5)
complex, dimension(10, 5, 3)  :: T    ! Complex 3D-Array mit 10x5x3 Elementen: T(1, 1, 1), T(2, 1, 1), ..., T(10, 5, 3)

integer, dimension(-5:3, 4:7) :: M    ! Integer Array mit 3-(-5)+1 = 9 mal 7-4+1 = 4 Elementen: M(-5, 4), M(-4, 4), ..., M(3, 7)

! Äquivalent ist:
integer   :: v(10)
real      :: A(10, 5)
complex   :: T(10, 5, 3)
integer   :: M(-5:3, 4:7)
```

### Allocatable Arrays
Man kann die Größe zur Runtime festlegen. Werden auf dem Heap gespeichert -> mehr RAM.
```fortran
! Rang (also Dimensionen) vom Array wird hier schon festgelegt
integer, dimension(:), allocatable      :: v        ! Array
integer, dimension(:, :), allocatable   :: A        ! 2D-Array
integer, allocatable                    :: B(:, :)  ! Äquivalent

! Hier Sachen machen

allocate(v(5:10))             ! Größe wird erst hier festgelegt
allocate(A(1, 5:20), B(5, 6)) ! Man kann auch mehrere gleichzeitig allocaten

deallocate(v, A, B)           ! Den Speicher wieder freigeben (da freut sich das OS)

allocated(A)                  ! Praktisch für if(allocated(A)) deallocate(A)
call move_alloc(A, B)         ! Deallocates A, allocates B. ACHTUNG subroutine -> 'call' nicht vergessen
```

### Initialisierung
Wenn man nicht initialisiert, ist der Ausgangszustand undefiniert! Nicht wie bei Java!
```fortran
! literal
v = (/3, 1, 5, 2, 3, 4/)
v = [3, 1, 5, 2, 3, 4]  ! äquivalent
v = 5                   ! v = [5, 5, ...], praktisch für 0er initialisierung

! normale loop
do i = 1, 5
  do j = 3, 8
    A(i, j) = 2 * i * j
  end do
end do

! implicit loop
v = (/ (i, i = 1,5) /)
v = [ (i, i = 1,5) ]    ! äquivalent

! forall loop
forall (i=1:5,   j=3:8)         A(i, j) = 2 * i * j
forall (i=1:5:2, j=3:8:3)       A(i, j) = i + 5 * j   ! Step size
forall (i=1:5,   j=3:8, i==j)   A(i, j) = 2 * i * j   ! logische Bedingung
```

### Array intrinsics
Nice Abkürzungen, endlich mal was gutes an Fortran. ;)
```fortran
integer   :: v(10), w(10), A(10, 10)  ! Gegeben

v(3:)       ! Alles ab 3tem Index
v(:5)       ! Alles bis 5ten Index
v(4::2)     ! Alle Elemente ab Index 4 mit geradem Index
v(4:2:-1)   ! Elemente von 4 bis 2 mit absteigendem Index
! Also: v(:) == v == v(1:10) == v(1:)

v = w               ! kopiert alle Elemente von w nach v
A = A + 1           ! Addiert 1 zu allen Elementen von A (geht auch mit allen anderen Operatoren)
v = A(3, :)         ! kopiert 3te Reihe von A nach v
A(2:5, 3) = v(4:7)  ! kopiert Bereiche in das andere Array
```

```fortran
reshape([1, 2, 3, 4, 5, 6], shape=[2, 3])   ! Bringt 1x6 Array in neue Form 2x3

lbound(A), ubound(A)  ! get niedrigste/höchste Indices (returned einen Vektor)
size(A)               ! get Größe
shape(A)              ! get Dimensionen (returned einen Vektor)
minval(A), maxval(A)  ! kleinstes/größtes Element

! Mit Masken, z.b. A > 4 gibt [.true., .false., ...] mit .true. wo A > 4, sonst .false.
! Wie in R
count(A > 4)  ! wie oft in der Maske .true. vorkommt
all(A > 4)    ! wenn Maske nur .true.
any(A > 4)    ! wenn Maske mind. einmal .true.
```

Mathe intrinsics wie `abs`, `sin`, `exp` usw. wirken auf ein Array elementwise (Geil!).

```fortran
matmul(A, B)      ! Matrixmultiplikation
norm2(v)          ! Betrag, ACHTUNG nicht mit abs() verwechseln

! Selbsterklärend:
transpose(A)
dot_product(v, w)
sum(A), product(A)
```


## Control structures

### Loops

#### do ... end do
```fortran
do i = 1, 10   ! beide inklusive (also in dem Fall 10 mal loopen)
    ! something here
end do

do i = 1, 10, 2   ! Schrittweite 2, optional
    ! something here
end do

do
    ! unendliche loop
end do

Schleife : do i = 1, 5
    ! gibt Schleife den Namen "Schleife"
end do Schleife
```
Keywords:
```fortran
exit        ! bricht aktuelle Schleife ab (break in Python)
exit Name   ! bricht alle aktuellen Schleifen bis inklusive Schleife "Name" ab
cycle       ! macht mit nächster Iteration weiter (continue in Python)
```
**Unendliche loops wenns geht nicht benutzen. [David Hilbert lässt grüßen.](https://de.wikipedia.org/wiki/Halteproblem)**  
**Man kann die Zählvariable in der Schleife nicht ändern. Auch Änderungen an den von/bis Werten ändert nichts.**  
**Um Bugs vorzubeugen, die Zählvariable nach der Schleife nicht mehr benutzen (außer für andere Schleifen).**

#### implicit do
Eigentlich nur gut um Arrays zu assignen oder printen.
```fortran
v = (/ (i, i=1,5) /) ! Macht Vektor (1, 2, 3, 4, 5)
```
*An der Stelle ein kleines uff für die Syntax... jede normale Sprache parsed das als i, dann i=1, dann 5.*
```fortran
print *, (v(i), i=1,3)              ! printet  1  2  3

print *, ((A(i,j), i=1,3), j=1,3)   ! kann man auch verschachteln für noch weniger readability
```

### if ... then ... else
Einfach ein Beispiel:
```
if (a > 0) then
    a = 0
else if (a == 0) then
    a = -1
else
    a = 1
end if
```
Den `else if` Block kann man natürlich auch weg lassen, wenn man nur `if` haben will geht auch:
```
if (a > 0) a = 0
```

### case
Gibts in Fortran, braucht aber keiner.


## Intrinsic functions
Funktionen die man sich nicht erst selber definieren muss.

### Mathe
```fortran
! Selbsterklärend
abs(x)
sqrt(x)
sin(x), cos(x), tan(x)
asin(x), acos(x), atan(x)
exp(x), log(x)

mod(x, y) ! Divisionsrest von x / y
** ! ist "hoch", also z.B. 5**3 = 125
```

### Type Conversion
```fortran
int(x)      ! real in integer verwandeln (abrunden)
nint(x)     ! real auf integer runden
floor(x)    ! gleich wie int
fraction(x) ! alles vor dem komma auf 0 setzen
real(x)     ! integer in real verwandeln
```

### Hilfsmethoden für Numerics
`[i,r,c]` ist hier als Platzhalter für **eine** Variable des Datentyps i für integer, r für real und c für complex.
```fortran
digits([i,r])       ! binary precision (signifikante Stellen in binär)
precision([r,c])    ! decimal precision (für real hilfreicher als digits)
range([i,r,c])      ! exponent range in decimal (größter möglicher exponent)
epsilon([r])        ! Kleinste Zahl e, so dass 1 + e immernoch größer als 1 ist. Garnicht mal so unpraktisch für numerische Berechnungen, wo man viele kleine Werte aufaddiert.
huge([i,r])         ! Größte Zahl mit selbem kind
tiny([r])           ! Kleinste (positive) Zahl
radix([i,r])        ! Krass unnötig
```

### Weitere
```fortran
max(x, y, ...)
min(x, y, ...)
```


## Eigene Types
Wenn man Bock auf objekt-orientiertes Programmieren hat.
```fortran
! Definition, das _t ist Konvention für Typenamen
type irgendwas_t
    ! Members
    integer :: i
    real    :: r
end type irgendwas_t

! Benutzen
type(irgendwas_t) :: var

var = irgendwas_t(1, 5.3) ! Konstruktor call wie in Python, wurde implizit definiert

var%i = 2   ! Mit % auf die Member zugreifen (ist der Punkt . in Java und Python)
```

### Vererbung bzw. extends

```fortran
! Hier den doppel-Doppelpunkt beachten (Fortran ist weird)
!                          v
type, extends(irgendwas_t) :: something_t
    integer :: a
end type something_t

! Ein Objekt vom type something_t hat jetzt Member i, r und a und wird erzeugt mit something_t(18, 4.2, 4)
!                                                                                              i    r   a
```

## Modules
```fortran
module Modulname

  ! private/public
  ! types
  ! variables
  ! interfaces

  contains

  ! subroutines
  ! functions

end module Modulname
```
Importiert wird das dann im Programm mit `use Modulname` vor allen anderen Definitionen (auch for `implicit none`).

Normalerweise sind alle Member im Modul `public`, außer man schreibt ein allein stehendes `private` rein (das kann man mit `public` bei Variablendeklaration wieder aufheben).



## Mehrere Files nutzen
Ein wenig kompliziert, aber macht das Programm deutlich übersichtlicher.

TODO

## Kleiner Style Guide
Zum Schluss noch ein kleiner Style Guide. Muss man nicht befolgen, aber sieht dann halt kacke aus.
```fortran
! Nach einem Komma ein Leerzeichen:
print *, "Hallo"
do n = 1, 2

! vor und nach einem Operator ein Leerzeichen:
a = 5 + 3 * 4
x > b

! Bei Variablendeklarierung Tabs benutzen
integer, paramter   :: a
real                :: b ! Der ist weiter hinten als nötig, aber sieht gut aus

! Zeilenlänge nicht über 100-120 Zeichen (je nach präferenz, aber konsequent)
arbeiterunfallversicherungsgesetzistdaslaengsteWortimDudenohneBindestrichaberweilsnurdreiunddreissigBuchstabenhatschreibichdashiernochumaufhundertachtundsiebzigBuchstabenzukommen

! Leere Zeilen zwischen semantisch verschiedenen Code Abschnitten sind euer Freund!

! Variablennamen in camelCase oder snake_case (je nach Präferenz, aber konsequent)
```
