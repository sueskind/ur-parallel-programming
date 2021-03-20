# Fortran Cheatsheet

## Table of contents
- [Hello World](#hello-world)
- [Basic program structure](#basic-program-structure)
  - [Subroutines](#subroutines)
  - [Functions](#functions)
- [Variables](#variables)
  - [Attributes](#attributes)
  - [Strings/Characters](#stringscharacters)
  - [Numeric precision aka kind values](#numeric-precision-aka-kind-values)
- [Arrays](#arrays)
  - [Static Arrays](#static-arrays)
  - [Allocatable arrays](#allocatable-arrays)
  - [Initialization](#initialization)
  - [Array intrinsics](#array-intrinsics)
- [Control structures](#control-structures)
  - [Loops](#loops)
  - [if ... then ... else](#if--then--else)
  - [case](#case)
- [Intrinsic functions](#intrinsic-functions)
  - [Math](#math)
  - [Type Conversion](#type-conversion)
  - [Helper methods for numerics](#helper-methods-for-numerics)
  - [Others](#others)
- [Custom types](#custom-types)
  - [Inheritance i.e. extends](#inheritance-ie-extends)
- [Modules](#modules)
- [Use multiple files](#use-multiple-files)
- [Brief style guide](#brief-style-guide)

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
Compile with `gfortran helloworld.f90` or `gfortran -o outname helloworld.f90`.


## Basic program structure
```fortran
program name
    ! Comments mit '!'
    
    ! >>>Variable declarations here<<<
    
    ! >>>Program statements here<<<
end program name
```

Generally, programs get compiled from top to bottom. Only earlier defined things can be used.

### Subroutines
Soubroutines do not have return values.
```fortran
subroutine name(var1, var2)   ! as many arguments as you wish
  implicit none   ! otherwise some symbols are predefined

  ! Declare variables
  integer     :: var1
  real        :: var2
  integer     :: a    

  ! >>>Program statements here<<<
end subroutine name
```
Call subroutines in a programm with `call name(var1, ...)`.

### Functions

There are 3 major variants to define a function, all are equivalent. Example `disatance()` function:

#### Variant 1
```fortran
real function distance(x, y)  ! Return type in title
  implicit none
  real  :: x, y

  distance = abs(x - y)       ! Assign the return value to the function name at the end
end function distance
```

#### Variant 2
```fortran
function distance(x, y)
  implicit none
  real  :: x, y
  real  :: distance       ! Define return type here

  distance = abs(x - y)   ! Assign the return value to the function name at the end
end function distance
```

#### Variant 2
```fortran
function distance(x, y) result(d)  ! Define which variable will be the return value
  implicit none
  real  :: x, y
  real  :: d      ! Define return type

  d = abs(x - y)  ! Assign the return value anywhere in the function body
end function distance
```


## Variables
```fortran
implicit none
integer             :: i        ! Integer
real                :: r        ! Floating point
complex             :: c        ! Complex number
logical             :: l        ! True or false
character           :: ch       ! Character or strings

integer             :: a, b     ! Declare multiple variables at once
```

### Attributes

```fortran
integer, parameter  :: const1
```
```fortran
parameter       ! Constant, must be immediately initialized (e.g. with = 1.5345)

! For functions/subroutines:
intent(in)      ! Read-only variable
intent(out)     ! Not supposed to read this variable
intent(inout)   ! Signals that this variable is vor reading and writing
optional        ! Marks an optional parameter, use present() to check for presence

save            ! Save value over multiple function calls
! Caution: integer :: i = 5 is the same as integer, save :: i = 5. It is advisable to only initialize constants at declaration

! For modules:
private         ! Module visibility
public          ! Variable visible outside of module
protected       ! Variable read only
```

### Strings/Characters
```fortran
character(len=10)       :: var1              ! This string can have at most 10 characters
character(*), parameter :: var2 = "foo bar"  ! Recognize length of constant strings with *
```

### Numeric precision aka kind values

```fortran
! Determine 'kind' value (kind values are of type 'integer, parameter'):
integer, parameter  :: ik = selected_int_kind(e)     ! kind value for an integer in range -10^e bis 10^e
integer, parameter  :: rk = selected_real_kind(p)    ! kind value for a real with guaranteed p digits after the decimal point
integer, parameter  :: sk = selected_real_kind(p, e) ! kind value for a real with guaranteed p digits after the decimal point and range -10^e to 10^e

! Assign 'kind' value
integer(kind=ik)    :: i
real(kind=rk)       :: r
real(sk)            :: s ! 'kind=' is not needed
complex(sk)         :: c ! complex takes the same kind as real

! Literal constants also need a kind value when they get too long
integer, parameter      :: pik = selected_real_kind(18)
real(rk), parameter     :: pi = 3.141592653589793239_pik    ! Append with _
```
Appending `k`, `_k` or `_kind` to the kind variable name is convention.


## Arrays
Indices start with 1. The last index is inclusive. Number of elements is therefore determined by `last - first + 1`. Fortran allows for defining which is the first and which is the last index. For multidimensional arrays the first index is the fastest one (column-major).

### Static Arrays
Set array size at compile time. -> They are stored on the stack (memory consideration for large arrays).
```fortran
integer, dimension(10)        :: v    ! Integer array with 10 elements: v(1), v(2), ..., v(10)
real, dimension(10, 5)        :: A    ! Real 2D-array with 10x5 elements: A(1, 1), A(2, 1), ..., A(10, 5)
complex, dimension(10, 5, 3)  :: T    ! Complex 3D-array with 10x5x3 elements: T(1, 1, 1), T(2, 1, 1), ..., T(10, 5, 3)

integer, dimension(-5:3, 4:7) :: M    ! Integer array mit 3-(-5)+1 = 9 times 7-4+1 = 4 elementen: M(-5, 4), M(-4, 4), ..., M(3, 7)

! Equivalent:
integer   :: v(10)
real      :: A(10, 5)
complex   :: T(10, 5, 3)
integer   :: M(-5:3, 4:7)
```

### Allocatable arrays
Allocate arrays and their size at runtime. They are stored on the heap.
```fortran
! Rank (= dimensions) of array has to be known beforehand
integer, dimension(:), allocatable      :: v        ! Array
integer, dimension(:, :), allocatable   :: A        ! 2D-Array
integer, allocatable                    :: B(:, :)  ! Equivalent

! Code possible here

allocate(v(5:10))             ! Set array size
allocate(A(1, 5:20), B(5, 6)) ! Allocate multiple at the same time

deallocate(v, A, B)           ! Free space

allocated(A)                  ! Useful for if(allocated(A)) deallocate(A)
call move_alloc(A, B)         ! Deallocates A, allocates B. This is a subroutine -> 'call' needed
```

### Initialization
Not initializing values will leave them undefined, not predefined with zeros.
```fortran
! literal
v = (/3, 1, 5, 2, 3, 4/)
v = [3, 1, 5, 2, 3, 4]  ! equivalent
v = 5                   ! v = [5, 5, ...], useful for initialization with zeros

! regular (do-) loop
do i = 1, 5
  do j = 3, 8
    A(i, j) = 2 * i * j
  end do
end do

! implicit loop
v = (/ (i, i = 1,5) /)
v = [ (i, i = 1,5) ]    ! equivalent

! forall loop
forall (i=1:5,   j=3:8)         A(i, j) = 2 * i * j
forall (i=1:5:2, j=3:8:3)       A(i, j) = i + 5 * j   ! Step size
forall (i=1:5,   j=3:8, i==j)   A(i, j) = 2 * i * j   ! Logical condition
```

### Array intrinsics
Shorthands for arrays.
```fortran
integer   :: v(10), w(10), A(10, 10)

v(3:)       ! Get everything from 3rd index
v(:5)       ! Get everything to 5th index
v(4::2)     ! Get everything from the 4th index at even indices
v(4:2:-1)   ! Get elements from the 4th to 2nd index in descending order
! Therefore: v(:) == v == v(1:10) == v(1:)

v = w               ! Copy all values from w to v
A = A + 1           ! Add 1 to all elements of A (the same with other operators)
v = A(3, :)         ! Copy the 3rd row from A to v
A(2:5, 3) = v(4:7)  ! Copy a section from v to A
```

```fortran
reshape([1, 2, 3, 4, 5, 6], shape=[2, 3])   ! Reshapes 1x6 array into 2x3 array

lbound(A), ubound(A)  ! Get highest/lowest indices (returns vector)
size(A)               ! Get size
shape(A)              ! Get dimensions (returns vektor)
minval(A), maxval(A)  ! Get largest/smallest element

! Masks, e.g. A > 4 yields [.true., .false., ...] with .true. where A > 4, else .false.
count(A > 4)  ! Counts all .true. occurences
all(A > 4)    ! True if all elements are true
any(A > 4)    ! At least one element true
```

Math intrinsics like `abs`, `sin`, `exp` etc. act element-wise to an array.

```fortran
matmul(A, B)      ! Matrix multiplication
norm2(v)          ! Length of a spacial vector, not to confuse with abs()

! Furthermore:
transpose(A)
dot_product(v, w)
sum(A), product(A)
```


## Control structures

### Loops

#### do ... end do
```fortran
do i = 1, 10   ! both inclusive, this would loop 10 times
    ! something here
end do

do i = 1, 10, 2   ! Step size 2
    ! something here
end do

do
    ! infinite loop
end do

Loop : do i = 1, 5
    ! give label "Loop" to loop
end do Loop
```
Keywords:
```fortran
exit        ! breaks current loop
exit Name   ! breaks all inner loops until loop "Name"
cycle       ! continues with next cycle
```
**One can't change the counting variable inside the loop, it will be overwritten next iteration.**  
**To prevent bugs, don't use the counting variable after the loop.**

#### implicit do
Useful to assign elements to arrays or print them.
```fortran
v = (/ (i, i=1,5) /) ! Creates vector (1, 2, 3, 4, 5)
```
**Pay attention to the syntax of ","**  

```fortran
print *, (v(i), i=1,3)              ! prints  1  2  3

print *, ((A(i,j), i=1,3), j=1,3)   ! Can be nested for even less readability
```

### if ... then ... else

```
if (a > 0) then
    a = 0
else if (a == 0) then
    a = -1
else
    a = 1
end if
```
`else if` and/or `else` block can be ommitted:
```
if (a > 0) a = 0
```

### case
Redundant.


## Intrinsic functions

### Math
```fortran
! Self-explanatory
abs(x)
sqrt(x)
sin(x), cos(x), tan(x)
asin(x), acos(x), atan(x)
exp(x), log(x)

mod(x, y) ! Remainder of x / y
** ! exponentiation, e.g. 5**3 = 125
```

### Type Conversion
```fortran
int(x)      ! real to integer (flooring)
nint(x)     ! real to integer (rounding)
floor(x)    ! same as int()
fraction(x) ! remove the integer part of a decimal number
real(x)     ! integer to real
```

### Helper methods for numerics
In this case `[i,r,c]` are placeholders for **one** variable of type i for integer, r for real and c for complex.
```fortran
digits([i,r])       ! binary precision (significant digits in binary)
precision([r,c])    ! decimal precision (more useful for reals than digits)
range([i,r,c])      ! exponent range in decimal (largest possible exponent)
epsilon([r])        ! Smallest number e, so that 1 + e is still larger than 1. Useful for calculations with approximate precision and a lot of addition.
huge([i,r])         ! Biggest number of same kind
tiny([r])           ! Smallest (positive) number
radix([i,r])        ! Very unnecessary
```

### Others
```fortran
max(x, y, ...)
min(x, y, ...)
```


## Custom types
```fortran
! Definition, the _t is convention for type names
type something_t
    ! Members
    integer :: i
    real    :: r
end type something_t

type(something_t) :: var

var = something_t(1, 5.3) ! Constructur is defined implicitly

var%i = 2   ! Access member with %
```

### Inheritance i.e. extends

```fortran
!                   Note the colon
!                          v
type, extends(irgendwas_t) :: something_t
    integer :: a
end type something_t

! An object of type something_t now has members i, r and a and is created by something_t(18, 4.2, 4)
!                                                                                        i    r   a
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
Import a module into a program with `use Modulname` **before** all other definitions (also before `implicit none`).

By default all member of a module are `public`, except the module contains `private` (this can be overriden with `public` at variable declaration).



## Use multiple files

TODO

## Brief style guide
```fortran
! Space after comma:
print *, "Hello"
do n = 1, 2

! Space before and after an operator
a = 5 + 3 * 4
x > b

! Indent the :: when declaring variables
integer, paramter   :: a
real                :: b

! Line length not longer than 100-120 characters (depends on preference, but should be used consequently)
arbeiterunfallversicherungsgesetzistdaslaengsteWortimDudenohneBindestrichaberweilsnurdreiunddreissigBuchstabenhatschreibichdashiernochumaufhundertachtundsiebzigBuchstabenzukommen

! Empty lines between semantically different parts of a program

! Variable names in camelCase or snake_case (depends on preference, but should be used consequently)
```
