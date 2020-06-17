# OpenMP Cheatsheet

## Table of contents
 * [Basics](#basics)
   + [Hello World](#hello-world)
   + [Compilation](#compilation)
 * [Runtime functions](#runtime-functions)
 * [Parallel regions](#parallel-regions)
   + [Clauses](#clauses)
     - [num_threads](#num_threadsnum)
     - [default, shared, private, firstprivate](#default-shared-private-firstprivate)
     - [threadprivate, copyin](#threadprivate-copyin)
     - [reduction](#reduction)
     - [if](#if)
 * [Workshare constructs](#workshare-constructs)
   + [single/master](#singlemaster)
     - [nowait](#nowait)
     - [copyprivate](#copyprivate)
   + [sections](#sections)
   + [do](#do)
     - [collapse](#collapse)
     - [schedule](#schedule)
   + [critical](#critical)
   + [Others](#others)

## Basics

### Hello World
```{fortran}
program main

  !$ use omp_lib
  integer :: nthreads

  nthreads = 1
  !$ nthreads = omp_get_max_threads()

  print *, "#threads =", nthreads

end program main
```

### Compilation
```
export OMP_NUM_THREADS=4 # optional

gfortran -fopenmp name.f90
```

## Runtime functions
```{fortran}
! Subroutines:
!$ call omp_set_num_threads(num)

! Functions:
!$ omp_get_num_threads()  ! Number of used threads
!$ omp_get_thread_num()   ! Number of current thread
!$ omp_get_max_threads()  ! Number of threads of machine
!$ omp_get_num_procs()    ! Number of processors of machine
!$ omp_in_parallel()      ! If in parallel part of program
!$ omp_get_wtime()        ! Elapsed wall clock time
```

## Parallel regions
```{fortran}
!$omp parallel [clauses]
  ! parallel things here
!$omp end parallel
```

### Clauses

#### num_threads
How many threads are used for this region.

```{fortran}
!$omp parallel num_threads(4)
```

#### default, shared, private, firstprivate

 * `default(none)`: Assume nothing for unspecified variables
 * `shared(varlist)`: Only one shared instance of variable
 * `private(varlist)`: Every thread has his own instance (see `reduction` to combine)
 * `firstprivate(varlist)`: Like private but initial value gets set for each private instance

```{fortran}
!$omp parallel default(none) shared(m, x) private(i, n) firstprivate(j)
```

#### threadprivate, copyin
`threadprivate(varlist)`: Variables are private to thread but global within that thread (e.g. module variables)
```{fortran}
module m
  integer :: t, x
  !$omp threadprivate(t, x)
end module m
```

`copyin(varlist)`: Copy master thread's threadprivate variables into other threads' variables
```{fortran}
t = 0; x = 5
!$omp parallel copyin(t, x)
```

#### reduction
Accumulates results of each thread after parallel region.

Usage: `reduction(op : varlist)`
| op    | Initial value     |
|-------|-------------------|
| +     | 0                 |
| -     | 0                 |
| *     | 1                 |
| .and. | .true.            |
| .or.  | .false            |
| max   | INT_MAX, REAL_MAX |
| min   | INT_MIN, REAL_MAX |

```{fortran}
! Initialize!
a = 0; b = 0; c = 1
!$omp parallel reduction(+: a, b) reduction(*: c)
```

#### if
Only do parallel if logical condition is met.

```{fortran}
!$omp parallel if(n == 5)
```

## Workshare constructs

### single/master
Only executed by one thread:
 * If `single` then first reaching thread, other threads wait until `end single`
 * If `master` then thread 0, other thread don't wait

```{fortran}
!$omp parallel
  !$omp single

  !$omp end single

  !$omp master

  !$omp end master
!$omp end parallel
```

#### nowait
Threads don't wait for thread in `single`.
```{fortran}
!$omp single

!$omp end single nowait
```

#### copyprivate
Broadcast the value of private variable to other threads after `single`.
```{fortran}
!$omp parallel private(n)
  !$omp single
  !$omp end single copyprivate(n)
!$omp end parallel
```

### sections
(Different) parallel tasks for multiple threads.
```{fortran}
!$omp sections
  !$omp section
    ! thread0 does this
  !$omp section
    ! thread1 does this
  !$omp section
    ! thread2 does this
!$omp end sections
```

### do
Splits a do loop into threaded parts.
```{fortran}
!$omp parallel
  !$omp do
    do i=1,100

    end do
  !$omp end do
!$omp end parallel
```

#### collapse
Can use `!$omp do collapse(n)` do parallelise not only one but n nested loops

#### schedule
Can use `!$omp do schedule(type)` or `!$omp do schedule(type, chunk)` to schedule the loop.
 * Type `static`: Divide loop into equal chunks.
 * Type `dynamic`: First finished thread receives next chunk etc.
 * Type `guided`: Like dynamic, but changes chunk size to handle load imbalance. `chunk` is then the min chunk size.
 * Type `auto`: Delegate decision to compiler

### critical
All thread do something but one after the other.
```{fortran}
!$omp critical
```

### Others
`task`, `workshare`, `atomic`
