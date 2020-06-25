# MPI Cheatsheet

## Table of contents
  * [Basics](#basics)
    + [Hello World](#hello-world)
    + [Compilation](#compilation)
    + [Run](#run)
  * [Constants](#constants)
  * [Routines](#routines)
    + [Basic](#basic)
    + [Point to point communication](#point-to-point-communication)

## Basics

### Hello World
```{fortran}
program main
    use mpi
    implicit none

    integer :: ierr

    call MPI_Init(ierr)

    print *, "Hello world!"

    call MPI_Finalize(ierr)
end program main
```

### Compilation
```
mpifort name.f90
```

### Run
```
mpirun name
```
Options:
 - `-np` is optional and specifies how many threads to use.
 - `--hostfile host_file` specify hostnames when running on multiple machines


## Constants
```{fortran}
MPI_COMM_WORLD          # Communicator with all MPI processes

MPI_INTEGER             # integer(4)
MPI_REAL                # real(4)
MPI_DOUBLE_PRECISION    # real(8)
MPI_COMPLEX             # complex(4)
MPI_LOGICAL             # logical
MPI_CHARACTER           # character(1)

MPI_ANY_SOURCE          # Receive from any source
MPI_ANY_TAG             # Send/Receive to any tag

MPI_STATUS_SIZE         # use for integer :: status(MPI_STATUS_SIZE)
MPI_SOURCE              # use on status array as index
MPI_TAG                 # use on status array as index
MPI_ERROR               # use on status array as index
```


## Routines
**All variables are integers unless specified otherwise.**

### Basic
```{fortran}
call MPI_Init(ierr)                 # Initialize MPI
call MPI_Finalize(ierr)             # End MPI
call MPI_Abort(com, errcode, ierr)  # Terminates all ranks (even when just called by one)

call MPI_Comm_rank(com, me, ierr)   # Get rank number inside communicator
call MPI_Comm_size(com, n, ierr)    # Get number of ranks in communicator

MPI_Wtime()     # Returns time in seconds as real(8)
```

### Point to point communication
```{fortran}
any         :: buf
integer     :: status(MPI_STATUS_SIZE)

# Blocking (Wait until action completed)
call MPI_Send(buf, n, typ, dest, tag, comm, ierr)
call MPI_Rsend
call MPI_Bsend
call MPI_Ssend
call MPI_Recv(buf, n, typ, src, tag, comm, status, ierr)

# Non-Blocking (Resume program after call even if not completed)
call MPI_Isend
call MPI_Irsend
call MPI_Ibsend
call MPI_Issend
call MPI_Irecv
```
