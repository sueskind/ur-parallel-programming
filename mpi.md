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
    + [Collective communication](#collective-communication)
    + [Reduction routines](#reduction-routines)
    + [MPI types](#mpi-types)
    + [Process topology and communicators](#process-topology-and-communicators)
    + [MPI-IO](#mpi-io)
      - [Individual data pointers](#individual-data-pointers)
      - [Shared data pointers](#shared-data-pointers)
      - [Explicit offset](#explicit-offset)
      - [File view](#file-view)

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
any         :: buf, buf2
integer     :: status(MPI_STATUS_SIZE)

# Blocking (Wait until action completed)
call MPI_Send(buf, n, typ, dest, tag, comm, ierr)   # Standard, chooses R,B,S on msg size
call MPI_Rsend(buf, n, typ, dest, tag, comm, ierr)  # Ready mode
call MPI_Bsend(buf, n, typ, dest, tag, comm, ierr)  # Buffered, small messages
call MPI_Ssend(buf, n, typ, dest, tag, comm, ierr)  # Synchronous, large messages

call MPI_Recv(buf, n, typ, src, tag, comm, status, ierr)

# Non-Blocking (Resume program after call even if not completed)
call MPI_Isend(buf, n, typ, dest, tag, comm, request, ierr)
call MPI_Irsend(buf, n, typ, dest, tag, comm, request, ierr)
call MPI_Ibsend(buf, n, typ, dest, tag, comm, request, ierr)
call MPI_Issend(buf, n, typ, dest, tag, comm, request, ierr)

call MPI_Irecv(buf, n, typ, src, tag, comm, request, ierr)

call MPI_Wait(request, status, ierr)      # Block until operation complete
call MPI_Test(request, ok, status, ierr)  # Test if operation complete

# Combined
call MPI_SendRecv(sbuf, scnt, styp, dest, tag, rbuf, rcnt, rtyp, src, tag, comm, status, ierr)
```

### Collective communication
Always called by all ranks equally.

```{fortran}
call MPI_Barrier(comm, ierr) # Synchronize ranks in communicator to this barrier

call MPI_Bcast(buf, cnt, typ, src, comm, ierr)                         # Broadcast from one to all
call MPI_Scatter(sbuf, scnt, styp, rbuf, rcnt, rtyp, src, comm, ierr)  # Spread field, opposite of gather

call MPI_Gather(sbuf, scnt, styp, rbuf, rcnt, rtyp, dest, comm, ierr)  # Collect all to one (scnt = rcnt)
call MPI_Allgather(sbuf, scnt, styp, rbuf, rcnt, rtyp, comm, ierr) # Gather + Bcast (Gather all to one, bcast those to all)

call MPI_Alltoall(sbuf, scnt, styp, rbuf, rcnt, rtyp, comm, ierr)
```

### Reduction routines
Always called by all ranks equally.

```{fortran}
call MPI_Reduce(local, global, cnt, type, typ, operation, dest, comm, ierr) # Result collected by one rank
call MPI_Allreduce(local, global, cnt, type, typ, operation, comm, ierr)    # Result collected by one, then bcasted

# Operations
MPI_MAX, MPI_MIN
MPI_SUM, MPI_PROD
MPI_LAND, MPI_LOR, MPI_LXOR   # logical
MPI_BAND, MPI_BOR, MPI_BXOR   # bitwise
MPI_MAXLOC, MPI_MINLOC        # value and location

call MPI_Op_create(func, commutes, opname, ierr)  # create reduce function
call MPI_Op_free(opname, ierr)                    # free after not needing it anymore
```

### MPI types
```{fortran}
integer :: typename

call MPI_Type_contiguous(cnt, typ, typename, ierr)                   # cnt fields of typ
call MPI_Type_vector(cnt, blocklength, stride, typ, typename, ierr)  # with empty spaces
call MPI_Type_indexed(cnt, blocklens, displs, typ, typename, ierr)   # different sized gaps, blocks
call MPI_Type_create_subarray
call MPI_Type_create_struct

call MPI_Type_commit(typename, ierr) # before type is usable
call MPI_Type_free(typename, ierr)   # after type isnt needed anymore
call MPI_Type_get_extent             # get type size in bytes
```

### Process topology and communicators
```{fortran}
call MPI_Dims_create()  # Balanced process distribution along cartesian grid
call MPI_Cart_create()  # Setup cartesian topology
call MPI_Cart_get()     # Return topology info
call MPI_Cart_rank()    # Get rank by cartesian coords
call MPI_Cart_coords()  # Get cartesian coords by rank number
call MPI_Cart_shift()   # Get neighbor ranks
call MPI_Neighbor_alltoall() # Send receive with neighbors

call MPI_Comm_dup()   # Duplicate communicator
call MPI_Comm_split() # Split based on color value

call MPI_Group_incl()     # Define group of processes
call MPI_Comm_create()    # Create communicator for that group

call MPI_Graph_create()   # General topology, define neighbors
```

### MPI-IO
```{fortran}
MPI_COMM_SELF   # Communicator with only this thread
MPI_INFO_NULL   # No info for info parameter

# modes
MPI_MODE_RDONLY           # read only
MPI_MODE_WRONLY           # write only
MPI_MODE_RDWR             # read write
MPI_MODE_CREATE           # create if not exists
MPI_MODE_EXCL             # error if file already exists
MPI_MODE_DELETE_ON_CLOSE  # delete on close
MPI_NODE_UNIQUE_OPEN      # file lock
MPI_MODE_SEQUENTIAL       # sequential access
MPI_MODE_APPEND           # set starting position to end of file

call MPI_File_open(comm, filename, mode, info, filehandle, ierr)
call MPI_File_close(handle, ierr)
call MPI_File_delete(filename, info, ierr)
call MPI_File_set_size(handle, size, ierr)
call MPI_File_get_size(handle, size, ierr)
```
#### Individual data pointers
Params: `(handle, buf, count, typ, status, ierr)`
```{fortran}
call MPI_File_read()
call MPI_File_write()

# collective
call MPI_File_read_all()
call MPI_File_write_all()

# non-blocking
call MPI_File_iread()
call MPI_File_iwrite()

# collective non-blocking
call MPI_File_read_all_begin()
call MPI_File_read_all_end()
call MPI_File_write_all_begin()
call MPI_File_write_all_end()
```
#### Shared data pointers
Params: `(handle, buf, count, typ, status, ierr)`
```{fortran}
call MPI_File_read_shared()
call MPI_File_write_shared()

# collective
call MPI_File_read_ordered()
call MPI_File_write_ordered()
call MPI_File_seek_shared()
call MPI_File_get_position_shared()

# non-blocking
call MPI_File_iread_shared()
call MPI_File_iwrite_shared()

# collective non-blocking
call MPI_File_read_ordered_begin()
call MPI_File_read_ordered_end()
call MPI_File_write_ordered_begin()
call MPI_File_write_ordered_end()
```
#### Explicit offset
Params: `(handle, offset, buf, count, typ, status, ierr)`
```{fortran}
call MPI_File_read_at()
call MPI_File_write_at()

# collective
call MPI_File_read_at_all()
call MPI_File_write_at_all()

# non-blocking
call MPI_File_iread_at()
call MPI_File_iwrite_at()

# collective non-blocking
call MPI_File_read_at_all_begin()
call MPI_File_read_at_all_end()
call MPI_File_write_at_all_begin()
call MPI_File_write_at_all_end()
```
#### File view
`datarep` is file format:
 - `'native'`: as in memory
 - `'internal'`: MPI format
 - `'external32'`: big-endian IEEE
```{fortran}
call MPI_File_set_view(handle, offset, typ, filetype, datarep, info, ierr)
call MPI_File_get_view(handle, offset, typ, filetype, datarep, ierr)
```
