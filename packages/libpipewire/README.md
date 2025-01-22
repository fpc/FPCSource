# This package provides an interface to libPipeWire.

Compiling:

LibPipewire uses another "c library" called libspa.
Unfortunately, libspa is not a real library, it is a collection of macros.
This means there is nothing we can link to, to get libspa functionality.
(Pascal is not the only language with this problem)

To remedy this, a spabridge.o file is created by compiling a C file which
contains references to all spa routines in a large table. 
The C compiler then includes all the spa routines in the spabridge.o file.

This file is compiled using gcc when compiling the libpipewire package. 
It assumes that spa headers are available under /usr/include/spa-0.2

If the spa headers are installed in a different location, you can change the
fpmake.pp file and correct the include directory.

If you don't have the headers, you must install libpipewire-dev (which in turn installs libspa). 

On a debian machine, this means executing

```
apt-get install libpipewire-0.3-dev
```

If the file cannot be compiled, this will not cause an error in the build
procedure, instead, a warning is printed to indicate that spabridge.c cannot
be compiled. To see this message, add FPMAKEOPT=-v to the make command.
