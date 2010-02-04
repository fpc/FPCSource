
This package contains an interface unit to the libSEE library.

SEE = Simple ECMAScript Engine

libSEE is a library that executes Javascript code, and is written by David Leonard.

The library itself must be obtained separately from 
http://www.adaptive-enterprises.com.au/~d/software/see/
The library tarball contains installation instructions.
Make sure you compile it with Garbage collection.

the libsee unit was generated from the header files of version 3.1 of
libsee. Note that all type names have been prepended with T. That means
that the C struct
  SEE_interpreter
becomes
  TSEE_interpreter
in pascal.

The examples directory contains some examples of what can be done
with libsee, including how to integrate Object Pascal objects in the
Javascript runtime environment.

Enjoy,

Michael.