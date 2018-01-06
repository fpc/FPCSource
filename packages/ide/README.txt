
You can have a fp.cfg file in the same directory as fp.exe. It works the
same as fpc.cfg

To compile the IDE you need to choose whether to build with or without
debugger (gdb) support. When debugger support is used a check will be
done if libgdb.a is available. If this is not available then it will
automatically fallback to create the IDE without debugger support.

To build with debugger support (default):

make gdb

To build without debugger support:

make nogdb


 
