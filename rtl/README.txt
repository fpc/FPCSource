This is the Run-Time Library (RTL) tree for Free Pascal.

To recompile the RTL, edit the main makefile. The makefiles NEED a GNU make
compatible make, they need unix-like 'rm' and 'mv' commands, as well as some
others. You can find these in the gnuutils package on the ftp site.

The main makefile is located ABOVE the RTL tree. It uses the FPC
makefile.fpc to guess reasonable defaults for everything it needs.
(these files can be found in base.zip on the FTP site)

The only variable that you may want to set are
FPC             - What compiler to use. Use an absolute path. 
                  (default is ppc386)
INSTALL_UNITDIR - Where to install the RTL units 
OPT             - any special options you want to set for the compiler.

In principle, you can also descend into the subdirectory of your OS, and
type 'make' there, that should also compile everything.

The tree contains subdirectories for all the supported operating systems,
as well as all processor architectures. The processor directories contain
low-level routines which are required for the system unit (if they are not 
available in high-level language form), as well as optimized versions of
the pascal generic routines (the generic routine source code is localed in
the inc subdirectory).

The following directories are not supported officially and may not work
correctly with FreePascal v1.0:

go32v1 - The DJGPP go32v1 DOS extender (no longer supported)
palmos - The PalmOS directory for the Dragonball (incomplete)


Enjoy.
