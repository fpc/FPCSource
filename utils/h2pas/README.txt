This is the h2pas program, a utility to convert C header files to pascal
units. It is part of the Free Pascal distribution.

COMPILING

To compile the program, a simple
 'make' 
should be sufficient; you need GNU make for this. When using TP, a simple
  tpc h2pas.pas
should also be possible. 

USAGE

h2pas [-p] [-t] [-o outputfilename] [-l libname] [-u unitname] filename

-t : Prepend 'T' to all type names in typedef definitions. This may help
     when the C header use uppercase types and lowercase variables of the
     same name.

-p : Use 'P' instead of ^ as a pointer symbol;
     This will convert 
        ^char to pchar
        ^longint to plongint 
     etc. It will also define a PSOMETYPE pointer for each SOMETYPE struct type 
     definition in the header file.
     Thus 
     typedef struct somestruct {
       ...
     }
     Will be converted to
     somestruct = record
       ...
     end;
     PSomestruct = ^Somestruct;
     If the -t options is used, the -p option takes care of that too.

-l : In the implementation part, the external functions will be
     written with 'external libname;' behind it.
     If you omit this option, all functions will be declared as 
     cdecl; external; 

-o : specify the outputname. By default, the inputname is used, with
     extension '.pp'.

-u : Specify the unit name. By default, the outputname is used, without
     extension.

-v : Replaces pointer types in parameter list by call by reference
     parameters:
        void p(int *i)  =>   procedure p(var i : longint);

Enjoy !
