This directory contains test files for various FPC bugs.
The most files are very simple and it's neccessary to check the assembler
output.

The first coloumn contains the file name. If the file name is indended,
the bug is fixed and the last coloumn contains the version where
the bug is fixed.

In future, please add also your name short cut, when fixing a bug.

Fixed bugs:
-----------
  bug0001.pp    tests a bug in the .ascii output (#0 and too long)  OK 0.9.2
  bug0002.pp    tests for the endless bug in the optimizer          OK 0.9.2
  bug0003.pp    dito                                                OK 0.9.2
  bug0004.pp    tests the continue instruction in the for loop      OK 0.9.2
  bug0005.pp    tests the if 1=1 then ... bug                       OK 0.9.2
  bug0006.pp    tests the wrong floating point code generation      OK 0.9.2
  bug0007.pp    tests the infinity loop when using byte counter     OK 0.9.2
  bug0008.pp    tests the crash when decrementing constants         OK 0.9.2
  bug0009.pp    tests comperations in function calls a(c<0);        OK 0.9.2
  bug0010.pp    tests string constants exceeding lines              OK 0.9.2
  bug0011.pp    tests div/mod bug, where edx is scrambled, if
                a called procedure does a div/mod                   OK 0.9.2
  bug0012.pp      tests type conversation byte(a>b)                 OK 0.9.9 (FK)
  bug0015.pp    tests for wrong allocated register for return result
                of floating function (allocates int register)       OK 0.9.2
  bug0018.pp    tests for the possibility to declare all types
                using pointers "forward" : type p = ^x; x=byte;     OK 0.9.3
  bug0021.pp    tests compatibility of empty sets with other set
                and the evalution of constant sets                  OK 0.9.3
  bug0022.pp    tests getting the address of a method               OK 0.9.3
  bug0023.pp    tests handling of self pointer in nested methods    OK 0.9.3

  bug0025.pp    tests for a wrong uninit. var. warning              OK 0.9.3
  bug0026.pp    tests for a wrong unused. var. warning              OK 0.9.4
  bug0027.pp    tests
                type
                   enumtype = (One, two, three, forty:=40, fifty);  OK 0.9.5
  bug0028.pp    type enumtype = (a); writeln(ord(a));
  bug0029.pp      tests typeof(object type)                         OK 0.99.1 (FK)
  bug0030.pp    tests type conversations in typed consts            OK 0.9.6
  bug0031.pp    tests array[boolean] of ....                        OK 0.9.8
  bug0032.pp    tests for a bug with the stack                      OK 0.9.9
  bug0033.pp    tests var p : pchar; begin p:='c'; end.             OK 0.9.9
  bug0034.pp    shows wrong line numbering when asmbler is parsed   OK 0.9.9
                in direct mode.
  bug0035.pp    label at end of block gives error                   OK 0.9.9 (FK)
  bug0036.pp    assigning a single character to array of char      ?OK 0.9.9
                gives a protection error
  ---------     cgi386.pas gives out gpf's when compiling the system OK 0.9.9 (FK)
                unit.
  bug0037.pp    tests missing graph.setgraphmode                    OK RTL (FK)
  bug0038.pp    tests const ps : ^string = nil;                     OK 0.9.9 (FK)
  bug0039.pp    shows the else-else problem                         OK 0.9.9 (FK)
  bug0040.pp    shows the if b1 xor b2 problem where b1,b2 :boolean OK 0.9.9 (FK)
  bug0041.pp    shows the if then end. problem                      OK 0.9.9 (FK)
  bug0044.pp    shows $ifdef and comment nesting/directive problem  OK 0.99.1 (PFV)
  bug0045.pp    shows problem with virtual private methods          OK 0.9.9 (FK)
                (might not be a true bug but more of an incompatiblity?)
                the compiler warns now if there is a private and virtual
                method
  bug0046.pp    problems with sets with values over 128 due to      OK 0.99.1 (FK)
                sign extension
                 (already fixed ) but also for SET_IN_BYTE
  bug0047.pp    compiling with -So crashes the compiler             OK 0.99.1 (CEC)
  bug0053.pp    shows a problem with open arrays                     OK 0.99.1 (FK)
                (crashes a win95-DOS box :) )
  bug0054.pp   wordbool and longbool types are missed                OK 0.99.6 (PFV)
  bug0055.pp    internal error 10 (means too few registers           OK 0.99.1 (FK)
                - i386 ONLY)
  bug0056.pp    shows a _very_ simple expression which generates     OK 0.99.1 (FK)
                wrong assembler
  bug0058.pp    causes an internal error 10 (problem with getregisterOK 0.99.1 (FK)
                in secondsmaller - i386 ONLY)
  bug0059.pp    shows the problem with syntax error with ordinal     OK 0.99.1 (FK)
                constants
  bug0060.pp    shows missing type checking for case statements      OK 0.99.1 (CEC)
  bug0061.pp    shows wrong errors when compiling (NOT A BUG)        OK 0.99.1
  bug0062.pp    shows illegal type conversion for boolean            OK 0.99.6
  bug0064.pp    shows other types of problems with case statements   OK 0.99.1 (FK)
  bug0065.pp    shows that frac() doesn't work correctly.            OK 0.99.1 (PFV)
  bug0066.pp    shows that Round doesn't work correctly. (NOT A BUG) OK 0.99.1
  bug0067.pp and bug0067b.pp (Work together)                         OK 0.99.1
      Shows incorrect symbol resolution when using uses in implementation
      More info can be found in file bug0067b.pp.
  bug0068.pp   Shows incorrect type of ofs()                         OK 0.99.1 (PFV and FK)
  bug0069.pp    Shows problem with far qualifier in units            OK 0.99.1 (CEC)
  bug0071.pp    shows that an unterminated constant string in a      OK 0.99.1 (PFV)
                writeln() statement crashes the compiler.
  bug0072.pp    causes an internal error 10  ( i386 ONLY )           OK 0.99.1 (FK)
  bug0073.pp    shows incompatiblity with bp for distance qualifiers OK 0.99.6 (PFV)
  bug0074.pp    shows MAJOR bug when trying to compile valid code    OK 0.99.1 (PM/CEC)
  bug0075.pp   shows invalid pchar output to console                 OK 0.99.1
  ----------    compiling pp -Us -di386 -Sg system.pp gives GPF      OK 0.99.1
  bug0076.pp    Bug in intel asm generator. was already fixed        OK 0.99.1 (FK)
  bug0077.pp    shows a bug with absolute in interface part of unit  OK 0.99.1 (FK)
  bug0077b.pp   used by unit bug0077.pp
  bug0078.pp   Shows problems with longint constant in intel asm     OK 0.99.1 (CEC)
               parsers
  bug0079.pp   Shows problems with stackframe with assembler keyword OK 0.99.1 (CEC)
  bug0081.pp   Shows incompatibility with borland's 'array of char'. OK 0.99.1 (FK)
  bug0082.pp   Shows incompatibility with BP : Multiple destructors. OK 0.99.1 (FK)
  bug0084.pp   no more pascal type checking                          OK 0.99.1 (FK)
  bug0085.pp   shows runerror 216                                    OK 0.99.1 (CEC)
  bug0086.pp   shows runerror 216                                    OK 0.99.1 (CEC)
  bug0087.pp   shows internal error 12 - no more SegFaults           OK 0.99.1 (FK)
  bug0088.pp   internal error 12 or Runerror 216                     OK 0.99.1 (FK)
  bug0089.pp   internal error 12 or Runerror 216                     OK 0.99.1 (FK)
  bug0092.pp   The unfixable bug. Maybe we find a solution one day.  OK 0.99.6 (FK)
  bug0093.pp   Two Cardinal type bugs                                0K 0.99.1 (FK/MvC)
  bug0094.pp   internal error when recordtype not found with case    OK 0.99.1
  bug0095.pp   case with ranges starting with #0 bugs                OK 0.99.1 (FK)
  bug0096.pp   problem with objects as parameters                    OK 0.99.6 (PM)
  bug0097.pp   two errors in bp7 but not in FPC                      OK 0.99.6 (FK)
  bug0098.pp   File type casts are not allowed (works in TP7)        OK 0.99.1 (FK)
  bug0099.pp   wrong assembler code is genereatoed for range check   OK 0.99.1 (?)
               (at least under 0.99.0)
  bug0100.pp   a unit may only occure once in uses                   OK 0.99.6 (PM)
  bug0101.pp   no type checking for routines in interfance and       OK 0.99.1 (CEC)
                implementation
  bug0102.pp   page fault when trying to compile under ppcm68k       OK 0.99.1
  bug0103.pp   problems with boolean typecasts (other type)          OK 0.99.6 (PFV)
  bug0104.pp   cardinal greater than $7fffffff aren't written        OK 0.99.1 (FK)
               correct
  bug0105.pp   typecasts are now ignored problem (NOT A BUG)         OK 0.99.1
  bug0106.pp   typecasts are now ignored problem (NOT A BUG)         OK 0.99.1
  bug0108.pp   gives wrong error message                             OK 0.99.1 (PFV)
  bug0109.pp   syntax error not detected when using a set as pointer OK 0.99.1 (FK)
  bug0110.pp   SigSegv when using undeclared var in Case             OK 0.99.6 (PFV)
  bug0112.pp   still generates an internal error 10                  OK 0.99.1 (FK)
  bug0113.pp   point initialization problems                         OK 0.99.1 (PM/FK)
  bug0114.pp   writeln problem (by Pavel Ozerski)                    OK 0.99.1 (PFV)
  bug0115.pp   missing writeln for comp data type                    OK 0.99.6 (FK)
  bug0116.pp   when local variable size is > $ffff, enter can't be   OK 0.99.1 (FK)
               used to create the stack frame, but it is with -Og
  bug0117.pp   internalerror 17 (and why is there an automatic float OK 0.99.6 (FK)
               conversion?)
  bug0118.pp   Procedural vars cannot be assigned nil ?              OK 0.99.6 (FK)
  bug0119.pp   problem with methods                                  OK 0.99.6 (FK)
  bug0121.pp   cardinal -> byte conversion not work (and crashes)    OK 0.99.6 (FK)
  bug0122.pp   exit() gives a warning that the result is not set     OK 0.99.6 (FK)
  bug0125.pp   wrong colors with DOS CRT unit                        OK 0.99.6 (PFV)
  bug0126.pp   packed array isn't allowed                            OK 0.99.6 (FK)
  bug0128.pp   problem with ^[                                       OK 0.99.6 (PFV)
  bug0129.pp   endless loop with while/continue                      OK 0.99.6 (FK)
  bug0130.pp   in [..#255] problem                                   OK 0.99.6 (PFV)
  bug0134.pp   'continue' keyword is buggy.                          OK 0.99.6 (FK)
  bug0136.pp   No types necessary in the procedure header            OK 0.99.6 (PFV)
  bug0138.pp   with problem, %esi can be crushed and is not restored OK 0.99.6 (PM)
  bug0140.pp   Shows that interdependent units still are not OK.     OK 0.99.6 (PFV)

Unproducable bugs:
------------------
bug0048.pp   shows a problem with putimage on some computers
             (I can't reproduce the bug neither with a Millenium II
              nor a Trio64 card (FK)  )
bug0107.pp   shows page fault problem (run in TRUE DOS mode)
             (runs here fine with/without qemm under dos6.2 (PFV) )


Unfixed not important bugs (mostly incompatibilities):
------------------------------------------------------
bug0111.pp   blockread(typedfile,...) is not allowed in TP7
bug0133.pp   object type declaration not 100% compatibile with TP7


Unfixed bugs:
-------------
bug0042.pp   shows assembler double operator expression problem
bug0043.pp   shows assembler nasm output with fpu opcodes problem
bug0049.pp   shows an error while defining subrange types
bug0050.pp   can't set a function result in a nested procedure of a function
bug0051.pp   shows a problem with putpixel
bug0052.pp   collects missing graph unit routines
bug0057.pp   shows a crash with switch graph/text/graph
bug0063.pp   shows problem with ranges in sets for variables
bug0070.pp   shows missing include and exclude from rtl
bug0080.pp   Shows Missing High() (internal) function.
bug0083.pp   shows missing "dynamic" set constructor
bug0090.pp   shows PChar comparison problem
bug0091.pp   missing standard functions in constant expressions
bug0120.pp   inc/dec(enumeration) doesn't work
bug0123.pp   problem with intel assembler (shrd)
bug0124.pp   problem with -Rintel switch and indexing (whatever the order)
bug0127.pp   problem with cdecl in implementation part
bug0131.pp   internal error 10 with highdimension arrays
bug0132.pp   segmentation fault with type loop
bug0135.pp   Unsupported subrange type construction.
bug0137.pp   Cannot assign child object variable to parent objcet type variable
bug0139.pp   Cannot access protected method of ancestor class from other unit.
bug0141.pp   Wrong Class sizes when using forwardly defined classes.
bug0142.pp   sizeof(object) is not tp7 compatible when no constructor is used
bug0143.pp   cannot concat string and array of char in $X+ mode
bug0144.pp   problem with 'with object do'
bug0145.pp   typed files with huges records (needs filerec.size:longint)
bug0146.pp   no sizeof() for var arrays and the size is pushed incorrect
bug0147.pp   function b; is not allowed in implementation
bug0148.pp   triple fault exception when setting function result of a declared
             but not yet implemented function in another function
bug0149.pp  (a, b) compile bug0149b twice and you'll get a triple exc. fault
bug0150.pp  Shows that the assert() macro is missing under Delphi.
