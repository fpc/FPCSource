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
  bug0042.pp    shows assembler double operator expression problem  OK 0.99.7 (PFV)
  bug0043.pp    shows assembler nasm output fpu opcodes problem     OK 0.99.6 (PFV)
  bug0044.pp    shows $ifdef and comment nesting/directive problem  OK 0.99.1 (PFV)
  bug0045.pp    shows problem with virtual private methods          OK 0.9.9 (FK)
                (might not be a true bug but more of an incompatiblity?)
                the compiler warns now if there is a private and virtual
                method
  bug0046.pp    problems with sets with values over 128 due to      OK 0.99.1 (FK)
                sign extension
                 (already fixed ) but also for SET_IN_BYTE
  bug0047.pp    compiling with -So crashes the compiler              OK 0.99.1 (CEC)
  bug0049.pp    shows an error while defining subrange types         OK 0.99.7 (PFV)
  bug0050.pp    can't set a function result in a nested procedure of a function OK 0.99.7 (PM)
  bug0051.pp    Graph, shows a problem with putpixel                 OK 0.99.9 (PM)
  bug0052.pp    Graph, collects missing graph unit routines          OK 0.99.9 (PM)
  bug0053.pp    shows a problem with open arrays                     OK 0.99.1 (FK)
                (crashes a win95-DOS box :) )
  bug0054.pp    wordbool and longbool types are missed               OK 0.99.6 (PFV)
  bug0055.pp    internal error 10 (means too few registers           OK 0.99.1 (FK)
                - i386 ONLY)
  bug0056.pp    shows a _very_ simple expression which generates     OK 0.99.1 (FK)
                wrong assembler
  bug0057.pp    Graph, shows a crash with switch graph/text/graph    OK 0.99.9 (PM)
  bug0058.pp    causes an internal error 10 (problem with getregisterOK 0.99.1 (FK)
                in secondsmaller - i386 ONLY)
  bug0059.pp    shows the problem with syntax error with ordinal     OK 0.99.1 (FK)
                constants
  bug0060.pp    shows missing type checking for case statements      OK 0.99.1 (CEC)
  bug0061.pp    shows wrong errors when compiling (NOT A BUG)        OK 0.99.1
  bug0062.pp    shows illegal type conversion for boolean            OK 0.99.6 (PFV)
  bug0063.pp    shows problem with ranges in sets for variables      OK 0.99.7 (PFV)
  bug0064.pp    shows other types of problems with case statements   OK 0.99.1 (FK)
  bug0065.pp    shows that frac() doesn't work correctly.            OK 0.99.1 (PFV)
  bug0066.pp    shows that Round doesn't work correctly. (NOT A BUG) OK 0.99.1
  bug0067.pp and bug0067b.pp (Work together)                         OK 0.99.1
      Shows incorrect symbol resolution when using uses in implementation
      More info can be found in file bug0067b.pp.
  bug0068.pp    Shows incorrect type of ofs()                        OK 0.99.1 (PFV and FK)
  bug0069.pp    Shows problem with far qualifier in units            OK 0.99.1 (CEC)
  bug0070.pp    shows missing include and exclude from rtl           OK 0.99.6 (MVC)
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
  bug0080.pp   Shows Missing High() (internal) function.             OK 0.99.6 (MVC)
  bug0081.pp   Shows incompatibility with borland's 'array of char'. OK 0.99.1 (FK)
  bug0082.pp   Shows incompatibility with BP : Multiple destructors. OK 0.99.1 (FK)
  bug0083.pp   shows missing "dynamic" set constructor               OK 0.99.7 (PFV)
  bug0084.pp   no more pascal type checking                          OK 0.99.1 (FK)
  bug0085.pp   shows runerror 216                                    OK 0.99.1 (CEC)
  bug0086.pp   shows runerror 216                                    OK 0.99.1 (CEC)
  bug0087.pp   shows internal error 12 - no more SegFaults           OK 0.99.1 (FK)
  bug0088.pp   internal error 12 or Runerror 216                     OK 0.99.1 (FK)
  bug0089.pp   internal error 12 or Runerror 216                     OK 0.99.1 (FK)
  bug0090.pp   shows PChar comparison problem                        OK 0.99.7 (PFV)
  bug0091.pp   missing standard functions in constant expressions    OK 0.99.7 (PFV)
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
  bug0107.pp   shows page fault problem (run in TRUE DOS mode)       OK ??.??
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
  bug0120.pp   inc/dec(enumeration) doesn't work                     OK 0.99.6 (MVC)
  bug0121.pp   cardinal -> byte conversion not work (and crashes)    OK 0.99.6 (FK)
  bug0122.pp   exit() gives a warning that the result is not set     OK 0.99.6 (FK)
  bug0125.pp   wrong colors with DOS CRT unit                        OK 0.99.6 (PFV)
  bug0126.pp   packed array isn't allowed                            OK 0.99.6 (FK)
  bug0127.pp   problem with cdecl in implementation part             OK 0.99.7 (PFV)
  bug0128.pp   problem with ^[                                       OK 0.99.6 (PFV)
  bug0129.pp   endless loop with while/continue                      OK 0.99.6 (FK)
  bug0130.pp   in [..#255] problem                                   OK 0.99.6 (PFV)
  bug0131.pp   internal error 10 with highdimension arrays           OK 0.99.6 (MVC)
  bug0132.pp   segmentation fault with type loop                     OK 0.99.7 (FK)
  bug0134.pp   'continue' keyword is buggy.                          OK 0.99.6 (FK)
  bug0135.pp   Unsupported subrange type construction.               OK 0.99.6
  bug0136.pp   No types necessary in the procedure header            OK 0.99.6 (PFV)
  bug0137.pp   Cannot assign child object variable to parent objcet type variable OK 0.99.6
  bug0138.pp   with problem, %esi can be crushed and is not restored OK 0.99.6 (PM)
  bug0139.pp   Cannot access protected method of ancestor class from other unit. OK 0.99.6
  bug0140.pp   Shows that interdependent units still are not OK.     OK 0.99.6 (PFV)
  bug0141.pp   Wrong Class sizes when using forwardly defined classes. OK 0.99.6
  bug0142.pp   sizeof(object) is not tp7 compatible when no constructor is used OK 0.99.9 (PM)
  bug0143.pp   cannot concat string and array of char in $X+ mode    OK 0.99.7 (PFV)
  bug0144.pp   problem with 'with object do'                         OK 0.99.7 (PFV)
  bug0145.pp   typed files with huges records (needs filerec.size:longint) OK 0.99.7 (PFV)
  bug0146.pp   no sizeof() for var arrays and the size is pushed incorrect OK 0.99.7 (PFV)
  bug0147.pp   function b; is not allowed in implementation          OK 0.99.7 (PFV)
  bug0148.pp   crash when setting function result of a declared but not yet OK 0.99.7 (PFV)
               implemented function in another function
  bug0149.pp   (a, b) compile bug0149b twice and you'll get a crash  OK 0.99.7 (PFV)
  bug0150.pp   Shows that the assert() macro is missing under Delphi OK 0.99.9 (PFV)
  bug0151.pp   crash when using undeclared variable in withstatement OK 0.99.7 (PFV)
  bug0152.pp   End value of loop variable must be calculated before loop
               variable is initialized.                              OK 0.99.11 (PM)
  bug0153.pp   Asm, indexing a local/para var should produce an error like tp7 OK 0.99.9 (PFV)
  bug0154.pp   Subrange types give type mismatch when assigning to   OK 0.99.7 (PFV)
  bug0156.pp   (a,b) forward type def in record crashes when loading ppu OK 0.99.7 (PM/PFV)
  bug0155.pp   Asm, Missing string return for asm functions
               (this is a feature rather than a bug :               OK 0.99.11 (FK)
                complex return values are not allowed for assembler
                functions (PM) Why not (FK)? )
  bug0157.pp   Invalid compilation and also crashes                  OK 0.99.7 (PFV)
  bug0158.pp   Invalid boolean typecast                              OK 0.99.7 (PFV)
  bug0159.pp   Invalid virtual functions - should compile            OK 0.99.7 (FK)
  bug0160.pp   Incompatibility with BP: Self shouldn't be a reserved word. OK 0.99.9 (PM)
  bug0161.pp   internal error when trying to create a set with another OK 0.99.9 (PFV)
  bug0162.pp   continue in repeat ... until loop doesn't work correct OK 0.99.8 (PFV)
  bug0163.pp   missing <= and >= operators for sets.                 OK 0.99.11 (JM)
  bug0164.pp   crash when using undeclared array index in with statement OK 0.99.8 (PFV)
  bug0165.pp   missing range check code for enumerated types.            OK 0.99.9 (PFV)
  bug0166.pp   forward type used in declaration crashes instead of error OK 0.99.9 (PFV)
  bug0167.pp   crash when declaring a procedure with same name as object OK 0.99.9 (PFV)
  bug0168.pp   set:=set+element is allowed (should be: set:=set+[element]) OK 0.99.9 (PFV)
  bug0169.pp   missing new(type) support for not object/class             OK 0.99.9 (PM)
  bug0170.pp   Asm, {$ifdef} is seen as a separator                  OK 0.99.9 (PFV)
  bug0171.pp   missing typecasting in constant expressions
               solved for pointers                                   OK 0.99.11 (PM)
  bug0172.pp   with with absolute seg:ofs should not be possible OK 0.99.9 (PM)
  bug0173.pp   secondbug is parsed as asm, but should be normal pascalcode OK 0.99.9 (PFV)
  bug0174.pp   Asm, offsets of fields are not possible yet           OK 0.99.9 (PFV)
  bug0175.pp   Asm, mov word,%eax should not be allowed without casting
               emits a warning (or error with range checking enabled)  OK 0.99.11 (PM)
  bug0176.pp   unit.symbol not allowed for implementation vars         OK 0.99.9 (PM)
  bug0177.pp   program.symbol not allowed (almost the same as bug 176) OK 0.99.9 (PM)
  bug0178.pp   problems with undefined labels and fail outside constructor OK 0.99.9 (PM)
  bug0179.pp   show a problem for -So mode                           OK 0.99.9 (PM)
  bug0180.pp   problem for units with names different from file name
                 should be accepted with -Un !!
               Solved, but you still need to use the file name from other
               units                                                 OK 0.99.9 (PM)
  bug0181.pp   shows a problem with name mangling                    OK 0.99.9 (PM)
  bug0182.pp   @record.field doesn't work in constant expr           OK 0.99.9 (PM)
  bug0183.pp   internal error 10 in secondnot                        OK 0.99.11 (PM)
  bug0184.pp   multiple copies of the same constant set are stored in executable OK 0.99.9 (PFV)
  bug0186.pp   Erroneous array syntax is accepted.                   OK 0.99.9 (PFV)
  bug0187.pp   constructor in a WIth statement isn't called correct.
               (works at lest in the case stated)                    OK 0.99.11 (PM)
  bug0188.pp   can't print function result of procedural var that returns a
               function. Not a bug : wrong syntax !! See source (PM)
  bug0189.pp   cant compare adresses of function variables !!
               As bug0188 FPC syntax problem see source (PM)
  bug0190.pp   can't have typecast for var params ??                 OK 0.99.11 (PM)
  bug0191.pp   missing vecn constant evaluation                      OK 0.99.11 (PM)
  bug0192.pp   can't compare boolean result with true/false, because the
               boolean result is already in the flags             OK 0.99.11 (PFV)
  bug0194.pp   @procedure var returns value in it instead of address !! OK 0.99.11 (PM)
  bug0195.pp   Problem with Getimage, crash of DOS box, even with dpmiexcp!! (PFV)
               Not a bug, you must use p^.
  bug0196.pp   "function a;" is accepted (should require result type) OK 0.99.1 (PM)
  bug0197.pp   should produce an error: problem with c1:=c2<c3 where c? is OK 0.99.11 (PM)
               a comp type
  bug0198.pp   calling specifications aren't allowed in class declarations,
               this should be allowed                                OK 0.99.11  (PM)
  bug0199.pp   bug in mul code                                       OK 0.99.11  (FK)
  bug0200.pp   array of char overloading problem with strings        OK 0.99.11 (PFV)
  bug0201.pp   problem with record var-parameters and assembler      OK 0.99.11 (PFV)
  bug0202.pp   flag results not supported with case                  OK 0.99.11 (PFV)
  bug0203.pp   problem with changed mangledname of procedures after use
               Generates an error now                                OK 0.99.11 (PM)
  bug0204.pp   can typecast the result var in an assignment          OK 0.99.11 (PM)
  bug0205.pp   and parsing bug, generates wrong code (tp7 gives parser error) OK 0.99.11 (PM)
  bug0206.pp   sets with variable ranges doesn't work                OK 0.99.11 (PFV)
  bug0207.pp   a class destructor doesn't release the memory        OK 0.99.11 (FK)
  bug0208.pp   implicit conversion from boolean to longint should not be allowed
               (this is the reason of bug0205 !)                    OK 0.99.11 (PM)
  bug0209.pp   problem with boolean expressions of different store sizes
               (problem created while solving bug205 ! PM)          OK 0.99.11 (PM)
  bug0210.pp   fillchar should accept boolean value also !!         OK 0.99.11 (PM)
  bug0211.pp   a and not a is true !!! (if a:=boolean(5))           OK 0.99.11 (PM)
  bug0213.pp   name mangling problem with nested procedures in overloaded
               procedure                                            OK 0.99.11 (PM)
  bug0214.pp   bug for static methods                               OK 0.99.11 (PM)
  bug0215.pp   more bugs with static methods                        OK 0.99.11 (PM)
  bug0216.pp   problem with with fields as function args            OK 0.99.11 (PM)
  bug0218.pp   rounding errors with write/str (the bug is fixed,    OK 0.99.11 (FK)
               but there is still some rounding error left when
               writing the extended value PFV;
               this is also fixed now by using integer constants
               in str and val FK)
  bug0219.pp   wrong error message                                  OK 0.99.11 (PFV)
  bug0220.pp   array of char overloading problem with strings        OK 0.99.11 (PFV)
  bug0223.pp   wrong boolean evaluation in writeln                  OK 0.99.11 (PFV)
  bug0224.pp   I/O-Error generation in readln can't be switched off OK 0.99.11 (PFV)


Unproducable bugs:
------------------
bug0048.pp   shows a problem with putimage on some computers
             (I can't reproduce the bug neither with a Millenium II
              nor a Trio64 card (FK)  )
             (maybe fixed : there was a problem in SetVESAmode for cards
              that use a different read and write window !!
              0.99.9  (PM) )


Unfixed not important bugs (mostly incompatibilities):
------------------------------------------------------
bug0111.pp   blockread(typedfile,...) is not allowed in TP7
bug0133.pp   object type declaration not 100% compatibile with TP7
bug0193.pp   overflow checking for 8 and 16 bit operations wrong
             overflow are just special range checks so
             as all operations are done on 32 bit integers in FPC
             overflow checking will only trap 32 bit overflow
             you have to use range checks for byte or 16 bit integers
1.pp         produces a linker error under win32/linux, sorry for the filename
             but the filename is the bug :)

Unfixed bugs:
-------------
bug0123.pp   Asm, problem with intel assembler (shrd)
bug0124.pp   Asm, problem with -Rintel switch and indexing (whatever the order)
bug0226.pp   Asm, offset of var is not allowed as constant
bug0228.pp   Asm, wrong warning for size

bug0185.pp   missing range checking for Val and subrange types
bug0212.pp   problem with properties
bug0217.pp   in tp mode can't use the procvar in writeln
bug0221.pp   syntax parsing incompatibilities with tp7
bug0222.pp   an record field can't be the counter index (compiles with TP)
bug0225.pp   Sigsegv when run with range checks on open arrays
bug0227.pp   external var does strange things when declared in localsymtable
bug0229.pp   consts > 255 are truncated (should work in -S2,-Sd)
bug0230.pp   several strange happen on the ln function: ln(0): no
             FPE and writeln can't write non numeric values
