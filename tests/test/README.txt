This directory contains tests for several parts of the compiler and RTL


--------------------------------------------------------------------
                      Code generator
--------------------------------------------------------------------
These tests should be considered unitary, as they only verify
simple cases of the code generator. Used for porting to other
architectures. Tries to validate all possible Location types
valid for that node. They are based on tests on these
reference platforms:
  Borland Pascal v7.01
  Delphi 3.0
  Delphi 4.0
  Delphi 6.0 Personal Edition

'Natural type' is a signed 32-bit value on 32-bit architectures.
'Natural type' is a signed 64-bit value on 64-bit architectures.


--------------------------------------------------------------------
                        Compiler
--------------------------------------------------------------------


Shortstrings .......... tstring1.pp    compatibility and speed of shortstrings
                        tstring2.pp    some misc. tests mainly collected
                                       from bug reports
                        tstring3.pp    Typed Constant string loading from
                                       other constants
                        tstring4.pp    Ansistring #1
                        tstring5.pp    Ansistring #2
Classes ............... tclass1.pp     AfterConstruction
                        tclass2.pp     BeforeDestruction
Objects ............... tobject1.pp    Fail in constructor
Exceptions ............ texception1.pp
                        texception2.pp
                        texception3.pp
                        texception4.pp Math exceptions
Procedure Variable .... tprocvar1.pp
                        tprocvar2.pp
Libraries ............. testlib.pp     a very primitive test
Parameter passing ..... tpara1.pp      Out Parameter

Units ................. testu1.pp      tests init. & finalization and halt
                        testu2.pp      in finalization
                        testu3.pp      a type redefining problem
                        testu4.pp
                        testu5.pp
case .................. tcase1.pp      tests case statements with byte and word
                                       sized decision variables
                        tcase2.pp      tests case with sub enum types
Arrays ................ tarray1.pp     open arrays with classes
                        tarray2.pp     Array of const
                        tarray3.pp     Array of Char #1 (Known bug)
                        tarray4.pp     Array of Char #2 (Known bug)
Enumerations .......... tenum1.pp      tests assignments of subrange
                                       enumerations
Codegenerration ....... tcg1.pp        i386 pushw
                        tcg2.pp        saveregisters
                        tinivar.pp     initial values for local variables (1.1)
Inline ................ tinline1.pp    tests recursive inlining, inlining
                                       a procedure multiple times and
                                       inlining procedures in other
                                       inline procedures.
                        tinlin64.pp    tests for a problem in pushing 64bit parameters
                                       by value.
TypeInfo .............. trtti2.pp      test the function system.typeinfo
                        trtti3.pp      tests the procedure system.finalize
Resourcestrings ....... tresstr.pp     tests a simple resource string
Range checking ........ trange1.pp     range checking when converting int64/
                                       qword to longint/cardinal
                        trange2.pp     range checking when converting
                                       between longint and cardinal
                        trange3.pp     range checking for array
                        trange4.pp     range checking when assigning
                                       values to int64/qword
Floating Point ........ tfpu1.pp
                        tfpu2.pp
Assembler readers...... tasmread.pp    tests for support of unit or program specifier
                        testmovd.pp    testspecial issues about MOVD instruction
Variants............... tvariant.pp    tests the variant support of FPC
                        tasout.pp      tests a problem if a unit is compiled with nasm

Code Page strings       tpcstr1.pp     tests the new codepage string type introduced
                         ...           in the 'cpstrnew' branch.
                        tcpstrXX.pp

--------------------------------------------------------------------
                            RTL
--------------------------------------------------------------------

SYSTEM
------
str/write(real_type) .. tstrreal1.pp   test correct rounding
                        tstrreal2.pp   test correct writing of 10 till 1e-24
Heap .................. theap.pp       Heap manager test
input/output .......... /units/system/tiorte.pp   tests inoutres values of invalid operations
Random ................ /units/system/trandom.pp  tests random (interactive)

DOS
---
General .............. /units/dos/tdos.pp         (interactive)
General .............. /units/dos/testdos.pas     (interactive)
FExpand .............. /units/dos/tfexpand.pp

CRT
---
General .............. /units/crt/tcrt.pp          tests most crt unit functions (interactive)
