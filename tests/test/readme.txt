This directory contains tests for several parts of the compiler:
The tests ordered how they should be executed

Shortstrings .......... teststr.pp     compatibility and speed of
                                              string functions
                        teststr2.pp    some misc. tests mainly collected
                                       from bug reports
                        testcstr.pp    Typed Constant string loading from
                                       other constants
Ansistrings ........... testansi.pp
                        testa2.pp
Classes ............... testdom.pp
Exceptions ............ testexc.pp
                        testexc2.pp
                        testexc3.pp
Libraries ............. testlib.pp     a very primitive test
Parameter passing
via out ............... testout.pp

str/write(real_type) .. strreal.pp     test correct rounding
                        strreal2.pp    test correct writing of 10 till 1e-24
input/output .......... inoutres.pp    tests inoutres values of invalid
                                       operations
Units ................. testu1.pp      tests init. & finalization and halt
                        testu2.pp      in finalization
                        testu3.pp      a type redefining problem
                        testu4.pp
                        testu5.pp
case .................. testcase.pp    tests case statements with byte and word
                                       sized decision variables
                        testcas2.pp    tests case with sub enum types
Arrays ................ testarr1.pp    small test for open arrays with classes
Enumerations .......... testenm1.pp    tests assignments of subrange
                                       enumerations
Inline ................ inline01.pp    tests recursive inlining, inlining
                                       a procedure multiple times and
                                       inlining procedures in other
                                       inline procedures.
Finalize .............. testfi1.pp     tests the procedure system.finalize
TypeInfo .............. testti1.pp     test the function system.typeinfo
Resourcestrings ....... testrstr.pp    tests a simple resource string
