This directory contains tests for several parts of the compiler:
The tests ordered how they should be executed

Shortstrings .......... teststr.pp     compatibility and speed of
                                              string functions
                        teststr2.pp    some misc. tests mainly collected
                                       from bug reports
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
