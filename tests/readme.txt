  TESTS directory for FPC :

  several test programs for FPC 
  with compilation and execution tests.

  Standard way :
  'make all' will try to compile all the sources
   will printout a list of errors
  - programs that do not compile but should
  - programs that do compile when they should create an error !

  'make allexec' will try to run all non interactive executables
  'make alltesiexec' will try to run all interactive executables

  source files are separated in different pattern :

   ts*.pp 
   files that should compile and run without error (if programs !)
   
target 'allts' compiles all these files
    ts*.log contains the output of  the compiler
    ts*.res contains the return code (should be zero !)

target 'alltsexec' runs all these files
   they are run non interactively without arguments
   ts*.exc contains the return code should be zero
   (I basically added some halt(1) if the 
   execution is faulty !)
   ts*.elg contains the output of the program 

  tf*.pp 
  files that should fail on compilation
  target 'alltf' tries to compile all these files
  tf*.res should have a non zero value !!

  to*.pp special case for optimization
(treated like ts*.pp)

  test*.pp are treated like ts*.pp
but with targets 'alltest' and 'alltestexec'

  tesi*.pp are special cases of programs that require interactive
handling (readln or keypressed ...)
 these are only executed with tagert 'alltesiexec'

  Lastly :

   tbs*.pp are like ts*.pp 
but are translations from the bugs directory
(i.e. tests that the bug has been removed !!)

  tbf*.pp are like tf*.pp
  tis*.pp are like tesi*.pp 

