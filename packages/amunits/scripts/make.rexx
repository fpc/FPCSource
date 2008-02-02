/*

  A simple make script for FPC Pascal.

  For your final release you can use this script
  to get the smallest possible program.

  If you are using the ms-dos cross compiler you
  can use this script to assemble and link your
  programs.
  This is what I started with, compiled all units
  on ms-dos and moved them over to my Amiga. There
  I assembled all to objectfiles. Now I could
  compile testprograms on ms-dos, move to Amiga
  and use this script to put it all together.

  Usage:

  rx make testprog.pas exec intuition graphics

  This will compile testprog.pas and link
  prt0.o, sysamiga.o, exec.o, intuition.o,
  graphics.o and testprog.o to testprog.

  rx make testprog.asm exec intuition graphics

  The same as above but it just assembles
  testprog.asm and links it.

  rx make testprog exec intuition graphics

  The same as above, treats testprog as an
  assembler file.


  Don't forget so set the correct paths for
  the binaries bellow.

  This is just a quick hack but it does work.

  nils.sjoholm@mailbox.swipnet.se

*/

SIGNAL ON BREAK_C
SIGNAL ON SYNTAX


parse arg main list

/*
  First parse the args and set up a list
*/

k = 0           œ
do while list ~= ''
parse var list keyword.k list
k=k+1
end

/*
  Set the correct path
*/

ASCOM    = 'dh1:fpc/bin/as'
LDCOM    = 'dh1:fpc/bin/ld'
UNITS    = 'dh1:fpc/units/'
SYSUNITS = 'dh1:fpc/lib/'
PPCCOM   = 'dh1:fpc/bin/ppc'
STRIPCOM = 'dh1:fpc/bin/strip'

/*
  Set the system units in the list
*/

linkline = SYSUNITS || 'prt0.o ' || SYSUNITS || 'sysamiga.o '

/*
  If there are more args, put in linklist
*/

do n=0 to k-1
linkline = linkline || UNITS || keyword.n || '.o'||' '
end

/*
  Check if it's  a pascal or assembler file
*/

parse var main head '.' ext
if upper(ext) = 'PAS' | upper(ext) = 'P' | upper(ext) = 'PP'  then do
   say 'Compiling ' || main
   address command PPCCOM || ' ' main || ' -Cn'
   if rc ~=0 then do
     say 'Problems with compiler'
     exit
   end
end
else do
   parse var main head '.' ext
   say 'Assembling ' || head
   address command ASCOM || ' ' || head || '.asm' || ' -o ' || head || '.o'
   if rc ~=0 then do
     say 'Problems with assembler'
   exit
   end
end

/*
  If we got here add to linklist
*/

linkline = linkline || head || '.o' || ' -o ' || head

/*
  Now link the files
*/

say 'Linking ' || head
address command LDCOM || ' ' || linkline
if rc ~=0 then do
  say 'Problems with linker'
  exit
  end

/*
  Use strip
*/

say 'Using Strip on ' || head
address command STRIPCOM || ' ' || head
if rc ~=0 then do
  say 'Problems with strip'
  exit
  end

say 'Done with ' || head
exit

BREAK_C:
SYNTAX:
SAY "Sorry, error line" SIGL ":" ErrorText(RC) ":-("
EXIT





