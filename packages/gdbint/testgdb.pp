{
    $Id$
    Copyright (c) 1998 by Peter Vreman

    Small example program to the GDB

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program testgdb;
uses gdbcon;
var
  last,s,parafile : string;
  gdb : tgdbcontroller;
begin
  gdb.init;
  if paramcount=1 then
    parafile:=paramstr(1)
  else
    parafile:='test';
  gdb.loadfile(parafile);
  Writeln('Welcome to the pascal GDB...');
  Writeln('Type "q" to exit...');
  last:='';
  repeat
    write('>');
    readln(s);
    if (s='a') then
     gdb.starttrace
    else
     if (s='s') then
      gdb.tracestep
    else
     if (s='n') then
      gdb.tracenext
    else
     if (s='q') then
      break
    else
     begin
       if s='' then
         s:=last;
       GDB.Command(s);
       GDB.WriteErrorBuf;
       GDB.WriteOutputBuf;
       last:=s;
     end;
  until false;
  Writeln('End of pascal GDB...');
end.
{
  $Log$
  Revision 1.1  2000-07-13 06:33:58  michael
  + Initial import

  Revision 1.1  1999/11/24 23:36:33  peter
    * moved to packages dir

  Revision 1.1  1999/05/22 13:43:00  peter
    * moved

  Revision 1.4  1999/01/29 10:32:57  peter
    * fixes to compile with the makefile

  Revision 1.3  1999/01/22 10:23:51  peter
    * small update to get it working with the IDE

}

