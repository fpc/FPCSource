{
  $Id$
  Copyright (c) 1998 by Pierre Muller

  Win32 DLL usage example. It needs testdll.pp
}
program dlltest;

procedure p1(x:pointer);
 external 'testdll' name 'P1';
procedure proc2(x:longint);
 external 'testdll' name 'Proc2';

var
   s : string;external 'testdll' name 'FPC_string';

begin
  writeln('Main: Hello!');
  p1(nil);
  writeln('Main: ',Hinstance,' ',Hprevinst);
  writeln('Main: testdll s string = ',s);
  s:='Changed by program';
  proc2(1234);
  writeln('Main: press enter');
  readln;
end.
{
  $Log$
  Revision 1.1  2001-05-03 21:39:34  peter
    * moved to own module

  Revision 1.2  2000/07/13 11:33:10  michael
  + removed logs
 
}
