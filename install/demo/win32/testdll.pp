{
  $Id$
  Copyright (c) 1998 by Pierre Muller

  Win32 DLL usage example. It needs dlltest.pp
}
library testdll;

function GetModuleFileName(hModule:longint;lpszPath:pchar;cchPath:longint):longint;
 external 'kernel32' name 'GetModuleFileNameA';
procedure beep(ID:longint);
 external 'user32' name 'MessageBeep';

var
  teststr : string;

procedure P1(var s:string);export;
var
  p:array[0..255] of char;
begin
  length(s);
  getmodulefilename(Hinstance,@p,255);
  writeln('DLL: Hello, I''m DLL ',pchar(@p));
end;

procedure P2(x:longint);export;
begin
  writeln('DLL: Argument X=',x);
  writeln('DLL: New teststr="',teststr,'"');
end;

procedure P3(var t);export;
var
  p : pointer;
begin
  p:=Addr(T);
  p:=p;
end;

procedure P4(x1:pointer);export;
begin
  Inc(x1);
end;

procedure NewExit;
begin
  beep(0);
  writeln('DLL: Exit from testdll');
end;

exports
 P1 index 1,
 P2 name 'Proc2',
 P3,
 P4 resident,
 teststr name 'FPC_string';

begin
  writeln('DLL: HInstance ',Hinstance,'  PrevInst ',Hprevinst,'  DLLReason ',DLLreason,'  DLLParam ',DLLparam);
  teststr:='DLL init done';
  exitproc:=@newExit;
end.
{
  $Log$
  Revision 1.5  2000-02-22 03:46:55  alex
  fixed the warning

  Revision 1.4  2000/01/21 00:44:51  peter
    * remove unused vars
    * renamed to .pp

  Revision 1.3  1999/10/26 12:33:53  peter
    * fixed illegal expr

  Revision 1.2  1999/06/30 22:04:56  michael
  * Added code to remove warnings

  Revision 1.1  1999/01/12 14:20:36  peter
    + dll example

}
