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
  getmodulefilename(Hinstance,@p,255);
  writeln('DLL: Hello, I''m DLL ',pchar(@p));
end;

procedure P2(x:longint);export;
begin
  writeln('DLL: Argument X=',x);
  writeln('DLL: New teststr="',teststr,'"');
end;

procedure P3(var t);export;
begin
end;

procedure P4(x1:pointer);export;
begin
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
  Revision 1.1  1999-01-12 14:20:36  peter
    + dll example

}