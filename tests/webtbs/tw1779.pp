{ %target=win32 }
{ Source provided for Free Pascal Bug Report 1779 }
{ Submitted by "Pierre" on  2002-01-25 }
{ e-mail: pierre@freepascal.org }

uses
  windows;

function GetLargestConsoleWindowSizeAlternate(h : longint) : dword;
  external 'kernel32' name 'GetLargestConsoleWindowSize';

var
  c1,c : coord;
  y : dword;
begin
  longint(c):=GetStdHandle(STD_OUTPUT_HANDLE);
  c1:=GetLargestConsoleWindowSize(GetStdHandle(STD_OUTPUT_HANDLE));
  Writeln('Max window size is ',c1.x,'x',c1.y);
  y:=GetLargestConsoleWindowSizeAlternate(GetStdHandle(STD_OUTPUT_HANDLE));
  c.x := y and $ffff;
  c.y:=  y shr 16;
  Writeln('Max window size is ',c.x,'x',c.y);
  if (c.x<>c1.X) or (c.Y<>c1.y) then
    begin
      Writeln('RTL bug');
      Halt(1);
    end;
end.
