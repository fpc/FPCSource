{ Old file: tbs0280.pp }
{ problem with object finalization.                    OK 0.99.13 (FK) }

{$mode objfpc}
{$H+}

program memhole;

{$ifdef go32v2}
uses
   dpmiexcp;
{$endif go32v2}

type
  TMyClass = class
    s: String;
  end;
  plongint = ^longint;

procedure dotest;

var
  c: TMyClass;
  s : string;

begin
  s:='world';
  s:='Hallo '+s;
  writeln((plongint(s)-4)^);
  c := TMyClass.Create;
  writeln(longint(c.s));
  c.s := Copy('Test', 1, 4);
  writeln((plongint(c.s)-4)^);
  c.free;
end;

var
   membefore : longint;
begin
  membefore:=memavail;
  writeln(memavail);
  dotest;
  writeln(memavail);
  if membefore<>memavail then
    begin
      Writeln('Memory hole using ansi strings in classes');
      Halt(1);
    end
  else
    Writeln('No memory hole unsing ansi strings in classes');
end.
