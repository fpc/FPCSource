{ Old file: tbs0280.pp }
{ problem with object finalization.                    OK 0.99.13 (FK) }
{$mode objfpc}
{$H+}

uses
  Erroru;

type
  TMyClass = class
    s: String;
  end;

procedure dotest;

var
  c: TMyClass;
  s : string;

begin
  s:='world';
  s:='Hallo '+s;
  writeln((plongint(s)-4)^);
  c := TMyClass.Create;
  writeln(ptrint(c.s));
  c.s := Copy('Test', 1, 4);
  writeln((pptrint(c.s)-4)^);
  c.free;
end;

var
   mem : sizeint;
begin
  DoMem(mem);
  dotest;
  if DoMem(mem)<>0 then
    Halt(1);
end.
