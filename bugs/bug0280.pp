{$H+}

program memhole;

uses
   dpmiexcp;

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

begin
  writeln(memavail);
  dotest;
  writeln(memavail);
end.
