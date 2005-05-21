{ %version=1.1 }

{$mode objfpc}
{$inline on}

type
  c = class
    l : longint;
    procedure p;inline;
    procedure p2;
  end;

    procedure c.p;inline;
    begin
      writeln(l);
      inc(l,10);
    end;

    procedure c.p2;
    begin
      l:=10;
      p;
      if l<>20 then
        halt(1);
    end;

var
  o : c;
begin
  o:=c.create;
  o.p2;
  o.free;
end.
