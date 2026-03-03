unit unitvariant1;

{$mode objfpc}

interface

procedure Run;

implementation

var v: variant;
s:string;

procedure Run;
begin
  v:='Hello';
  s:=v;
  writeln(s);
end;

end.
