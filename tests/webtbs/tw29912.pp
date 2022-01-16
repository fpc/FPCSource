program Project1;

{$mode objfpc}{$H+}
{$R+,Q+,S+,T+}

var
  x,y,z:integer;
begin
  x:=0;
  z:=0;
  // all ok
  y:=Int64(x-1);
  writeln(y);
  // all ok
  y:=Int64(z);
  writeln(y);
  // arithmetic overflow
  y:=Int64(x-1)*Int64(z);
  writeln(y);
end.

