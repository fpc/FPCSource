{$mode objfpc}

type
  TForm1 = class
    function crash(n:integer):real;
  end;

function TForm1.crash(n:integer):real;
begin
  case n of
  0: Result:=0;
  1..100: Result:=crash(n-1)+crash(n-1);
  end;
end;

var
  f : TForm1;

begin
  f:=TForm1.create;
  writeln(f.crash(15));
  f.Free;
end.
