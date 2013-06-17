{ %fail }

program tmacfunret;
  {$MODE MACPAS}

  procedure B(var x: Integer);

  begin
    x:= 42;
  end;

  function A: Integer;

  begin
    B(A);
  end;

var
  i: Integer;

begin
  i:= A;
  Writeln(i);
  if i <> 42 then
    halt(1);
end.
