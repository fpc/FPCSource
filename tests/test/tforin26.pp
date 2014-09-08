{$mode objfpc}
Function Write(Const Args: Array of const) : Integer;

Var
  V : TVarRec;
begin
  result:=0;
  For V in Args do
    begin
      if V.VType<>vtInteger then
        halt(1);
      inc(result);
    end;
end;

begin
  if Write([1,2,3,4])<>4 then
    halt(1);
  writeln('ok');
end.

