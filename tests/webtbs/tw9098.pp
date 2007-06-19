uses variants;

var
  v: variant;
  i: longint;
begin
  v:=VarArrayOf([1, True, '123']);
  for i:=VarArrayLowBound(v, 1) to VarArrayHighBound(v, 1) do
    Writeln(v[i]);

  v:=VarArrayOf([]);
  if VarType(v) <> 8204 then begin
    writeln('Wrong vartype: ', VarType(v));
    Halt(1);
  end;
end.
