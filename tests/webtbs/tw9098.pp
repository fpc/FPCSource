uses variants;

var
  v: variant;
  i: longint;
begin
  v:=VarArrayOf([1, True, '123']);
  for i:=VarArrayLowBound(v, 1) to VarArrayHighBound(v, 1) do
    Writeln(v[i]);
    
  VarArrayRedim(v, -1);

  v:=VarArrayOf([]);
  if VarType(v) <> 8204 then begin
    writeln('Wrong vartype: ', VarType(v));
    Halt(1);
  end;
  
  if VarArrayHighBound(v, 1) <> -1 then begin
    writeln('Wrong high bound: ', VarArrayHighBound(v, 1));
    Halt(2);
  end;
end.
