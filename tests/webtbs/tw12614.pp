{$mode objfpc}
program testb;

Function IntRes : int64;

begin
  Result:=3;
end;

Var
  B : WordBool;

begin
  B:=WordBool(IntRes);
end.
