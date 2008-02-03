{$MODE Objfpc}

type
  Ta = class 
    T: array of Double;
  end;

var
  a: Ta;

function P:Ta;
begin
  Result := a;
end;

function M: Double;
begin
  Result := 300;
end;

var
  i: Integer;

begin
  a := Ta.Create;
  SetLength(P.T,2);
  P.T[0] := 70;
  P.T[1] := 80;
  i := 0;
  while (i < Length(P.T)) and (M > P.T[i]) do
    Inc(i);
  if (i<>2) then
    halt(1);
end.
