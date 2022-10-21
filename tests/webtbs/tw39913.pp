{ %opt=-O3 }
{$mode objfpc}{$H+}

type
  TDynCardinalArray = array of Cardinal;

function Test(N: Cardinal): TDynCardinalArray;
var
  L: Cardinal;
begin
  SetLength(Result, 0);
  if N <= 1 then
    Exit
  else
  begin
    L := 0;
    if N mod 2 = 0 then
    begin
      Inc(L);
      SetLength(Result, L);
      Result[L - 1] := 2;
    end;
    Inc(L);
    SetLength(Result, L);
    Result[L - 1] := N;
  end;
end;

begin
  Test(2);
  WriteLn('OK');
end.
