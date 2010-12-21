program toperator8;

{$mode objfpc}
{$apptype console}

type
  TFoo = record
    F: Integer;
  end;

  TBar = record
    F: Integer;
  end;

var
  Test: Integer = 0;

operator =(const F1, F2: TFoo): Boolean;
begin
  Result := F1.F = F2.F;
  Test := 1;
end;

operator <>(const F1, F2: TFoo): Boolean;
begin
  Result := F1.F <> F2.F;
  Test := 2;
end;

operator =(const F1, F2: TBar): Boolean;
begin
  Result := F1.F = F2.F;
  Test := 3;
end;

var
  F1, F2: TFoo;
  B1, B2: TBar;
begin
  F1.F := 1;
  F2.F := 2;
  if F1 = F2 then
    halt(1);
  if Test <> 1 then
    halt(2);
  F2.F := 1;
  if F1 <> F2 then
    halt(3);
  if Test <> 2 then
    halt(4);
  B1.F := 1;
  B2.F := 2;
  if B1 = B2 then
    halt(5);
  if Test <> 3 then
    halt(6);
  B2.F := 1;
  if B1 <> B2 then
    halt(7);
  WriteLn('ok');
end.

