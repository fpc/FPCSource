program toperator11;

{$mode objfpc}{$H+}
{$apptype console}

type
  TFoo = record
    F: Integer;
  end;

operator Inc(a: TFoo): TFoo;
begin
  Result.F := a.F + 1;
end;

operator Dec(a: TFoo): TFoo;
begin
  Result.F := a.F - 1;
end;

operator :=(a: TFoo): Integer;
begin
  Result := a.F;
end;

operator explicit(a: TFoo): Integer;
begin
  Result := a.F + 1;
end;

var
  a: TFoo;
  i: Integer;
begin
  a.F := 1;
  inc(a);
  if a.F <> 2 then
    halt(1);
  dec(a);
  if a.F <> 1 then
    halt(2);
  i := a;
  if i <> 1 then
    halt(3);
  if Integer(a) <> 2 then
    halt(4);
  WriteLn('ok');
end.

