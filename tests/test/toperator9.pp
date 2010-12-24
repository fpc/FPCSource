program toperator9;

// Check IN operator support

{$mode objfpc}{$H+}
{$apptype console}

type
  TFoo = record
    F: Integer;
  end;

operator in(const I: Integer; const Foo: TFoo): Boolean;
begin
  Result := Foo.F = I;
end;

var
  Foo: TFoo;
begin
  Foo.F := 1;
  if not (1 in Foo) then
    halt(1);
  if 2 in Foo then
    halt(2);
  WriteLn('ok');
end.

