program terecs6;

{$mode delphi}
{$apptype console}
uses
  SysUtils;

type
  TFoo = record
    F: Integer;
    class operator Equal(a, b: TFoo): Boolean;
    class operator NotEqual(a, b: TFoo): Boolean;
    class operator In(a, b: TFoo): Boolean;
    class operator GreaterThan(a, b: TFoo): Boolean;
    class operator GreaterThanOrEqual(a, b: TFoo): Boolean;
    class operator LessThan(a, b: TFoo): Boolean;
    class operator LessThanOrEqual(a, b: TFoo): Boolean;
    class operator Multiply(a, b: TFoo): Integer;
    class operator Divide(a, b: TFoo): Integer;
    class operator IntDivide(a, b: TFoo): Integer;
    class operator Modulus(a, b: TFoo): Integer;
    class operator LeftShift(a, b: TFoo): Integer;
    class operator RightShift(a, b: TFoo): Integer;
 end;

class operator TFoo.Equal(a, b: TFoo): Boolean;
begin
  Result := a.F = b.F;
end;

class operator TFoo.NotEqual(a, b: TFoo): Boolean;
begin
  Result := a.F <> b.F;
end;

class operator TFoo.In(a, b: TFoo): Boolean;
begin
  Result := a.F in [0..b.F];
end;

class operator TFoo.GreaterThan(a, b: TFoo): Boolean;
begin
  Result := a.F > b.F;
end;

class operator TFoo.GreaterThanOrEqual(a, b: TFoo): Boolean;
begin
  Result := a.F >= b.F;
end;

class operator TFoo.LessThan(a, b: TFoo): Boolean;
begin
  Result := a.F < b.F;
end;

class operator TFoo.LessThanOrEqual(a, b: TFoo): Boolean;
begin
  Result := a.F <= b.F;
end;

class operator TFoo.Multiply(a, b: TFoo): Integer;
begin
  Result := a.F * b.F;
end;

class operator TFoo.Divide(a, b: TFoo): Integer;
begin
  Result := Round(a.F / b.F);
end;

class operator TFoo.IntDivide(a, b: TFoo): Integer;
begin
  Result := a.F div b.F;
end;

class operator TFoo.Modulus(a, b: TFoo): Integer;
begin
  Result := a.F mod b.F;
end;

class operator TFoo.LeftShift(a, b: TFoo): Integer;
begin
  Result := a.F shl b.F;
end;

class operator TFoo.RightShift(a, b: TFoo): Integer;
begin
  Result := a.F shr b.F;
end;

var
  a, b: TFoo;
begin
  a.F := 1;
  b.F := 2;
  if a = b then
    halt(1);
  if not (a <> b) then
    halt(2);
  if not (a in b) then
    halt(3);
  if (b in a) then
    halt(4);
  if a > b then
    halt(5);
  if a >= b then
    halt(6);
  if not (a < b) then
    halt(7);
  if not (a <= b) then
    halt(8);
  if a * b <> 2 then
    halt(9);
  if a / b <> 0 then
    halt(10);
  if a div b <> 0 then
    halt(11);
  if a mod b <> 1 then
    halt(12);
  if a shl b <> 4 then
    halt(13);
  if b shr a <> 1 then
    halt(14);
  WriteLn('ok');
end.
