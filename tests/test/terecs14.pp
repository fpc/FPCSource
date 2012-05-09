program terecs14;

{$mode objfpc}
{$modeswitch advancedrecords}
{$apptype console}
uses
  SysUtils;

{$define enableoperators}
{ $define enablebitwise}

type
  TFoo = record
    F: Integer;
    class operator Explicit(a: TFoo): Integer;
    class operator :=(a: TFoo): Integer;
    class operator =(a, b: TFoo): Boolean;
{$ifdef enableoperators}
    class operator <>(a, b: TFoo): Boolean;
{$endif}
    class operator In(a, b: TFoo): Boolean;
{$ifdef enableoperators}
    class operator > (a, b: TFoo): Boolean;
    class operator >= (a, b: TFoo): Boolean;
    class operator < (a, b: TFoo): Boolean;
    class operator <= (a, b: TFoo): Boolean;
{$endif}
    class operator + (a, b: TFoo): Integer;
    class operator - (a, b: TFoo): Integer;
    class operator * (a, b: TFoo): Integer;
    class operator / (a, b: TFoo): Integer;
    class operator div (a, b: TFoo): Integer;
    class operator mod (a, b: TFoo): Integer;
    class operator shl (a, b: TFoo): Integer;
    class operator shr (a, b: TFoo): Integer;
    class operator or(a, b: TFoo): Boolean;
    class operator and(a, b: TFoo): Boolean;
    class operator xor(a, b: TFoo): Boolean;
    class operator Not(a: TFoo): TFoo;
{$ifdef enablebitwise}
    class operator or(a, b: TFoo): TFoo;
    class operator and(a, b: TFoo): TFoo;
    class operator xor(a, b: TFoo): TFoo;
{$endif}
    class operator Inc(a: TFoo): TFoo;
    class operator Dec(a: TFoo): TFoo;
    class operator - (a: TFoo): TFoo;
    class operator + (a: TFoo): TFoo;
    // FPC operators
    class operator ** (a, b: TFoo): Integer;
{$ifdef enableoperators}
    class operator >< (a, b: TFoo): Integer;
{$endif}
 end;

class operator TFoo.Explicit(a: TFoo): Integer;
begin
  // to check the difference with implicit
  Result := a.F + 1;
end;

class operator TFoo.:=(a: TFoo): Integer;
begin
  Result := a.F;
end;

class operator TFoo.=(a, b: TFoo): Boolean;
begin
  Result := a.F = b.F;
end;

{$ifdef enableoperators}
class operator TFoo.<>(a, b: TFoo): Boolean;
begin
  Result := a.F <> b.F;
end;
{$endif}

class operator TFoo.In(a, b: TFoo): Boolean;
begin
  Result := a.F in [0..b.F];
end;

{$ifdef enableoperators}
class operator TFoo.>(a, b: TFoo): Boolean;
begin
  Result := a.F > b.F;
end;

class operator TFoo.>=(a, b: TFoo): Boolean;
begin
  Result := a.F >= b.F;
end;

class operator TFoo.<(a, b: TFoo): Boolean;
begin
  Result := a.F < b.F;
end;

class operator TFoo.<=(a, b: TFoo): Boolean;
begin
  Result := a.F <= b.F;
end;
{$endif}

class operator TFoo.+(a, b: TFoo): Integer;
begin
  Result := a.F + b.F;
end;

class operator TFoo.-(a, b: TFoo): Integer;
begin
  Result := a.F - b.F;
end;

class operator TFoo.*(a, b: TFoo): Integer;
begin
  Result := a.F * b.F;
end;

class operator TFoo./(a, b: TFoo): Integer;
begin
  Result := Round(a.F / b.F);
end;

class operator TFoo.div(a, b: TFoo): Integer;
begin
  Result := a.F div b.F;
end;

class operator TFoo.mod(a, b: TFoo): Integer;
begin
  Result := a.F mod b.F;
end;

class operator TFoo.shl(a, b: TFoo): Integer;
begin
  Result := a.F shl b.F;
end;

class operator TFoo.shr(a, b: TFoo): Integer;
begin
  Result := a.F shr b.F;
end;

class operator TFoo.or(a, b: TFoo): Boolean;
begin
  Result := (a.F or b.F) <> 0;
end;

class operator TFoo.and(a, b: TFoo): Boolean;
begin
  Result := (a.F and b.F) <> 0;
end;

class operator TFoo.xor(a, b: TFoo): Boolean;
begin
  Result := (a.F xor b.F) <> 0;
end;

class operator TFoo.not(a: TFoo): TFoo;
begin
  Result.F := not a.F;
end;

{$ifdef enablebitwise}
class operator TFoo.or(a, b: TFoo): TFoo;
begin
  Result.F := a.F or b.F;
end;

class operator TFoo.and(a, b: TFoo): TFoo;
begin
  Result.F := a.F and b.F;
end;

class operator TFoo.xor(a, b: TFoo): TFoo;
begin
  Result.F := a.F xor b.F;
end;
{$endif}

class operator TFoo.Inc(a: TFoo): TFoo;
begin
  Result.F := a.F + 1;
end;

class operator TFoo.Dec(a: TFoo): TFoo;
begin
  Result.F := a.F - 1;
end;

class operator TFoo.-(a: TFoo): TFoo;
begin
  Result.F := -a.F;
end;

class operator TFoo.+(a: TFoo): TFoo;
begin
  // to check that operator is called change the result value
  Result.F := a.F+1;
end;

class operator TFoo.**(a, b: TFoo): Integer;
var
  i: LongInt;
begin
  Result := 1;
  for i := 0 to b.F - 1 do
    Result := Result * a.F;
end;

{$ifdef enableoperators}
class operator TFoo.><(a, b: TFoo): Integer;
begin
  Result := a.F - b.F + 2 * (a.F + b.F);
end;
{$endif}

var
  a, b: TFoo;
  i: integer;
begin
  a.F := 1;
  b.F := 2;
  if a = b then
    halt(1);
{$ifdef enableoperators}
  if not (a <> b) then
    halt(2);
{$endif enableoperators}
  if not (a in b) then
    halt(3);
  if (b in a) then
    halt(4);
{$ifdef enableoperators}
  if a > b then
    halt(5);
  if a >= b then
    halt(6);
  if not (a < b) then
    halt(7);
  if not (a <= b) then
    halt(8);
{$endif}
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
  if a + b <> 3 then
    halt(15);
  if a - b <> -1 then
    halt(16);
  if not (a or b) then
    halt(17);
  if a and b then
    halt(18);
  if not (a xor b) then
    halt(19);
  if (not a).F <> (not 1) then
    halt(20);
{ bitwise operators current does not work if logical are defined
  if (a or b).F <> (a.F or b.F) then
    halt(21);
  if (a and b).F <> (a.F and b.F) then
    halt(22);
  if (a xor b).F <> (a.F xor b.F) then
    halt(23);
}
  inc(a);
  if a.F <> 2 then
    halt(24);
  dec(b);
  if b.F <> 1 then
    halt(25);
  i := b;
  if i <> 1 then
    halt(26);
  if Integer(b) <> 2 then
    halt(27);
  b := -b;
  if b.F <> -1 then
    halt(28);
  b := +b;
  if b.F <> 0 then
    halt(29);
  a.F := 2;
  b.F := 3;
  if (a ** b) <> 8 then
    halt(30);
{$ifdef enableoperators}
  if (a >< b) <> 9 then
    halt(31);
{$endif}
  WriteLn('ok');
end.
