program tint1281;

uses
  uinteger128;

procedure Error;
begin
  Writeln('Error!');
  Halt(1);
end;

function MK_UINT128(const h, l: QWord): UInt128;
var
  res: UInt128;
begin
  FillChar(res, SizeOf(res), 0);
{$ifdef FPC_LITTLE_ENDIAN}
  Move(l, res, 8);
  Move(h, (PByte(@res)+8)^, 8);
{$else FPC_LITTLE_ENDIAN}
  Move(h, res, 8);
  Move(l, (PByte(@res)+8)^, 8);
{$endif FPC_LITTLE_ENDIAN}
  MK_UINT128 := res;
end;

procedure TestUInt128ToString(const v: UInt128; const expect_s: string);
var
  s: string;
begin
  s := IntToStr(v);
  if s <> expect_s then
    Error;
end;

procedure UInt128ToStringTests;
begin
  TestUInt128ToString(MK_UINT128(                   0,                    0), '0');
  TestUInt128ToString(MK_UINT128(                   0,                    1), '1');
  TestUInt128ToString(MK_UINT128(                   0,                    9), '9');
  TestUInt128ToString(MK_UINT128(                   0,                   10), '10');
  TestUInt128ToString(MK_UINT128(                   0, 18446744073709551615), '18446744073709551615');
  TestUInt128ToString(MK_UINT128(                   1,                    0), '18446744073709551616');
  TestUInt128ToString(MK_UINT128( 9223372036854775807,                    0), '170141183460469231713240559642174554112');
  TestUInt128ToString(MK_UINT128( 9223372036854775807, 18446744073709551615), '170141183460469231731687303715884105727');
  TestUInt128ToString(MK_UINT128( 9223372036854775808,                    0), '170141183460469231731687303715884105728');
  TestUInt128ToString(MK_UINT128( 9223372036854775808, 18446744073709551615), '170141183460469231750134047789593657343');
  TestUInt128ToString(MK_UINT128(18446744073709551615,                    0), '340282366920938463444927863358058659840');
  TestUInt128ToString(MK_UINT128(18446744073709551615, 18446744073709551615), '340282366920938463463374607431768211455');
end;

begin
  UInt128ToStringTests;
  Writeln('Ok!');
end.
