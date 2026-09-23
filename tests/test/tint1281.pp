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

function MK_INT128(const h, l: QWord): Int128;
var
  res: Int128;
begin
  FillChar(res, SizeOf(res), 0);
{$ifdef FPC_LITTLE_ENDIAN}
  Move(l, res, 8);
  Move(h, (PByte(@res)+8)^, 8);
{$else FPC_LITTLE_ENDIAN}
  Move(h, res, 8);
  Move(l, (PByte(@res)+8)^, 8);
{$endif FPC_LITTLE_ENDIAN}
  MK_INT128 := res;
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

procedure TestInt128ToString(const v: Int128; const expect_s: string);
var
  s: string;
begin
  s := IntToStr(v);
  if s <> expect_s then
    Error;
end;

procedure Int128ToStringTests;
begin
  TestInt128ToString(MK_INT128(                   0,                    0), '0');
  TestInt128ToString(MK_INT128(                   0,                    1), '1');
  TestInt128ToString(MK_INT128(                   0,                    9), '9');
  TestInt128ToString(MK_INT128(                   0,                   10), '10');
  TestInt128ToString(MK_INT128(                   0, 18446744073709551615), '18446744073709551615');
  TestInt128ToString(MK_INT128(                   1,                    0), '18446744073709551616');
  TestInt128ToString(MK_INT128( 9223372036854775807,                    0), '170141183460469231713240559642174554112');
  TestInt128ToString(MK_INT128( 9223372036854775807, 18446744073709551615), '170141183460469231731687303715884105727');
  TestInt128ToString(MK_INT128( 9223372036854775808,                    0), '-170141183460469231731687303715884105728');
  TestInt128ToString(MK_INT128( 9223372036854775808, 18446744073709551615), '-170141183460469231713240559642174554113');
  TestInt128ToString(MK_INT128(18446744073709551615,                    0), '-18446744073709551616');
  TestInt128ToString(MK_INT128(18446744073709551615, 18446744073709551615), '-1');
end;

procedure TestStringToUInt128(const s: string; const expect_v: UInt128);
var
  v: UInt128;
  c: ValSInt;
begin
  val_uint128(s, v, c);
  if c<>0 then
    Error;
  if v<>expect_v then
    Error;
end;

procedure StringToUInt128Tests;
begin
  TestStringToUInt128('0',                                       MK_UINT128(0, 0));
  TestStringToUInt128('1',                                       MK_UINT128(0, 1));
  TestStringToUInt128('9',                                       MK_UINT128(0, 9));
  TestStringToUInt128('10',                                      MK_UINT128(0, 10));
  TestStringToUInt128('18446744073709551615',                    MK_UINT128(                   0, 18446744073709551615));
  TestStringToUInt128('18446744073709551616',                    MK_UINT128(                   1,                    0));
  TestStringToUInt128('170141183460469231713240559642174554112', MK_UINT128( 9223372036854775807,                    0));
  TestStringToUInt128('170141183460469231731687303715884105727', MK_UINT128( 9223372036854775807, 18446744073709551615));
  TestStringToUInt128('170141183460469231731687303715884105728', MK_UINT128( 9223372036854775808,                    0));
  TestStringToUInt128('170141183460469231750134047789593657343', MK_UINT128( 9223372036854775808, 18446744073709551615));
  TestStringToUInt128('340282366920938463444927863358058659840', MK_UINT128(18446744073709551615,                    0));
  TestStringToUInt128('340282366920938463463374607431768211455', MK_UINT128(18446744073709551615, 18446744073709551615));
end;

begin
  UInt128ToStringTests;
  Int128ToStringTests;
  StringToUInt128Tests;
  Writeln('Ok!');
end.
