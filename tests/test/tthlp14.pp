{ this tests that constructors in helpers are working correctly }

program tthlp14;

{$mode objfpc}
{$apptype console}

uses
  uthlp, math;

var
  ui8: UInt8;
  ui16: UInt16;
  ui32: UInt32;
  ui64: UInt64;
  i8: Int8;
  i16: Int16;
  i32: Int32;
  i64: Int64;
  s: Single;
  d: Double;
  e: Extended;
  ss: ShortString;
  _as: AnsiString;
  ws: WideString;
  us: UnicodeString;
  ac: AnsiChar;
  wc: WideChar;
  pb8: Boolean;
  pb16: Boolean16;
  pb32: Boolean32;
  pb64: Boolean64;
  b8: ByteBool;
  b16: WordBool;
  b32: LongBool;
  b64: QWordBool;
  ml: MyLongInt;
  ts: TTestSet;
  te: TTestEnum;
  ta, ta2: TTestArray;
  p: Pointer;
  pl: PLongInt;
  v: Variant;
begin
  Writeln('Ordinal variables');
  ui8 := UInt8.Create(42);
  if ui8 <> 42 then
    Halt(1);
  ui16 := UInt16.Create(42);
  if ui16 <> 42 then
    Halt(2);
  ui32 := UInt32.Create(42);
  if ui32 <> 42 then
    Halt(3);
  ui64 := UInt64.Create(42);
  if ui64 <> 42 then
    Halt(4);
  i8 := Int8.Create(42);
  if i8 <> 42 then
    Halt(5);
  i16 := Int16.Create(42);
  if i16 <> 42 then
    Halt(6);
  i32 := Int32.Create(42);
  if i32 <> 42 then
    Halt(7);
  i64 := Int64.Create(42);
  if i64 <> 42 then
    Halt(8);
  Writeln('Boolean variables');
  pb8 := Boolean.Create(True);
  if not pb8 then
    Halt(9);
  pb16 := Boolean16.Create(True);
  if not pb16 then
    Halt(10);
  pb32 := Boolean32.Create(True);
  if not pb32 then
    Halt(11);
  pb64 := Boolean64.Create(True);
  if not pb64 then
    Halt(12);
  b8 := ByteBool.Create(True);
  if not b8 then
    Halt(13);
  b16 := WordBool.Create(True);
  if not b16 then
    Halt(14);
  b32 := LongBool.Create(True);
  if not b32 then
    Halt(15);
  b64 := QWordBool.Create(True);
  if not b64 then
    Halt(16);
  Writeln('Float variables');
  s := Single.Create(4.2);
  if not SameValue(s, Single(4.2), 1e-100) then
    Halt(17);
  d := Double.Create(4.2);
  if not SameValue(d, Double(4.2), 1e-100) then
    Halt(18);
{$if sizeof(Extended) <> sizeof(Double)}
  e := Extended.Create(4.2);
  if not SameValue(e, Extended(4.2), 1e-100) then
    Halt(19);
{$endif}
  Writeln('Char variables');
  ac := AnsiChar.Create('a');
  if ac <> 'a' then
    Halt(20);
  wc := WideChar.Create(#$1234);
  if wc <> #$1234 then
    Halt(21);
  Writeln('String variables');
  ss := ShortString.Create('Test');
  if ss <> 'Test' then
    Halt(22);
  _as := AnsiString.Create('Test');
  if _as <> 'Test' then
    Halt(23);
  ws := WideString.Create(#$1234#$4321);
  if ws <> #$1234#$4321 then
    Halt(24);
  us := UnicodeString.Create(#$1234#$4321);
  if us <> #$1234#$4321 then
    Halt(25);
  Writeln('Pointer variables');
  p := Pointer.Create(@p);
  if p <> @p then
    Halt(26);
  pl := PLongInt.Create(@pl);
  if pl <> @pl then
    Halt(27);
  Writeln('Other variables');
  ml := MyLongInt.Create(42);
  if ml <> 42 then
    Halt(28);
  te := TTestEnum.Create(teOne);
  if te <> teOne then
    Halt(29);
  ts := TTestSet.Create([teOne, teTwo]);
  if ts <> [teOne, teTwo] then
    Halt(30);
  SetLength(ta2, 2);
  ta2[0] := 42;
  ta2[1] := 21;
  ta := TTestArray.Create(ta2);
  if (Length(ta) <> 2) or (ta[0] <> ta2[0]) or (ta[1] <> ta2[1]) then
    Halt(31);
  v := Variant.Create(42);
  if v <> 42 then
    Halt(32);
  Writeln('OK');
end.
