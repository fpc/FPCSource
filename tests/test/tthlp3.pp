{ this tests that the correct helper is used for variables }

program tthlp3;

{$mode objfpc}
{$apptype console}

uses
  uthlp;

procedure TestResult(aActual, aExpected, aError: LongInt);
begin
  if aActual <> aExpected then begin
    Writeln('Expected: ', aExpected, ' got: ', aActual);
    Halt(aError);
  end;
end;

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
  ta: TTestArray;
  p: Pointer;
  pl: PLongInt;
  v: Variant;
begin
  Writeln('Ordinal variables');
  TestResult(ui8.Test, 1, 1);
  TestResult(ui16.Test, 2, 2);
  TestResult(ui32.Test, 4, 3);
  TestResult(ui64.Test, 8, 4);
  TestResult(i8.Test, - 1, 5);
  TestResult(i16.Test, - 2, 6);
  TestResult(i32.Test, - 4, 7);
  TestResult(i64.Test, - 8, 8);
  Writeln('Boolean variables');
  TestResult(pb8.Test, 1, 9);
  TestResult(pb16.Test, 2, 10);
  TestResult(pb32.Test, 4, 11);
  TestResult(pb64.Test, 8, 12);
  TestResult(b8.Test, - 1, 13);
  TestResult(b16.Test, - 2, 14);
  TestResult(b32.Test, - 4, 15);
  TestResult(b64.Test, - 8, 16);
  Writeln('Float variables');
  TestResult(s.Test, 4, 17);
  TestResult(d.Test, 8, 18);
{$if sizeof(Extended) = sizeof(Double)}
  // expect the helper for Doubles
  TestResult(e.Test, 8, 19);
{$else}
  TestResult(e.Test, 10, 19);
{$endif}
  Writeln('Char variables');
  TestResult(ac.Test, - 1, 20);
  TestResult(wc.Test, - 2, 21);
  Writeln('String variables');
  TestResult(ss.Test, 1, 22);
  TestResult(_as.Test, 2, 23);
  TestResult(ws.Test, 3, 24);
  TestResult(us.Test, 4, 25);
  Writeln('Pointer variables');
  TestResult(p.Test, 1, 26);
  TestResult(pl.Test, 4, 27);
  Writeln('Other variables');
  TestResult(ml.Test, 42, 28);
  TestResult(te.Test, 1, 29);
  TestResult(ts.Test, 2, 30);
  TestResult(ta.Test, 0, 31);
  TestResult(v.Test, 3, 32);
  Writeln('OK');
end.
