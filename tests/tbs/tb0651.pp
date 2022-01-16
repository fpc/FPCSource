program tb0651;

{$mode objfpc}{$H+}

type
  TBooleanArray = array[0..7] of Boolean;

  TBooleanByte = bitpacked array[0..7] of Boolean;
  TBoolean16Byte = bitpacked array[0..7] of Boolean16;
  TBoolean32Byte = bitpacked array[0..7] of Boolean32;
  TBoolean64Byte = bitpacked array[0..7] of Boolean64;
  TByteBoolByte = bitpacked array[0..7] of ByteBool;
  TWordBoolByte = bitpacked array[0..7] of WordBool;
  TLongBoolByte = bitpacked array[0..7] of LongBool;
  TQWordBoolByte = bitpacked array[0..7] of QWordBool;

generic procedure CheckValue<T>(aArr: T; const aExpected: TBooleanArray; aCode: LongInt);
var
  i: LongInt;
begin
  if SizeOf(T) <> 1 then
    Halt(aCode * 10 + 1);
  if BitSizeOf(T) <> 8 then
    Halt(aCode * 10 + 2);
  for i := 0 to High(aArr) do
    if aArr[i] <> aExpected[i] then
      Halt(aCode * 10 + 3 + i);
end;

var
  exp: TBooleanArray = (True, False, True, False, False, True, False, True);
  b: Byte = $A5;
  pb8: TBooleanByte absolute b;
  pb16: TBoolean16Byte absolute b;
  pb32: TBoolean32Byte absolute b;
  pb64: TBoolean64Byte absolute b;
  bb8: TByteBoolByte absolute b;
  bb16: TWordBoolByte absolute b;
  bb32: TLongBoolByte absolute b;
  bb64: TQWordBoolByte absolute b;
begin
  specialize CheckValue<TBooleanByte>(pb8, exp, 0);
  specialize CheckValue<TBoolean16Byte>(pb16, exp, 1);
  specialize CheckValue<TBoolean32Byte>(pb32, exp, 2);
{$ifdef CPU64}
  specialize CheckValue<TBoolean64Byte>(pb64, exp, 3);
{$endif}
  specialize CheckValue<TByteBoolByte>(bb8, exp, 4);
  specialize CheckValue<TWordBoolByte>(bb16, exp, 5);
  specialize CheckValue<TLongBoolByte>(bb32, exp, 6);
{$ifdef CPU64}
  specialize CheckValue<TQWordBoolByte>(bb64, exp, 7);
{$endif}
  Writeln('ok');
end.
