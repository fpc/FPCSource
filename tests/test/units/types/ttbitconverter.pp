program ttbitconverter;

{$mode Delphi}

uses
  SysUtils, Types;

type
  TMyRecord = record
    Number: Integer;
    SmallNumber: Word;
    UseIt: Boolean;
  end;

  TMyPackedRecord = packed record
    Number: Integer;
    SmallNumber: Word;
    UseIt: Boolean;
  end;

procedure TestFromInteger;
var
  arr: TBytes;
  item: Byte;
  i: Integer;
begin
  i := 0;
  SetLength(arr, SizeOf(Integer));
  TBitConverter.From<Integer>(NtoLE(Integer(1038)), arr);
  for item in arr do
  begin
    Inc(i);
    writeln(i.ToString + ': ' + item.ToString);
  end;

  if arr[0] <> 14 then halt(1);
  if arr[1] <> 4 then halt(1);
  if arr[2] <> 0 then halt(1);
  if arr[3] <> 0 then halt(1);

  writeln('');
end;

procedure TestFromDouble;
var
  arr: TBytes;
  item: Byte;
  i: Integer;
begin
  i := 0;
  SetLength(arr, SizeOf(Double));
  TBitConverter.From<Double>(Double(3.14), arr);
  for item in arr do
  begin
    Inc(i);
    writeln(i.ToString + ': ' + item.ToString);
  end;

  if PDouble(@arr[0])^ <> Double(3.14) then halt(2);

  writeln('');
end;

procedure TestFromTMyRecord;
var
  arr: TBytes;
  item: Byte;
  rec: TMyRecord;
  i: Integer;
begin
  i := 0;
  SetLength(arr, SizeOf(TMyRecord));

  rec := Default(TMyRecord);
  rec.Number := NToLE(LongInt(42));
  rec.SmallNumber := NToLE(Word(5));
  rec.UseIt := True;
  TBitConverter.From<TMyRecord>(rec, arr);

  for item in arr do
  begin
    Inc(i);
    writeln(i.ToString + ': ' + item.ToString);
  end;

  if arr[0] <> 42 then halt(3);
  if arr[1] <> 0 then halt(3);
  if arr[2] <> 0 then halt(3);
  if arr[3] <> 0 then halt(3);
  if arr[4] <> 5 then halt(3);
  if arr[5] <> 0 then halt(3);
  if arr[6] <> 1 then halt(3);
  if arr[7] <> 0 then halt(3);

  writeln('');
end;

procedure TestFromTMyPackedRecord;
var
  arr: TBytes;
  item: Byte;
  rec: TMyPackedRecord;
  i: Integer;
begin
  i := 0;
  SetLength(arr, SizeOf(TMyPackedRecord));

  rec := Default(TMyPackedRecord);
  rec.Number := NToLe(Integer(42));
  rec.SmallNumber := NToLe(Word(289));
  rec.UseIt := True;
  TBitConverter.From<TMyPackedRecord>(rec, arr);

  for item in arr do
  begin
    Inc(i);
    writeln(i.ToString + ': ' + item.ToString);
  end;

  if arr[0] <> 42 then halt(4);
  if arr[1] <> 0 then halt(4);
  if arr[2] <> 0 then halt(4);
  if arr[3] <> 0 then halt(4);
  if arr[4] <> 33 then halt(4);
  if arr[5] <> 1 then halt(4);
  if arr[6] <> 1 then halt(4);

  writeln('');
end;

procedure TestFromAnsiChar;
var
  arr: TBytes;
  item: Byte;
  c: AnsiChar;
  i: Integer;
begin
  i := 0;
  c := 'A';
  SetLength(arr, SizeOf(c));
  TBitConverter.From<AnsiChar>(c, arr);

  for item in arr do
  begin
    Inc(i);
    writeln(i.ToString + ': ' + item.ToString);
  end;

  if arr[0] <> 65 then halt(5);

  writeln('');
end;

procedure TestFromUnicodeChar;
var
  arr: TBytes;
  item: Byte;
  c: UnicodeChar;
  i: Integer;
begin
  i := 0;
  c := 'A';
  SetLength(arr, SizeOf(c));
  TBitConverter.From<UnicodeChar>(UnicodeChar(NToLE(Ord(c))), arr);

  for item in arr do
  begin
    Inc(i);
    writeln(i.ToString + ': ' + item.ToString);
  end;

  if arr[0] <> 65 then halt(6);
  if arr[1] <> 0 then halt(6);

  writeln('');
end;

procedure TestInToInteger;
var
  arr: TBytes;
  num: Integer;
begin
  arr := TArray<Byte>.Create(1, 0, 1, 0);

  num := TBitConverter.InTo<Integer>(arr, 0);
  num := LEToN(num);
  writeln(num.ToString);

  if num <> 65537 then halt(7);

  writeln('');
end;

procedure TestInToDouble;
var
  arr: TBytes;
  num: Double;
  tmp: Int64;
begin
  arr := TArray<Byte>.Create($1C, $C7, $71, $1C, $C7, $71, $BC, $3F);

  num := TBitConverter.InTo<Double>(arr, 0);
  tmp := LEToN(PInt64(@num)^);
  num := PDouble(@tmp)^;
  writeln(num.ToString);

  if num <> Double(0.1111111111111111111) then halt(8);

  writeln('');
end;

procedure TestInToTMyRecord;
var
  arr: TBytes;
  rec: TMyRecord;
begin
  arr := TArray<Byte>.Create(66, 0, 0, 0, 15, 0, 0, 0);

  rec := TBitConverter.InTo<TMyRecord>(arr, 0);
  rec.Number := LEToN(rec.Number);
  rec.SmallNumber := LEToN(rec.SmallNumber);
  writeln(rec.Number.ToString);
  writeln(rec.SmallNumber.ToString);
  writeln(BoolToStr(rec.UseIt, True));

  if rec.Number <> 66 then halt(9);
  if rec.SmallNumber <> 15 then halt(9);
  if rec.UseIt <> False then halt(9);

  writeln('');
end;

procedure TestInToTMyPackedRecord;
var
  arr: TBytes;
  rec: TMyPackedRecord;
begin
  arr := TArray<Byte>.Create(255, 1, 0, 0, 15, 0, 1);

  rec := TBitConverter.InTo<TMyPackedRecord>(arr, 0);
  rec.Number := LEToN(rec.Number);
  rec.SmallNumber := LEToN(rec.SmallNumber);
  writeln(rec.Number.ToString);
  writeln(rec.SmallNumber.ToString);
  writeln(BoolToStr(rec.UseIt, True));

  if rec.Number <> 511 then halt(10);
  if rec.SmallNumber <> 15 then halt(10);
  if rec.UseIt <> True then halt(10);

  writeln('');
end;

procedure TestInToAnsiChar;
var
  arr: TBytes;
  c: AnsiChar;
begin
  arr := TArray<Byte>.Create(65);

  c := TBitConverter.InTo<AnsiChar>(arr, 0);
  writeln(c);

  if c <> 'A' then halt(11);

  writeln('');
end;

procedure TestInToUnicodeChar;
var
  arr: TBytes;
  c: UnicodeChar;
begin
  arr := TArray<Byte>.Create(66, 0);

  c := TBitConverter.InTo<UnicodeChar>(arr, 0);
  c := UnicodeChar(LEToN(Ord(c)));
  writeln(c);

  if c <> 'B' then halt(12);

  writeln('');
end;

procedure TestFromIntegerOffset;
var
  arr: TBytes;
  item: Byte;
  i: Integer;
begin
  i := 0;
  SetLength(arr, SizeOf(Integer) + 2);
  TBitConverter.From<Integer>(NToLE(Integer(257)), arr, 2);
  for item in arr do
  begin
    Inc(i);
    writeln(i.ToString + ': ' + item.ToString);
  end;

  if arr[0] <> 0 then halt(13);
  if arr[1] <> 0 then halt(13);
  if arr[2] <> 1 then halt(13);
  if arr[3] <> 1 then halt(13);
  if arr[4] <> 0 then halt(13);
  if arr[5] <> 0 then halt(13);

  writeln('');
end;

procedure TestInToIntegerOffset;
var
  arr: TBytes;
  num: Integer;
begin
  arr := TArray<Byte>.Create(2, 0, 2, 0, 1, 0, 2, 0);

  num := TBitConverter.InTo<Integer>(arr, 2);
  num := LEToN(num);
  writeln(num.ToString);

  if num <> 65538 then halt(14);

  writeln('');
end;
{
procedure TestInToAnsiString;
var
  arr: TBytes;
  str: AnsiString;
begin
  arr := TArray<Byte>.Create(44, 44, 45, 67, 66);
  
  str := TBitConverter.InTo<AnsiString>(arr, 0);
  writeln(str);

  if str <> 'hello' then halt(15);

  writeln('');
end;

procedure TestFromAnsiString;
var
  arr: TBytes;
  item: Byte;
  str: AnsiString;
  i: Integer;
begin
  i := 0;
  str := 'hello world!';
  SetLength(arr, Length(str));
  TBitConverter.From<AnsiString>(str, arr);

  for item in arr do
  begin
    Inc(i);
    writeln(i.ToString + ': ' + item.ToString);
  end;

  writeln('');
end;
}
begin
  {* testing TBitConverter.From<T> *}
  TestFromInteger;
  TestFromDouble;
  TestFromTMyRecord;
  TestFromTMyPackedRecord;
  TestFromAnsiChar;
  TestFromUnicodeChar;

  {* testing TBitConverter.InTo<T> *}
  TestInToInteger;
  TestInToDouble;
  TestInToTMyRecord;
  TestInToTMyPackedRecord;
  TestInToAnsiChar;
  TestInToUnicodeChar;

  {* testing offset *}
  TestFromIntegerOffset;
  TestInToIntegerOffset;

  {* non base types *}
  //TestInToAnsiString;
  //TestFromAnsiString;

  writeln('ok');
  //readln;
end.
