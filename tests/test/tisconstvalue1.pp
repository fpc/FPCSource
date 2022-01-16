program tisconstvalue1;

{$mode objfpc}
{$modeswitch advancedrecords}

type
  TTestLongInt = record
    a: LongInt;
  end;

  TTestAnsiString = record
    a: AnsiString;
  end;
{
  TTestManaged = record
    a: LongInt;
    class operator Initialize(var aTestManaged: TTestManaged);
  end;

  class operator TTestManaged.Initialize(var aTestManaged: TTestManaged);
  begin
    aTestManaged.a := 42;
  end;
}
type
  TDynArrayLongInt = array of LongInt;
  TStaticArrayAnsiString = array[0..4] of AnsiString;

  TEnum = (eOne, eTwo, eThree);
  TSet = set of (sOne, sTwo, sThree);

const
  // untyped
  Number = 100;
  Str = 'Hello World!';
  Dbl = 1.1;
  NilPtr = nil;
  IsConst = True;
  GUID = '{10101010-1010-0101-1001-110110110110}';
  // typed
  IntConst: Integer = 13;
  RealConst: Real = 12;
  Alphabet: array [1..26] of char =
       ('A','B','C','D','E','F','G','H','I',
        'J','K','L','M','N','O','P','Q','R',
        'S','T','U','V','W','X','Y','Z');
  MyGUID: TGUID = '{10101010-1010-0101-1001-110110110110}';
  Bool: Boolean = False;

var
  l: LongInt;
  o: TObject;
  _as: AnsiString;
  lir: TTestLongInt;
  asr: TTestAnsiString;
  //mr: TTestManaged;
  liarr: TDynArrayLongInt;
  sasarr: TStaticArrayAnsiString;

begin
  l := 1;
  if IsConstValue(l) then
    Halt(1);

  o := TObject.Create;
  try
    if IsConstValue(o) then
      Halt(2);
  finally
    o.Free;
  end;

  _as := 'Hello World!';
  if IsConstValue(_as) then
    Halt(3);

  if not IsConstValue(eOne) then
    Halt(4);
  if not IsConstValue(eTwo) then
    Halt(5);
  if not IsConstValue(eThree) then
    Halt(6);

  if not IsConstValue(Number) then
    Halt(7);
  if not IsConstValue(Str) then
    Halt(8);

  lir.a := 5;
  if IsConstValue(lir) then
    Halt(9);

  asr.a := 'Hello World!';
  if IsConstValue(asr) then
    Halt(10);
{
  if IsConstValue(mr) then
    Halt(11);
}
  SetLength(liarr, 2);
  liarr[0] := 1;
  liarr[1] := 2;
  if IsConstValue(liarr) then
    Halt(12);

  sasarr[0] := 'Hell';
  sasarr[1] := 'o ';
  sasarr[2] := 'Wor';
  sasarr[3] := 'ld!';
  if IsConstValue(sasarr) then
    Halt(13);

  if not IsConstValue(sOne) then
    Halt(14);
  if not IsConstValue(sTwo) then
    Halt(15);
  if not IsConstValue(sThree) then
    Halt(16);

  if not IsConstValue(Dbl) then
    Halt(17);

  if not IsConstValue(NilPtr) then
    Halt(18);

  if not IsConstValue(IsConst) then
    Halt(19);

  if not IsConstValue(GUID) then
    Halt(20);

  if IsConstValue(IntConst) then
    Halt(21);

  if IsConstValue(RealConst) then
    Halt(22);

  if IsConstValue(Alphabet) then
    Halt(23);

  if IsConstValue(MyGUID) then
    Halt(24);

  if IsConstValue(Bool) then
    Halt(25);

  Writeln('Ok');
end.
