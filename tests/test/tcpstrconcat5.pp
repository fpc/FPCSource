{ %skipcpu=jvm }
{ A string constant with a string type of its own, which a cast gives it, takes
  part in choosing the common type of a concatenation or comparison as that
  type. In a mode whose default string is unicodestring it counted as a literal
  without one, which prefers unicodestring: a pansichar taken of the result
  pointed at UTF-16 bytes, and bytes that are not valid UTF-8 were replaced. }
{$mode delphiunicode}
program tcpstrconcat5;

uses
  sysutils;

const
  cNamed = AnsiString('kk');

var
  gFailed: Integer = 0;
  gLastOverload: string;
  gAllocs: Integer = 0;
  gManager: TMemoryManager;

function CountingGetMem(aSize: PtrUInt): Pointer;
begin
  Inc(gAllocs);
  Result := gManager.GetMem(aSize);
end;

procedure Over(const aValue: AnsiString); overload;
begin
  gLastOverload := 'ansi';
end;

procedure Over(const aValue: UnicodeString); overload;
begin
  gLastOverload := 'unicode';
end;

procedure Check(aRow: Integer; aHolds: Boolean);
begin
  if not aHolds then begin
    WriteLn('row ', aRow, ' failed');
    Inc(gFailed);
  end;
end;

function BytesOf(aText: PAnsiChar; aCount: Integer): AnsiString;
var
  lIdx: Integer;
begin
  SetLength(Result, aCount);
  for lIdx := 1 to aCount do
    Result[lIdx] := aText[lIdx - 1];
end;

function Bytes8(const aBytes: array of Byte): Utf8String;
var
  lIdx: Integer;
begin
  SetLength(Result, Length(aBytes));
  for lIdx := 0 to High(aBytes) do
    Result[lIdx + 1] := AnsiChar(aBytes[lIdx]);
end;

function SameBytes(const aLeft, aRight: Utf8String): Boolean;
begin
  Result := (Length(aLeft) = Length(aRight)) and
    CompareMem(Pointer(aLeft), Pointer(aRight), Length(aLeft));
end;

var
  lAnsi: AnsiString;
  lShort: ShortString;
  lWide: UnicodeString;
  lChars: PAnsiChar;
  lBad, lOther, lResult: Utf8String;
  lManager: TMemoryManager;
  lHolds: Boolean;
  lAllocs: Integer;
begin
  lAnsi := 'xy';
  lShort := 'xy';
  lWide := 'xy';
  lChars := 'ef';
  lBad := Bytes8([$C0, $80]);
  lOther := Bytes8([$C1, $81]);

  { a pansichar sees the bytes of the cast's string type, either side }
  Check(1, BytesOf(PAnsiChar(AnsiString('pp') + lAnsi), 4) = 'ppxy');
  Check(2, BytesOf(PAnsiChar(lAnsi + AnsiString('qq')), 4) = 'xyqq');
  Check(3, BytesOf(PAnsiChar(AnsiString('p') + lAnsi), 3) = 'pxy');
  Check(4, BytesOf(PAnsiChar(AnsiString('pp') + lAnsi + AnsiString('qq')), 6) = 'ppxyqq');
  Check(5, BytesOf(PAnsiChar(RawByteString('pp') + lAnsi), 4) = 'ppxy');
  Check(6, BytesOf(PAnsiChar(ShortString('pp') + lAnsi), 4) = 'ppxy');
  Check(7, BytesOf(PAnsiChar(cNamed + lAnsi), 4) = 'kkxy');
  Check(8, BytesOf(PAnsiChar(AnsiString('pp') + lChars), 4) = 'ppef');

  { bytes that are not valid UTF-8 survive }
  lResult := Utf8String('pp') + lBad;
  Check(9, SameBytes(lResult, Bytes8([$70, $70, $C0, $80])));
  lResult := lBad + Utf8String('qq');
  Check(10, SameBytes(lResult, Bytes8([$C0, $80, $71, $71])));
  lResult := Utf8String('p') + lBad + Utf8String('q');
  Check(11, SameBytes(lResult, Bytes8([$70, $C0, $80, $71])));

  { two different invalid sequences do not become equal on the way }
  Check(12, (lBad + Utf8String('pp')) <> (lOther + Utf8String('pp')));

  { the overload follows the type }
  Over(AnsiString('pp') + lAnsi);
  Check(13, gLastOverload = 'ansi');
  Over(lAnsi + AnsiString('qq'));
  Check(14, gLastOverload = 'ansi');

  { two cast literals are folded into one of that type }
  Check(15, BytesOf(PAnsiChar(AnsiString('pp') + AnsiString('qq')), 4) = 'ppqq');
  Over(AnsiString('pp') + AnsiString('qq'));
  Check(16, gLastOverload = 'ansi');

  { a unicodestring operand still makes it a unicodestring }
  Over(AnsiString('pp') + lWide);
  Check(17, gLastOverload = 'unicode');
  Over(lAnsi + lWide);
  Check(18, gLastOverload = 'unicode');
  { the cast is what counts here too; Delphi treats this one as an ansistring }
  Over(UnicodeString('pp') + lAnsi);
  Check(19, gLastOverload = 'unicode');

  { no literal at all }
  Over(lAnsi + lAnsi);
  Check(20, gLastOverload = 'ansi');
  { a literal without a cast keeps the mode's preferred string type }
  Over(lAnsi + 'qq');
  Check(21, gLastOverload = 'unicode');
  Over('pp' + lAnsi);
  Check(22, gLastOverload = 'unicode');

  { two characters folded have a string type too, without a cast }
  Over(('p' + 'q') + lAnsi);
  Check(23, gLastOverload = 'ansi');
  Over(lAnsi + ('p' + 'q'));
  Check(24, gLastOverload = 'ansi');

  { the other side of an ansistring or a pansichar }
  Check(27, BytesOf(PAnsiChar(lAnsi + RawByteString('qq')), 4) = 'xyqq');
  Check(28, BytesOf(PAnsiChar(lAnsi + ShortString('qq')), 4) = 'xyqq');
  Check(29, BytesOf(PAnsiChar(lAnsi + cNamed), 4) = 'xykk');
  Check(30, BytesOf(PAnsiChar(lChars + AnsiString('qq')), 4) = 'efqq');

  { a shortstring cast beside a shortstring gives an ansistring, as in Delphi 13 }
  Check(31, BytesOf(PAnsiChar(ShortString('pp') + lShort), 4) = 'ppxy');
  Check(32, BytesOf(PAnsiChar(lShort + ShortString('qq')), 4) = 'xyqq');
  Over(ShortString('pp') + lShort);
  Check(33, gLastOverload = 'ansi');

  { a comparison with a cast literal compares in that type too. Its result does not show
    it, as the operands are plain text, so this counts the heap blocks a conversion to a
    unicodestring takes; a widestring's come from the system and would not show here }
  GetMemoryManager(gManager);
  lManager := gManager;
  lManager.GetMem := CountingGetMem;
  SetMemoryManager(lManager);
  gAllocs := 0;
  lHolds := lAnsi = AnsiString('xy');
  lHolds := lHolds and (AnsiString('xy') = lAnsi);
  lHolds := lHolds and not (lAnsi <> AnsiString('xy'));
  lHolds := lHolds and not (AnsiString('xy') <> lAnsi);
  lHolds := lHolds and (lAnsi < AnsiString('xz'));
  lHolds := lHolds and (AnsiString('xa') < lAnsi);
  lHolds := lHolds and (lAnsi <= AnsiString('xy'));
  lHolds := lHolds and (AnsiString('xy') <= lAnsi);
  lHolds := lHolds and (lAnsi > AnsiString('xa'));
  lHolds := lHolds and (AnsiString('xz') > lAnsi);
  lHolds := lHolds and (lAnsi >= AnsiString('xy'));
  lHolds := lHolds and (AnsiString('xy') >= lAnsi);
  lAllocs := gAllocs;
  SetMemoryManager(gManager);
  Check(25, lHolds);
  Check(26, lAllocs = 0);

  if gFailed <> 0 then
    Halt(1);
  WriteLn('ok');
end.
