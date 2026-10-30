{ %skipcpu=jvm }
{ tcpstrconcat5's point in the other configuration that prefers unicodestring: a string
  constant with a string type of its own takes part in choosing the common type as that
  type, and a literal without one still prefers unicodestring. }
{$mode objfpc}{$H+}{$modeswitch unicodestrings}
program tcpstrconcat6;

var
  gFailed: Integer = 0;
  gLastOverload: string;

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

function BytesAre(aText: PAnsiChar; const aWant: array of Byte): Boolean;
var
  lIdx: Integer;
begin
  Result := True;
  for lIdx := 0 to High(aWant) do
    if Ord(aText[lIdx]) <> aWant[lIdx] then
      Result := False;
end;

var
  lAnsi: AnsiString;
  lWide: UnicodeString;
  lBad, lResult: Utf8String;
begin
  lAnsi := 'xy';
  lWide := 'xy';
  SetLength(lBad, 2);
  lBad[1] := AnsiChar($C0);
  lBad[2] := AnsiChar($80);

  Over(AnsiString('pp') + lAnsi);
  Check(1, gLastOverload = 'ansi');
  Over(lAnsi + AnsiString('qq'));
  Check(2, gLastOverload = 'ansi');
  Over(ShortString('pp') + lAnsi);
  Check(3, gLastOverload = 'ansi');
  Check(4, BytesAre(PAnsiChar(AnsiString('pp') + lAnsi), [$70, $70, $78, $79]));
  lResult := Utf8String('pp') + lBad;
  Check(5, BytesAre(PAnsiChar(lResult), [$70, $70, $C0, $80]));

  { a unicodestring operand, and a literal without a cast on either side }
  Over(AnsiString('pp') + lWide);
  Check(6, gLastOverload = 'unicode');
  Over(lAnsi + 'qq');
  Check(7, gLastOverload = 'unicode');
  Over('pp' + lAnsi);
  Check(8, gLastOverload = 'unicode');

  if gFailed <> 0 then
    Halt(1);
  WriteLn('ok');
end.
