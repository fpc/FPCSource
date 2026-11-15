{ Under codepage utf8, and with a unicodestring result: an ansistring constant
  with a code page of its own is converted from that code page when a folded
  expression makes it unicode. }
program tcpstrconcat11;

{$mode delphi}
{$codepage utf8}

type
  T1252String = type AnsiString(1252);

var
  gErrors: LongInt = 0;

procedure CheckA(const aWhat: AnsiString; const aGot: RawByteString; aWantPage: Word; const aWant: array of Byte);
var
  i: LongInt;
  lSame: Boolean;
begin
  lSame := (Length(aGot) = Length(aWant)) and (StringCodePage(aGot) = aWantPage);
  if lSame then
    for i := 0 to High(aWant) do
      if Ord(aGot[i + 1]) <> aWant[i] then
        lSame := False;
  if not lSame then
    begin
      Write(aWhat, ': code page ', StringCodePage(aGot), ', got');
      for i := 1 to Length(aGot) do
        Write(' ', HexStr(Ord(aGot[i]), 2));
      Writeln;
      Inc(gErrors);
    end;
end;

procedure CheckU(const aWhat: AnsiString; const aGot: UnicodeString; const aWant: array of Word);
var
  i: LongInt;
  lSame: Boolean;
begin
  lSame := Length(aGot) = Length(aWant);
  if lSame then
    for i := 0 to High(aWant) do
      if Ord(aGot[i + 1]) <> aWant[i] then
        lSame := False;
  if not lSame then
    begin
      Write(aWhat, ': got');
      for i := 1 to Length(aGot) do
        Write(' ', HexStr(Ord(aGot[i]), 4));
      Writeln;
      Inc(gErrors);
    end;
end;

var
  l1252: T1252String;
  lU8: Utf8String;
begin
  l1252 := T1252String('éé') + 'èè';
  CheckA('T1252String(''éé'') + ''èè''', l1252, 1252, [$E9, $E9, $E8, $E8]);
  lU8 := T1252String('éé') + 'èè';
  CheckA('T1252String(''éé'') + ''èè'' into UTF-8', lU8, CP_UTF8, [$C3, $A9, $C3, $A9, $C3, $A8, $C3, $A8]);
  l1252 := 'èè' + T1252String('éé');
  CheckA('''èè'' + T1252String(''éé'')', l1252, 1252, [$E8, $E8, $E9, $E9]);
  CheckU('UnicodeString(Utf8String(''è!''))', UnicodeString(Utf8String('è!')), [$00E8, $0021]);
  CheckU('UnicodeString(T1252String(''é!''))', UnicodeString(T1252String('é!')), [$00E9, $0021]);
  CheckU('UnicodeString(''é'') + Utf8String(''è!'')', UnicodeString('é') + Utf8String('è!'), [$00E9, $00E8, $0021]);
  CheckU('Utf8String(''è'') + ''é''', UnicodeString(Utf8String('è') + 'é'), [$00E8, $00E9]);
  { controls }
  CheckU('a unicode literal', 'éè', [$00E9, $00E8]);
  l1252 := T1252String('éé');
  CheckA('one cp1252 constant', l1252, 1252, [$E9, $E9]);
  if gErrors <> 0 then
    Halt(1);
  Writeln('ok');
end.
