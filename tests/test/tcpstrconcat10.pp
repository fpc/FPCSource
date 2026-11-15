{ A concatenation of string constants folded at compile time, one of them an
  ansistring constant with a code page of its own: its bytes are not converted
  a second time, and an operand of another code page is converted once. }
program tcpstrconcat10;

{$mode delphi}
{$codepage cp1252}

type
  T1252String = type AnsiString(1252);

const
  cInferred = Utf8String(#$FEFF) + 'a';
  cTyped: Utf8String = Utf8String(#$FEFF) + 'a';

var
  gErrors: LongInt = 0;

procedure Check(const aWhat: AnsiString; const aGot: RawByteString; aWantPage: Word; const aWant: array of Byte);
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

var
  lU, lR: Utf8String;
  l1252: T1252String;
begin
  Check('Utf8String(#$FEFF) + ''a''', Utf8String(#$FEFF) + 'a', CP_UTF8, [$EF, $BB, $BF, $61]);
  Check('Utf8String(#$00E9) + ''a''', Utf8String(#$00E9) + 'a', CP_UTF8, [$C3, $A9, $61]);
  Check('''a'' + Utf8String(#$00E9)', 'a' + Utf8String(#$00E9), CP_UTF8, [$61, $C3, $A9]);
  Check('Utf8String(#$00E9) + Utf8String(#$FEFF)', Utf8String(#$00E9) + Utf8String(#$FEFF), CP_UTF8, [$C3, $A9, $EF, $BB, $BF]);
  Check('''x'' + Utf8String(#$00E9) + ''y''', 'x' + Utf8String(#$00E9) + 'y', CP_UTF8, [$78, $C3, $A9, $79]);
  Check('a constant of inferred type', cInferred, CP_UTF8, [$EF, $BB, $BF, $61]);
  Check('a typed constant', cTyped, CP_UTF8, [$EF, $BB, $BF, $61]);
  lR := Utf8String(#$FEFF) + 'a';
  Check('assigned to a variable', lR, CP_UTF8, [$EF, $BB, $BF, $61]);
  { a zero inside }
  Check('with a zero', Utf8String(#$FEFF#0'a') + 'b', CP_UTF8, [$EF, $BB, $BF, $00, $61, $62]);
  { operands that have to be converted: each of another code page than the
    result, and of another length there }
  lU := T1252String(#$00E9'!') + T1252String(#$00E8'?');
  Check('cp1252 + cp1252 into UTF-8', lU, CP_UTF8, [$C3, $A9, $21, $C3, $A8, $3F]);
  lU := Utf8String(#$FEFF) + T1252String(#$00E9'!');
  Check('UTF-8 + cp1252 into UTF-8', lU, CP_UTF8, [$EF, $BB, $BF, $C3, $A9, $21]);
  lU := T1252String(#$00E9'!') + Utf8String(#$00E8'?');
  Check('cp1252 + UTF-8 into UTF-8', lU, CP_UTF8, [$C3, $A9, $21, $C3, $A8, $3F]);
  l1252 := T1252String(#$00E9'!') + Utf8String(#$00E8'?');
  Check('cp1252 + UTF-8 into cp1252', l1252, 1252, [$E9, $21, $E8, $3F]);
  l1252 := T1252String(#$00E9'!') + 'a';
  Check('cp1252 + literal', l1252, 1252, [$E9, $21, $61]);
  { controls: right before as well }
  Check('one constant', Utf8String(#$FEFF'a'), CP_UTF8, [$EF, $BB, $BF, $61]);
  lU := Utf8String(#$FEFF);
  lR := lU + 'a';
  Check('through a variable', lR, CP_UTF8, [$EF, $BB, $BF, $61]);
  Check('plain literals', Utf8String('a' + 'b'), CP_UTF8, [$61, $62]);
  if gErrors <> 0 then
    Halt(1);
  Writeln('ok');
end.
